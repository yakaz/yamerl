-module(yaml_constr).

-include("yaml_errors.hrl").
-include("yaml_tokens.hrl").
-include("yaml_nodes.hrl").
-include("internal/yaml_constr.hrl").

%% Public API.
-export([
    new/1,
    new/2,
    string/1,
    string/2,
    file/1,
    file/2,
    next_chunk/3,
    next_chunk/2,
    last_chunk/2,
    get_pres_details/1,
    node_line/1,
    node_column/1
  ]).

%% -------------------------------------------------------------------
%% Public API: chunked stream scanning.
%% -------------------------------------------------------------------

new(Source) ->
    new(Source, []).

new(Source, Options) ->
    Parser_Options = initialize(Options),
    yaml_parser:new(Source, Parser_Options).

next_chunk(Parser, More_Data, EOS) ->
    Parser = yaml_parser:next_chunk(Parser, More_Data, EOS),
    if
        EOS  -> get_docs(Parser);
        true -> Parser
    end.

next_chunk(Parser, More_Data) ->
    next_chunk(Parser, More_Data, false).

last_chunk(Parser, More_Data) ->
    next_chunk(Parser, More_Data, true).

get_docs(Parser) ->
    case yaml_parser:get_token_fun(Parser) of
        undefined ->
            Error = #yaml_parsing_error{
              name = token_fun_cleared
            },
            yaml_errors:throw(Error);
        Token_Fun ->
            Token_Fun(get_docs)
    end.

%% -------------------------------------------------------------------
%% Public API: common stream sources.
%% -------------------------------------------------------------------

string(String) ->
    string(String, []).

string(String, Options) ->
    Parser_Options = initialize(Options),
    Parser = yaml_parser:string(String, Parser_Options),
    get_docs(Parser).

file(Filename) ->
    file(Filename, []).

file(Filename, Options) ->
    Parser_Options = initialize(Options),
    Parser = yaml_parser:file(Filename, Parser_Options),
    get_docs(Parser).

%% -------------------------------------------------------------------
%% Presentation details.
%% -------------------------------------------------------------------

get_pres_details(Token) ->
    Line   = ?TOKEN_LINE(Token),
    Column = ?TOKEN_COLUMN(Token),
    [{line, Line}, {column, Column}].

%% -------------------------------------------------------------------
%% Node informations.
%% -------------------------------------------------------------------

node_line(Node) ->
    case node_pres(Node) of
        undefined -> undefined;
        Pres      -> proplists:get_value(line, Pres)
    end.

node_column(Node) ->
    case node_pres(Node) of
        undefined -> undefined;
        Pres      -> proplists:get_value(column, Pres)
    end.

node_pres(Node) when
  is_record(Node, yaml_seq) orelse
  is_record(Node, yaml_map) orelse
  is_record(Node, yaml_str) orelse
  is_record(Node, yaml_null) orelse
  is_record(Node, yaml_bool) orelse
  is_record(Node, yaml_int) orelse
  is_record(Node, yaml_timestamp) orelse
  is_record(Node, yaml_erlang_atom) orelse
  is_record(Node, yaml_erlang_fun) ->
    ?NODE_PRES(Node);
node_pres(Node) when is_tuple(Node) ->
    %% For user-defined nodes, we call the module responsible for it.
    Mod = ?NODE_MOD(Node),
    try
        Mod:node_pres(Node)
    catch
        error:undef ->
            undefined
    end.

%% -------------------------------------------------------------------
%% Representation.
%% -------------------------------------------------------------------

construct(Repr, #yaml_doc_start{}) ->
    %% Prepare a document node.
    Doc = #yaml_doc{},
    Repr1 = Repr#yaml_constr{
      current_doc = [Doc]
    },
    return_new_fun(Repr1);

construct(_, Token) when
  is_record(Token, yaml_stream_start) orelse
  is_record(Token, yaml_stream_end) orelse
  is_record(Token, yaml_yaml_directive) orelse
  is_record(Token, yaml_tag_directive) orelse
  is_record(Token, yaml_reserved_directive) orelse
  is_record(Token, yaml_doc_end) ->
    %% This token doesn't start a node: ignore it.
    ok;

construct(
  #yaml_constr{current_doc = Doc, current_node_is_leaf = false,
    mods = Mods, tags = Tags} = Repr,
  Token) when Doc /= undefined andalso
  (is_record(Token, yaml_collection_start) orelse
   is_record(Token, yaml_scalar)) ->
    %% This token starts a node. We must determine the module to use to
    %% construct this node.
    Tag = case Token of
        #yaml_collection_start{tag = T} -> T;
        #yaml_scalar{tag = T}           -> T
    end,
    Ret = case Tag of
        #yaml_tag{uri = {non_specific, _}} ->
            %% The node has a non-specific tag. We let each module
            %% decides if they want to construct the node.
            try_construct(Repr, Mods, Token);
        #yaml_tag{uri = URI} ->
            %% We look up this URI in the tag's index.
            case proplists:get_value(URI, Tags) of
                Mod when Mod /= undefined ->
                    Mod:construct_token(Repr, undefined, Token);
                undefined ->
                    %% This tag isn't handled by anything!
                    Error = #yaml_parsing_error{
                      name   = unrecognized_node,
                      token  = Tag,
                      line   = ?TOKEN_LINE(Tag),
                      column = ?TOKEN_COLUMN(Tag)
                    },
                    Error1 = yaml_errors:format(Error,
                      "Tag \"~s\" unrecognized by any module", [URI]),
                    yaml_errors:throw(Error1)
            end
    end,
    handle_construct_return(Repr, Doc, Ret);

construct(
  #yaml_constr{current_doc =
    [#unfinished_node{module = Mod} = Node | Doc]} = Repr,
  Token) ->
    %% This token continues a node. We call the current node's module to
    %% handle it.
    Ret = Mod:construct_token(Repr, Node, Token),
    handle_construct_return(Repr, Doc, Ret).

try_construct(Repr, [Mod | Rest], Token) ->
    case Mod:try_construct_token(Repr, undefined, Token) of
        unrecognized -> try_construct(Repr, Rest, Token);
        Ret          -> Ret
    end;
try_construct(_, [], Token) ->
    Error = #yaml_parsing_error{
      name   = unrecognized_node,
      token  = Token,
      text   = "No module found to handle node",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    yaml_errors:throw(Error).

construct_parent(#yaml_constr{docs = Docs, docs_count = Count} = Repr,
  [#yaml_doc{} = Doc], Root) ->
    %% This node is the root of the document.
    Doc1 = Doc#yaml_doc{
      root = Root
    },
    Repr1 = Repr#yaml_constr{
      docs                 = Docs ++ [Doc1],
      docs_count           = Count + 1,
      current_doc          = undefined,
      current_node_is_leaf = false,
      anchors              = dict:new()
    },
    return_new_fun(Repr1);
construct_parent(Repr, [#unfinished_node{module = Mod} = Node | Doc], Child) ->
    %% We call the parent node's module to handle this new child node.
    Ret = Mod:construct_node(Repr, Node, Child),
    handle_construct_return(Repr, Doc, Ret).

handle_construct_return(Repr, Doc, {finished, Node}) ->
    %% Give this node to the parent node.
    construct_parent(Repr, Doc, Node);
handle_construct_return(Repr, Doc, {unfinished, Node, Is_Leaf}) ->
    %% Unfinished node, wait for the next tokens.
    Repr1 = Repr#yaml_constr{
      current_doc          = [Node | Doc],
      current_node_is_leaf = Is_Leaf
    },
    return_new_fun(Repr1).

return_new_fun(#yaml_constr{simple_structs = Simple} = Repr) ->
    Fun = fun
        (get_docs) when Simple ->
            [Doc#yaml_doc.root || Doc <- Repr#yaml_constr.docs];
        (get_docs) ->
            Repr#yaml_constr.docs;
        (get_constr) ->
            Repr;
        (T) ->
            construct(Repr, T)
    end,
    {ok, Fun}.

%% -------------------------------------------------------------------
%% Node modules.
%% -------------------------------------------------------------------

setup_node_mods(Repr) ->
    Mods1 = umerge_unsorted(
      proplists:get_value(node_mods, Repr#yaml_constr.options, []),
      yaml_app:get_param(node_mods)
    ),
    Mods  = case proplists:get_value(schema, Repr#yaml_constr.options, core) of
        failsafe -> umerge_unsorted(Mods1, ?FAILSAFE_SCHEMA_MODS);
        json     -> umerge_unsorted(Mods1, ?JSON_SCHEMA_MODS);
        core     -> umerge_unsorted(Mods1, ?CORE_SCHEMA_MODS)
    end,
    Auto  = filter_autodetection_capable_mods(Mods, []),
    Tags  = index_tags(Mods, []),
    Repr1 = Repr#yaml_constr{
      mods = Auto,
      tags = Tags
    },
    return_new_fun(Repr1).

umerge_unsorted(List1, List2) ->
    Fun = fun(Mod, List) ->
        case lists:member(Mod, List) of
            true  -> List;
            false -> List ++ [Mod]
        end
    end,
    lists:foldl(Fun, List1, List2).

filter_autodetection_capable_mods([Mod | Rest], Auto) ->
    Auto1 = case erlang:function_exported(Mod, try_construct_token, 3) of
        true  -> [Mod | Auto];
        false -> Auto
    end,
    filter_autodetection_capable_mods(Rest, Auto1);
filter_autodetection_capable_mods([], Auto) ->
    lists:reverse(Auto).

index_tags([Mod | Rest], Tags) ->
    try
        Tags1 = index_tags2(Tags, Mod:tags(), Mod),
        index_tags(Rest, Tags1)
    catch
        _:_ ->
            index_tags(Rest, Tags)
    end;
index_tags([], Tags) ->
    Tags.

index_tags2(Tags, [Tag | Rest], Mod) ->
    Tags1 = case lists:keymember(Tag, 1, Tags) of
        false -> [{Tag, Mod} | Tags];
        true  -> Tags
    end,
    index_tags2(Tags1, Rest, Mod);
index_tags2(Tags, [], _) ->
    Tags.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

initialize(Options) ->
    {Repr_Options, Parser_Options} = filter_options(Options),
    Repr = #yaml_constr{
      options        = Repr_Options,
      simple_structs = proplists:get_value(simple_structs, Repr_Options, true)
    },
    {ok, Token_Fun} = setup_node_mods(Repr),
    [{token_fun, Token_Fun} | Parser_Options].

filter_options(Options) ->
    Parser_Options = yaml_parser:option_names(),
    Fun = fun
        ({Name, _}) -> not lists:member(Name, Parser_Options);
        (_)         -> true
    end,
    {Repr_Options, _} = Filtered_Opts = lists:partition(Fun, Options),
    check_options(Repr_Options),
    Filtered_Opts.

check_options([Option | Rest]) ->
    case is_option_valid(Option) of
        true  -> check_options(Rest);
        false -> invalid_option(Option)
    end;
check_options([]) ->
    ok.

is_option_valid({simple_structs, Flag}) when is_boolean(Flag) ->
    true;
is_option_valid({node_mods, Mods}) when is_list(Mods) ->
    Fun = fun(Mod) ->
        not yaml_app:is_node_mod(Mod)
    end,
    case lists:filter(Fun, Mods) of
        [] -> true;
        _  -> false
    end;
is_option_valid({schema, Schema})
  when Schema == failsafe orelse Schema == json orelse Schema == core ->
    true;
is_option_valid(_) ->
    false.

invalid_option(Option) ->
    Error = #yaml_invalid_option{
      option = Option
    },
    Error1 = case Option of
        {simple_structs, _} ->
            Error#yaml_invalid_option{
              text = "Invalid value for option \"simple_structs\": "
              "it must be a boolean"
            };
        {node_mods, _} ->
            Error#yaml_invalid_option{
              text = "Invalid value for option \"node_mods\": "
              "it must be a list of modules"
            };
        _ ->
            yaml_errors:format(Error, "Unknown option \"~w\"", [Option])
    end,
    yaml_errors:throw(Error1).
