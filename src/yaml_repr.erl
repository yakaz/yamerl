-module(yaml_repr).

-include("yaml_parser.hrl").
-include("yaml_tokens.hrl").
-include("yaml_repr.hrl").
-include("yaml_nodes.hrl").

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
        EOS ->
            (yaml_parser:get_token_fun(Parser))(get_docs);
        true ->
            Parser
    end.

next_chunk(Parser, More_Data) ->
    next_chunk(Parser, More_Data, false).

last_chunk(Parser, More_Data) ->
    next_chunk(Parser, More_Data, true).

%% -------------------------------------------------------------------
%% Public API: common stream sources.
%% -------------------------------------------------------------------

string(String) ->
    string(String, []).

string(String, Options) ->
    Parser_Options = initialize(Options),
    Parser = yaml_parser:string(String, Parser_Options),
    (yaml_parser:get_token_fun(Parser))(get_docs).

file(Filename) ->
    file(Filename, []).

file(Filename, Options) ->
    Parser_Options = initialize(Options),
    Parser = yaml_parser:file(Filename, Parser_Options),
    (yaml_parser:get_token_fun(Parser))(get_docs).

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

represent(Repr, #yaml_doc_start{}) ->
    %% Prepare a document node.
    Doc = #yaml_doc{},
    Repr1 = Repr#yaml_repr{
      current_doc = [Doc]
    },
    return_new_fun(Repr1);

represent(_, Token) when
  is_record(Token, yaml_stream_start) orelse
  is_record(Token, yaml_stream_end) orelse
  is_record(Token, yaml_yaml_directive) orelse
  is_record(Token, yaml_tag_directive) orelse
  is_record(Token, yaml_reserved_directive) orelse
  is_record(Token, yaml_doc_end) ->
    %% This token doesn't start a node: ignore it.
    ok;

represent(
  #yaml_repr{current_doc = Doc, current_node_is_leaf = false,
    mods = Mods, tags = Tags} = Repr,
  Token) when Doc /= undefined andalso
  (is_record(Token, yaml_collection_start) orelse
   is_record(Token, yaml_scalar)) ->
    %% This token starts a node. We must determine the module to use to
    %% represent this node.
    Tag = case Token of
        #yaml_collection_start{tag = T} -> T;
        #yaml_scalar{tag = T}           -> T
    end,
    Ret = case Tag of
        #yaml_tag{uri = {non_specific, _}} ->
            %% The node has a non-specific tag. We let each module
            %% decides if they want to represent the node.
            try_represent(Repr, Mods, Token);
        #yaml_tag{uri = URI} ->
            %% We look up this URI in the tag's index.
            case proplists:get_value(URI, Tags) of
                Mod when Mod /= undefined ->
                    Mod:represent_token(Repr, undefined, Token);
                undefined ->
                    %% This tag isn't handled by anything!
                    Text = lists:flatten(io_lib:format(
                        "Tag \"~s\" unrecognized by any module", [URI])),
                    Error = #yaml_parser_error{
                      name   = unrecognized_node,
                      token  = Tag,
                      text   = Text,
                      line   = ?TOKEN_LINE(Tag),
                      column = ?TOKEN_COLUMN(Tag)
                    },
                    yaml_parser:throw_error(Error)
            end
    end,
    handle_represent_return(Repr, Doc, Ret);

represent(
  #yaml_repr{current_doc =
    [#unfinished_node{module = Mod} = Node | Doc]} = Repr,
  Token) ->
    %% This token continues a node. We call the current node's module to
    %% handle it.
    Ret = Mod:represent_token(Repr, Node, Token),
    handle_represent_return(Repr, Doc, Ret).

try_represent(Repr, [Mod | Rest], Token) ->
    case Mod:try_represent_token(Repr, undefined, Token) of
        unrecognized -> try_represent(Repr, Rest, Token);
        Ret          -> Ret
    end;
try_represent(_, [], Token) ->
    Error = #yaml_parser_error{
      name   = unrecognized_node,
      token  = Token,
      text   = "No module found to handle node",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

represent_parent(#yaml_repr{docs = Docs, docs_count = Count} = Repr,
  [#yaml_doc{} = Doc], Root) ->
    %% This node is the root of the document.
    Doc1 = Doc#yaml_doc{
      root = Root
    },
    Repr1 = Repr#yaml_repr{
      docs                 = Docs ++ [Doc1],
      docs_count           = Count + 1,
      current_doc          = undefined,
      current_node_is_leaf = false,
      anchors              = dict:new()
    },
    return_new_fun(Repr1);
represent_parent(Repr, [#unfinished_node{module = Mod} = Node | Doc], Child) ->
    %% We call the parent node's module to handle this new child node.
    Ret = Mod:represent_node(Repr, Node, Child),
    handle_represent_return(Repr, Doc, Ret).

handle_represent_return(Repr, Doc, {finished, Node}) ->
    %% Give this node to the parent node.
    represent_parent(Repr, Doc, Node);
handle_represent_return(Repr, Doc, {unfinished, Node, Is_Leaf}) ->
    %% Unfinished node, wait for the next tokens.
    Repr1 = Repr#yaml_repr{
      current_doc          = [Node | Doc],
      current_node_is_leaf = Is_Leaf
    },
    return_new_fun(Repr1).

return_new_fun(#yaml_repr{simple_structs = Simple} = Repr) ->
    Fun = fun
        (get_docs) when Simple ->
            [Doc#yaml_doc.root || Doc <- Repr#yaml_repr.docs];
        (get_docs) ->
            Repr#yaml_repr.docs;
        (get_repr) ->
            Repr;
        (T) ->
            represent(Repr, T)
    end,
    {ok, Fun}.

%% -------------------------------------------------------------------
%% Node modules.
%% -------------------------------------------------------------------

setup_node_mods(Repr) ->
    Mods  = yaml_app:get_param(node_mods) ++ ?CORE_SCHEMA_MODS,
    Auto  = filter_autodetected_node_mods(Mods, []),
    Tags  = index_tags(Mods, []),
    Repr1 = Repr#yaml_repr{
      mods = Auto,
      tags = Tags
    },
    return_new_fun(Repr1).

filter_autodetected_node_mods([Mod | Rest], Auto) ->
    Auto1 = case erlang:function_exported(Mod, try_represent_token, 3) of
        true  -> [Mod | Auto];
        false -> Auto
    end,
    filter_autodetected_node_mods(Rest, Auto1);
filter_autodetected_node_mods([], Auto) ->
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
    check_options(Repr_Options),
    Repr = #yaml_repr{
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
    lists:partition(Fun, Options).

check_options([Option | Rest]) ->
    case is_option_valid(Option) of
        true  -> check_options(Rest);
        false -> invalid_option(Option)
    end;
check_options([]) ->
    ok.

is_option_valid({simple_structs, Flag}) when is_boolean(Flag) ->
    true;
is_option_valid(_) ->
    false.

invalid_option(Option) ->
    Error = #yaml_parser_error{
      name  = invalid_repr_option,
      extra = [{option, Option}]
    },
    Error1 = case Option of
        {simple_structs, _} ->
            Error#yaml_parser_error{
              text = "Invalid value for option \"simple_structs\": "
              "it must be a boolean"
            };
        _ ->
            Error#yaml_parser_error{
              text = lists:flatten(io_lib:format("Unknown option \"~w\"",
                  [Option]))
            }
    end,
    yaml_parser:throw_error(Error1).
