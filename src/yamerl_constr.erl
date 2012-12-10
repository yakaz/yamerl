-module(yamerl_constr).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

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
    node_column/1,
    option_names/0
  ]).

%% -------------------------------------------------------------------
%% Exported types.
%% -------------------------------------------------------------------

%% FIXME:
%% This type should be "-opaque". However, up-to Erlang R15B03, an issue
%% with either this code or Dialyzer prevents us from declaring it
%% properly: Dialyzer reports warning regarding the stream_state_fun()
%% type and several guard expression which will never match.
-type yamerl_constr() :: #yamerl_constr{}.

-export_type([
    yamerl_constr/0,
    yamerl_constr_option/0,

    yamerl_node/0,
    yamerl_seq/0,
    yamerl_map/0,
    yamerl_str/0,
    yamerl_null/0,
    yamerl_bool/0,
    yamerl_int/0,
    yamerl_float/0,
    yamerl_timestamp/0,
    yamerl_erlang_atom/0,
    yamerl_erlang_fun/0,
    yamerl_user_node/0,
    yamerl_doc/0,

    yamerl_simple_node/0,
    yamerl_simple_seq/0,
    yamerl_simple_map/0,
    yamerl_simple_str/0,
    yamerl_simple_null/0,
    yamerl_simple_bool/0,
    yamerl_simple_int/0,
    yamerl_simple_float/0,
    yamerl_simple_timestamp/0,
    yamerl_simple_erlang_atom/0,
    yamerl_simple_erlang_fun/0,
    yamerl_user_simple_node/0,
    yamerl_simple_doc/0
  ]).

%% -------------------------------------------------------------------
%% Public API: chunked stream scanning.
%% -------------------------------------------------------------------

-spec new(Source) ->
        Parser | no_return() when
          Source :: term(),
          Parser :: yamerl_parser:yamerl_parser().

new(Source) ->
    new(Source, []).

-spec new(Source, Options) ->
        Parser | no_return() when
          Source  :: term(),
          Options :: [
            yamerl_constr_option() |
            yamerl_parser:yamerl_parser_option() |
            proplists:property()
          ],
          Parser  :: yamerl_parser:yamerl_parser().

new(Source, Options) ->
    Parser_Options = initialize(Options),
    yamerl_parser:new(Source, Parser_Options).

-spec next_chunk(Parser, Chunk) ->
        Ret | no_return() when
          Parser     :: yamerl_parser:yamerl_parser(),
          Chunk      :: unicode_binary(),
          Ret        :: {continue, New_Parser},
          New_Parser :: yamerl_parser:yamerl_parser().

next_chunk(Parser, Chunk) ->
    next_chunk(Parser, Chunk, false).

-spec next_chunk(Parser, Chunk, Last_Chunk) ->
        Ret | no_return() when
          Parser     :: yamerl_parser:yamerl_parser(),
          Chunk      :: unicode_binary(),
          Last_Chunk :: boolean(),
          Ret        :: {continue, New_Parser} | Result,
          New_Parser :: yamerl_parser:yamerl_parser(),
          Result     :: [yamerl_doc()]
                      | [yamerl_simple_doc()]
                      | term().

next_chunk(Parser, Chunk, EOS) ->
    Parser = yamerl_parser:next_chunk(Parser, Chunk, EOS),
    if
        EOS  -> get_docs(Parser);
        true -> Parser
    end.

-spec last_chunk(Parser, Chunk) ->
        Result | no_return() when
          Parser     :: yamerl_parser:yamerl_parser(),
          Chunk      :: unicode_binary(),
          Result     :: [yamerl_doc()]
                      | [yamerl_simple_doc()]
                      | term().

last_chunk(Parser, Chunk) ->
    next_chunk(Parser, Chunk, true).

get_docs(Parser) ->
    case yamerl_parser:get_token_fun(Parser) of
        Not_Fun when Not_Fun == acc orelse Not_Fun == drop ->
            Error = #yamerl_parsing_error{
              name = token_fun_cleared
            },
            yamerl_errors:throw(Error);
        Token_Fun ->
            Token_Fun(get_docs)
    end.

%% -------------------------------------------------------------------
%% Public API: common stream sources.
%% -------------------------------------------------------------------

-spec string(String) ->
        Result | no_return() when
          String :: unicode_data(),
          Result :: [yamerl_doc()]
                  | [yamerl_simple_doc()]
                  | term().

string(String) ->
    string(String, []).

-spec string(String, Options) ->
        Result | no_return() when
          String  :: unicode_data(),
          Options :: [ yamerl_parser:yamerl_parser_option()
                     | yamerl_constr_option()
                     | proplists:property()],
          Result  :: [yamerl_doc()]
                   | [yamerl_simple_doc()]
                   | term().

string(String, Options) ->
    Parser_Options = initialize(Options),
    Parser = yamerl_parser:string(String, Parser_Options),
    get_docs(Parser).

-spec file(Filename) ->
        Result | no_return() when
          Filename :: string(),
          Result   :: [yamerl_doc()]
                    | [yamerl_simple_doc()]
                    | term().

file(Filename) ->
    file(Filename, []).

-spec file(Filename, Options) ->
        Result | no_return() when
          Filename :: string(),
          Options  :: [ yamerl_parser:yamerl_parser_option()
                      | yamerl_constr_option()
                      | proplists:property()],
          Result   :: [yamerl_doc()]
                    | [yamerl_simple_doc()]
                    | term().

file(Filename, Options) ->
    Parser_Options = initialize(Options),
    Parser = yamerl_parser:file(Filename, Parser_Options),
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
  is_record(Node, yamerl_seq) orelse
  is_record(Node, yamerl_map) orelse
  is_record(Node, yamerl_str) orelse
  is_record(Node, yamerl_null) orelse
  is_record(Node, yamerl_bool) orelse
  is_record(Node, yamerl_int) orelse
  is_record(Node, yamerl_timestamp) orelse
  is_record(Node, yamerl_erlang_atom) orelse
  is_record(Node, yamerl_erlang_fun) ->
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
%% Construction.
%% -------------------------------------------------------------------

construct(Constr, #yamerl_doc_start{}) ->
    %% Prepare a document node.
    Doc = #yamerl_doc{},
    Constr1 = Constr#yamerl_constr{
      current_doc = [Doc]
    },
    return_new_fun(Constr1);

construct(_, Token) when
  is_record(Token, yamerl_stream_start) orelse
  is_record(Token, yamerl_stream_end) orelse
  is_record(Token, yamerl_yaml_directive) orelse
  is_record(Token, yamerl_tag_directive) orelse
  is_record(Token, yamerl_reserved_directive) orelse
  is_record(Token, yamerl_doc_end) ->
    %% This token doesn't start a node: ignore it.
    ok;

construct(
  #yamerl_constr{current_doc = Doc, current_node_is_leaf = false,
    mods = Mods, tags = Tags} = Constr,
  Token) when Doc /= undefined andalso
  (is_record(Token, yamerl_collection_start) orelse
   is_record(Token, yamerl_scalar)) ->
    %% This token starts a node. We must determine the module to use to
    %% construct this node.
    Tag = case Token of
        #yamerl_collection_start{tag = T} -> T;
        #yamerl_scalar{tag = T}           -> T
    end,
    Ret = case Tag of
        #yamerl_tag{uri = {non_specific, _}} ->
            %% The node has a non-specific tag. We let each module
            %% decides if they want to construct the node.
            try_construct(Constr, Mods, Token);
        #yamerl_tag{uri = URI} ->
            %% We look up this URI in the tag's index.
            case proplists:get_value(URI, Tags) of
                Mod when Mod /= undefined ->
                    Mod:construct_token(Constr, undefined, Token);
                undefined ->
                    %% This tag isn't handled by anything!
                    Error = #yamerl_parsing_error{
                      name   = unrecognized_node,
                      token  = Tag,
                      line   = ?TOKEN_LINE(Tag),
                      column = ?TOKEN_COLUMN(Tag)
                    },
                    Error1 = yamerl_errors:format(Error,
                      "Tag \"~s\" unrecognized by any module", [URI]),
                    yamerl_errors:throw(Error1)
            end
    end,
    handle_construct_return(Constr, Doc, Ret);

construct(
  #yamerl_constr{current_doc = Doc, current_node_is_leaf = false} = Constr,
  #yamerl_anchor{name = Anchor}) when Doc /= undefined ->
    handle_construct_return(Constr, Doc, #node_anchor{name = Anchor});

construct(
  #yamerl_constr{current_doc = Doc, anchors = Anchors} = Constr,
  #yamerl_alias{name = Alias} = Token) when Doc /= undefined ->
    try
        Node = dict:fetch(Alias, Anchors),
        handle_construct_return(Constr, Doc, {finished, Node})
    catch
        _:_ ->
            %% This alias references a non-existent anchor!
            Error = #yamerl_parsing_error{
              name   = no_matching_anchor,
              token  = Token,
              line   = ?TOKEN_LINE(Token),
              column = ?TOKEN_COLUMN(Token)
            },
            Error1 = yamerl_errors:format(Error,
              "No anchor corresponds to alias \"~s\"", [Alias]),
            yamerl_errors:throw(Error1)
    end;

construct(
  #yamerl_constr{current_doc =
    [#unfinished_node{module = Mod} = Node | Doc]} = Constr,
  Token) ->
    %% This token continues a node. We call the current node's module to
    %% handle it.
    Ret = Mod:construct_token(Constr, Node, Token),
    handle_construct_return(Constr, Doc, Ret).

try_construct(Constr, [Mod | Rest], Token) ->
    case Mod:try_construct_token(Constr, undefined, Token) of
        unrecognized -> try_construct(Constr, Rest, Token);
        Ret          -> Ret
    end;
try_construct(_, [], Token) ->
    Error = #yamerl_parsing_error{
      name   = unrecognized_node,
      token  = Token,
      text   = "No module found to handle node",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    yamerl_errors:throw(Error).

construct_parent(#yamerl_constr{anchors = Anchors} = Constr,
  [#node_anchor{name = Anchor} | Doc], Child) ->
    Anchors1 = dict:store(Anchor, Child, Anchors),
    Constr1  = Constr#yamerl_constr{
      anchors = Anchors1
    },
    construct_parent(Constr1, Doc, Child);
construct_parent(#yamerl_constr{docs = Docs, docs_count = Count} = Constr,
  [#yamerl_doc{} = Doc], Root) ->
    %% This node is the root of the document.
    Doc1 = Doc#yamerl_doc{
      root = Root
    },
    Constr1 = Constr#yamerl_constr{
      docs                 = Docs ++ [Doc1],
      docs_count           = Count + 1,
      current_doc          = undefined,
      current_node_is_leaf = false,
      anchors              = dict:new()
    },
    return_new_fun(Constr1);
construct_parent(Constr, [#unfinished_node{module = Mod} = Node | Doc],
  Child) ->
    %% We call the parent node's module to handle this new child node.
    Ret = Mod:construct_node(Constr, Node, Child),
    handle_construct_return(Constr, Doc, Ret).

handle_construct_return(Constr, Doc, {finished, Node}) ->
    %% Give this node to the parent node.
    construct_parent(Constr, Doc, Node);
handle_construct_return(Constr, Doc, {unfinished, Node, Is_Leaf}) ->
    %% Unfinished node, wait for the next tokens.
    Constr1 = Constr#yamerl_constr{
      current_doc          = [Node | Doc],
      current_node_is_leaf = Is_Leaf
    },
    return_new_fun(Constr1);
handle_construct_return(Constr, Doc, #node_anchor{} = Anchor) ->
    %% Anchor before a (not-yet-started) node, wait this node.
    Constr1 = Constr#yamerl_constr{
      current_doc = [Anchor | Doc]
    },
    return_new_fun(Constr1).

return_new_fun(#yamerl_constr{detailed_constr = Detailed} = Constr) ->
    Fun = fun
        (get_docs) when not Detailed ->
            [Doc#yamerl_doc.root || Doc <- Constr#yamerl_constr.docs];
        (get_docs) ->
            Constr#yamerl_constr.docs;
        (get_constr) ->
            Constr;
        (T) ->
            construct(Constr, T)
    end,
    {ok, Fun}.

%% -------------------------------------------------------------------
%% Node modules.
%% -------------------------------------------------------------------

setup_node_mods(Constr) ->
    Mods1 = umerge_unsorted(
      proplists:get_value(node_mods, Constr#yamerl_constr.options, []),
      yamerl_app:get_param(node_mods)
    ),
    Schema = proplists:get_value(schema, Constr#yamerl_constr.options, core),
    Mods   = case Schema of
        failsafe -> umerge_unsorted(Mods1, ?FAILSAFE_SCHEMA_MODS);
        json     -> umerge_unsorted(Mods1, ?JSON_SCHEMA_MODS);
        core     -> umerge_unsorted(Mods1, ?CORE_SCHEMA_MODS);
        yaml11   -> umerge_unsorted(Mods1, ?YAML11_SCHEMA_MODS)
    end,
    Auto    = filter_autodetection_capable_mods(Mods, []),
    Tags    = index_tags(Mods, []),
    Constr1 = Constr#yamerl_constr{
      mods = Auto,
      tags = Tags
    },
    return_new_fun(Constr1).

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
    Options0 = proplists:unfold(Options),
    {Constr_Options, Parser_Options, Ext_Options} = filter_options(Options0),
    check_options(Constr_Options),
    Detailed = proplists:get_value(detailed_constr, Constr_Options, false),
    Constr = #yamerl_constr{
      options         = Constr_Options,
      ext_options     = Ext_Options,
      detailed_constr = Detailed
    },
    {ok, Token_Fun} = setup_node_mods(Constr),
    [{token_fun, Token_Fun} | Parser_Options].

filter_options(Options) ->
    Constr_Option_Names = option_names(),
    Parser_Option_Names = yamerl_parser:option_names(),
    filter_options2(Options, Constr_Option_Names, Parser_Option_Names,
      [], [], []).

filter_options2([{Name, _} = Option | Rest],
  Constr_Option_Names, Parser_Option_Names,
  Constr_Options, Parser_Options, Ext_Options) ->
    case lists:member(Name, Constr_Option_Names) of
        true ->
            filter_options2(Rest,
              Constr_Option_Names, Parser_Option_Names,
              [Option | Constr_Options], Parser_Options, Ext_Options);
        false ->
            case lists:member(Name, Parser_Option_Names) of
                true ->
                    filter_options2(Rest,
                      Constr_Option_Names, Parser_Option_Names,
                      Constr_Options, [Option | Parser_Options], Ext_Options);
                false ->
                    filter_options2(Rest,
                      Constr_Option_Names, Parser_Option_Names,
                      Constr_Options, Parser_Options, [Option | Ext_Options])
            end
    end;
filter_options2([], _, _, Constr_Options, Parser_Options, Ext_Options) ->
    {
      lists:reverse(Constr_Options),
      lists:reverse(Parser_Options),
      lists:reverse(Ext_Options)
    }.

option_names() ->
    [
      node_mods,
      schema,
      detailed_constr
    ].

check_options([Option | Rest]) ->
    case is_option_valid(Option) of
        true  -> check_options(Rest);
        false -> invalid_option(Option)
    end;
check_options([]) ->
    ok.

is_option_valid({detailed_constr, Flag}) when is_boolean(Flag) ->
    true;
is_option_valid({node_mods, Mods}) when is_list(Mods) ->
    Fun = fun(Mod) ->
        not yamerl_app:is_node_mod(Mod)
    end,
    case lists:filter(Fun, Mods) of
        [] -> true;
        _  -> false
    end;
is_option_valid({schema, Schema})
when Schema == failsafe
orelse Schema == json
orelse Schema == core
orelse Schema == yaml11 ->
    true;
is_option_valid(_) ->
    false.

invalid_option(Option) ->
    Error = #yamerl_invalid_option{
      option = Option
    },
    Error1 = case Option of
        {detailed_constr, _} ->
            Error#yamerl_invalid_option{
              text = "Invalid value for option \"detailed_constr\": "
              "it must be a boolean"
            };
        {node_mods, _} ->
            Error#yamerl_invalid_option{
              text = "Invalid value for option \"node_mods\": "
              "it must be a list of modules"
            };
        _ ->
            yamerl_errors:format(Error, "Unknown option \"~w\"", [Option])
    end,
    yamerl_errors:throw(Error1).
