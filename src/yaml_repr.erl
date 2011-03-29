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
    last_chunk/2
  ]).

-define(DEFAULT_NODE_MODS, [
    yaml_node_str,
    yaml_node_seq,
    yaml_node_map
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
            Repr = (yaml_parser:get_token_fun(Parser))(get_repr),
            Repr#yaml_repr.docs;
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
    Repr   = (yaml_parser:get_token_fun(Parser))(get_repr),
    Repr#yaml_repr.docs.

file(Filename) ->
    file(Filename, []).

file(Filename, Options) ->
    Parser_Options = initialize(Options),
    Parser = yaml_parser:file(Filename, Parser_Options),
    Repr   = (yaml_parser:get_token_fun(Parser))(get_repr),
    Repr#yaml_repr.docs.

%% -------------------------------------------------------------------
%% Representation.
%% -------------------------------------------------------------------

represent(Repr, #yaml_doc_start{}) ->
    %% Prepare a document node.
    Doc = #yaml_document{},
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
    Mod = case Tag of
        #yaml_tag{uri = {non_specific, _}} ->
            %% The node has a non-specific tag. We let each module
            %% decides if they want to represent the node.
            find_matching_mod(Repr, Mods, Token);
        #yaml_tag{uri = URI} ->
            %% We look up this URI in the tag's index.
            case proplists:get_value(URI, Tags) of
                M when M /= undefined ->
                    M;
                undefined ->
                    %% This tag isn't handled by anything!
                    Text = lists:flatten(io_lib:format(
                        "Tag \"~s\" unrecognized by any module~n", [URI])),
                    Error = #yaml_parser_error{
                      name   = unrecognized_tag,
                      token  = Tag,
                      text   = Text,
                      line   = ?TOKEN_LINE(Tag),
                      column = ?TOKEN_COLUMN(Tag)
                    },
                    throw(Error)
            end
    end,
    Ret = Mod:represent_token(Repr, undefined, Token),
    handle_represent_return(Repr, Doc, Ret);

represent(#yaml_repr{current_doc = [{Mod, _, _} = Node | Doc]} = Repr, Token) ->
    %% This token continues a node. We call the current node's module to
    %% handle it.
    Ret = Mod:represent_token(Repr, Node, Token),
    handle_represent_return(Repr, Doc, Ret).

find_matching_mod(Repr, [Mod | Rest], Token) ->
    case Mod:matches(Repr, Token) of
        true  -> Mod;
        false -> find_matching_mod(Repr, Rest, Token)
    end;
find_matching_mod(_, [], Token) ->
    Error = #yaml_parser_error{
      name   = unrecognized_tag,
      token  = Token,
      text   = "No module found to handle token~n",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

represent_parent(#yaml_repr{docs = Docs, docs_count = Count} = Repr,
  [#yaml_document{} = Doc], Root) ->
    %% This node is the root of the document.
    Doc1 = Doc#yaml_document{
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
represent_parent(Repr, [{Mod, _, _} = Node | Doc], Child) ->
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

return_new_fun(Repr) ->
    Fun = fun
        (get_repr) -> Repr;
        (T)        -> represent(Repr, T)
    end,
    {ok, Fun}.

%% -------------------------------------------------------------------
%% Node modules.
%% -------------------------------------------------------------------

setup_node_mods(#yaml_repr{options = Options} = Repr) ->
    Mods  = proplists:get_value(node_mods, Options, []) ++ ?DEFAULT_NODE_MODS,
    Tags  = index_tags(Mods, []),
    Repr1 = Repr#yaml_repr{
      mods = Mods,
      tags = Tags
    },
    return_new_fun(Repr1).

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
is_option_valid({node_mods, Mods}) when is_list(Mods) ->
    Fun = fun(Mod) ->
        try
            Mod:module_info(),
            false
        catch
            _:_ ->
                true
        end
    end,
    case lists:filter(Fun, Mods) of
        [] -> true;
        _  -> false
    end;
is_option_valid(_) ->
    false.

invalid_option(Option) ->
    Error = #yaml_parser_error{
      name  = invalid_repr_option,
      extra = [{option, Option}]
    },
    Error1 = case Option of
        {simpl_estructs, _} ->
            Error#yaml_parser_error{
              text = "Invalid value for option \"simple_structs\": "
              "it must be a boolean\n"
            };
        {node_mods, _} ->
            Error#yaml_parser_error{
              text = "Invalid value for option \"node_mods\": "
              "it must be a list of module names (atoms)\n"
            };
        _ ->
            Error#yaml_parser_error{
              text = io_lib:flatten(io_lib:format("Unknown option \"~w\"~n",
                  [Option]))
            }
    end,
    throw(Error1).
