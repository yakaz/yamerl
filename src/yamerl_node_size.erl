-module(yamerl_node_size).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    node_pres/1
  ]).

-define(TAG, "tag:yamerl,2012:size").

-define(REGEX, "^([0-9]+)(k|M|G|T)(B?)$").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yamerl_scalar{tag = #yamerl_tag{uri = {non_specific, "?"}}} = Token) ->
    try
        construct_token(Constr, Node, Token)
    catch
        _:#yamerl_parsing_error{name = not_a_size} ->
            unrecognized
    end;
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yamerl_constr{simple_structs = true},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case string_to_size(Text) of
        error ->
            exception(Token);
        Int ->
            {finished, Int}
    end;
construct_token(#yamerl_constr{simple_structs = false},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    Pres = yamerl_constr:get_pres_details(Token),
    case string_to_size(Text) of
        error ->
            exception(Token);
        Int ->
            Node = #yamerl_int{
              module = ?MODULE,
              tag    = ?TAG,
              pres   = Pres,
              value  = Int
            },
            {finished, Node}
    end;

construct_token(_, _, Token) ->
    exception(Token).

node_pres(Node) ->
    ?NODE_PRES(Node).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

string_to_size(Text) ->
    case re:run(Text, ?REGEX, [{capture, all_but_first, list}]) of
        {match, [I, U, B]} ->
            Multiplier = case {U, B} of
                {"k", "B"} -> 1024;
                {"k", _}   -> 1000;
                {"M", "B"} -> 1048576;
                {"M", _}   -> 1000000;
                {"G", "B"} -> 1073741824;
                {"G", _}   -> 1000000000;
                {"T", "B"} -> 1099511627776;
                {"T", _}   -> 1000000000000
            end,
            case yamerl_node_int:string_to_integer(I) of
                error ->
                    error;
                Int ->
                    Int * Multiplier
            end;
        nomatch ->
            error
    end.

exception(Token) ->
    Error = #yamerl_parsing_error{
      name   = not_a_size,
      token  = Token,
      text   = "Invalid size",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).
