-module(yamerl_node_timestamp).

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

-define(TAG, "tag:yamerl,2012:timestamp").

-define(REGEX,
  "^(?:"
  "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])"
  "[Tt ]"
  "([0-9][0-9]):([0-9][0-9]):([0-9][0-9])"
  ")|(?:"
  "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])"
  ")|(?:"
  "([0-9][0-9]):([0-9][0-9]):([0-9][0-9])"
  ")$").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yamerl_scalar{tag = #yamerl_tag{uri = {non_specific, "?"}}} = Token) ->
    try
        construct_token(Constr, Node, Token)
    catch
        _:#yamerl_parsing_error{name = not_a_timestamp} ->
            unrecognized
    end;
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yamerl_constr{detailed_constr = false},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case string_to_timestamp(Text) of
        {undefined, undefined, undefined, H, Mi, S, _, _} ->
            {finished, {undefined, {H, Mi, S}}};
        {Y, Mo, D, undefined, undefined, undefined, _, _} ->
            {finished, {{Y, Mo, D}, undefined}};
        {Y, Mo, D, H, Mi, S, _, _} ->
            {finished, {{Y, Mo, D}, {H, Mi, S}}};
        error ->
            exception(Token)
    end;
construct_token(#yamerl_constr{detailed_constr = true},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case string_to_timestamp(Text) of
        {Y, Mo, D, H, Mi, S, F, Z} ->
            Pres = yamerl_constr:get_pres_details(Token),
            Node = #yamerl_timestamp{
              module = ?MODULE,
              tag    = ?TAG,
              pres   = Pres,
              year   = Y,
              month  = Mo,
              day    = D,
              hour   = H,
              minute = Mi,
              second = S,
              frac   = F,
              tz     = Z
            },
            {finished, Node};
        error ->
            exception(Token)
    end;

construct_token(_, _, Token) ->
    exception(Token).

node_pres(Node) ->
    ?NODE_PRES(Node).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

string_to_timestamp(Text) ->
    case re:run(Text, ?REGEX, [{capture, all_but_first, list}]) of
        {match, [Y, Mo, D, H, Mi, S]} ->
            %% Date/time.
            {
              list_to_integer(Y),
              list_to_integer(Mo),
              list_to_integer(D),
              list_to_integer(H),
              list_to_integer(Mi),
              list_to_integer(S),
              0, 0
            };
        {match, [_, _, _, _, _, _, Y, Mo, D]} ->
            %% Only a date.
            {
              list_to_integer(Y),
              list_to_integer(Mo),
              list_to_integer(D),
              undefined, undefined, undefined,
              0, 0
            };
        {match, [_, _, _, _, _, _, _, _, _, H, Mi, S]} ->
            %% Only a time.
            {
              undefined, undefined, undefined,
              list_to_integer(H),
              list_to_integer(Mi),
              list_to_integer(S),
              0, 0
            };
        _ ->
            error
    end.

exception(Token) ->
    Error = #yamerl_parsing_error{
      name   = not_a_timestamp,
      token  = Token,
      text   = "Invalid timestamp",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).
