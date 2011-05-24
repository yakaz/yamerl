-module(yaml_node_timestamp).

-include("yaml_parser.hrl").
-include("yaml_tokens.hrl").
-include("yaml_repr.hrl").
-include("yaml_nodes.hrl").

%% Public API.
-export([
    tags/0,
    try_represent_token/3,
    represent_token/3,
    node_pres/1
  ]).

-define(TAG, "tag:yakaz.com,2011:timestamp").

-define(REGEX,
  "(?:"
  "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])"
  "[Tt ]"
  "([0-9][0-9]):([0-9][0-9]):([0-9][0-9])"
  ")|(?:"
  "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])"
  ")|(?:"
  "([0-9][0-9]):([0-9][0-9]):([0-9][0-9])"
  ")").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_represent_token(Repr, Node,
  #yaml_scalar{tag = #yaml_tag{uri = {non_specific, "?"}}} = Token) ->
    try
        represent_token(Repr, Node, Token)
    catch
        _:#yaml_parser_error{name = not_a_timestamp} ->
            unrecognized
    end;
try_represent_token(_, _, _) ->
    unrecognized.

represent_token(#yaml_repr{simple_structs = true},
  undefined, #yaml_scalar{text = Text} = Token) ->
    case string_to_timestamp(Text) of
        {undefined, undefined, undefined, H, Mi, S, _, _} ->
            {finished, {undefined, {H, Mi, S}}};
        {Y, Mo, D, H, Mi, S, _, _} ->
            {finished, {{Y, Mo, D}, {H, Mi, S}}};
        error ->
            exception(Token)
    end;
represent_token(#yaml_repr{simple_structs = false},
  undefined, #yaml_scalar{text = Text} = Token) ->
    case string_to_timestamp(Text) of
        {Y, Mo, D, H, Mi, S, F, Z} ->
            Pres = yaml_repr:get_pres_details(Token),
            Node = #yaml_timestamp{
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

represent_token(_, _, Token) ->
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
              0, 0, 0, 0, 0
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
    Error = #yaml_parser_error{
      name   = not_a_timestamp,
      token  = Token,
      text   = "Invalid timestamp",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).
