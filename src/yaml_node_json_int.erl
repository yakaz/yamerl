-module(yaml_node_json_int).

-include("yaml_errors.hrl").
-include("yaml_tokens.hrl").
-include("yaml_nodes.hrl").
-include("internal/yaml_constr.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    node_pres/1,
    string_to_integer/1
  ]).

-define(TAG, "tag:yaml.org,2002:int").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yaml_scalar{tag = #yaml_tag{uri = {non_specific, "?"}}} = Token) ->
    try
        construct_token(Constr, Node, Token)
    catch
        _:#yaml_parsing_error{name = not_an_integer} ->
            unrecognized
    end;
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yaml_constr{simple_structs = true},
  undefined, #yaml_scalar{text = Text} = Token) ->
    case string_to_integer(Text) of
        error ->
            exception(Token);
        Int ->
            {finished, Int}
    end;
construct_token(#yaml_constr{simple_structs = false},
  undefined, #yaml_scalar{text = Text} = Token) ->
    case string_to_integer(Text) of
        error ->
            exception(Token);
        Int ->
            Pres = yaml_constr:get_pres_details(Token),
            Node = #yaml_int{
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

%% Zero and sign.
string_to_integer("0") ->
    0;
string_to_integer("-0") ->
    0;
string_to_integer([$-, C | Text]) when C >= $1 andalso C =< $9 ->
    case yaml_node_int:base10_to_integer(Text, C - $0) of
        error -> error;
        Int   -> -Int
    end;
string_to_integer([C | Text]) when C >= $1 andalso C =< $9 ->
    yaml_node_int:base10_to_integer(Text, C - $0);
string_to_integer(_) ->
    error.

exception(Token) ->
    Error = #yaml_parsing_error{
      name   = not_an_integer,
      token  = Token,
      text   = "Invalid integer",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).
