-module(yaml_node_str).

-include("yaml_errors.hrl").
-include("yaml_tokens.hrl").
-include("yaml_nodes.hrl").
-include("internal/yaml_constr.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    node_pres/1
  ]).

-define(TAG, "tag:yaml.org,2002:str").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Repr, Node, #yaml_scalar{} = Token) ->
    construct_token(Repr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yaml_constr{simple_structs = true},
  undefined, #yaml_scalar{text = Text}) ->
    Node = Text,
    {finished, Node};
construct_token(#yaml_constr{simple_structs = false},
  undefined, #yaml_scalar{text = Text} = Token) ->
    Pres = yaml_constr:get_pres_details(Token),
    Node = #yaml_str{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres,
      text   = Text
    },
    {finished, Node};

construct_token(_, _, Token) ->
    Error = #yaml_parsing_error{
      name   = not_a_string,
      token  = Token,
      text   = "Invalid string",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

node_pres(Node) ->
    ?NODE_PRES(Node).
