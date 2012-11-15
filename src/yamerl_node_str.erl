-module(yamerl_node_str).

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

-define(TAG, "tag:yaml.org,2002:str").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node, #yamerl_scalar{} = Token) ->
    construct_token(Constr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yamerl_constr{simple_structs = true},
  undefined, #yamerl_scalar{text = Text}) ->
    Node = Text,
    {finished, Node};
construct_token(#yamerl_constr{simple_structs = false},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    Pres = yamerl_constr:get_pres_details(Token),
    Node = #yamerl_str{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres,
      text   = Text
    },
    {finished, Node};

construct_token(_, _, Token) ->
    Error = #yamerl_parsing_error{
      name   = not_a_string,
      token  = Token,
      text   = "Invalid string",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

node_pres(Node) ->
    ?NODE_PRES(Node).
