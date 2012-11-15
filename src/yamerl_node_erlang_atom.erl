-module(yamerl_node_erlang_atom).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

%% Public API.
-export([
    tags/0,
    construct_token/3,
    node_pres/1
  ]).

-define(TAG, "tag:erlang.org,2011:atom").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

construct_token(#yamerl_constr{simple_structs = true},
  undefined, #yamerl_scalar{text = Text}) ->
    {finished, list_to_atom(Text)};
construct_token(#yamerl_constr{simple_structs = false},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    Pres = yamerl_constr:get_pres_details(Token),
    Node = #yamerl_erlang_atom{
      module   = ?MODULE,
      tag      = ?TAG,
      pres     = Pres,
      name     = list_to_atom(Text)
    },
    {finished, Node};

construct_token(_, _, Token) ->
    Error = #yamerl_parsing_error{
      name   = not_an_erlang_atom,
      token  = Token,
      text   = "Invalid Erlang atom",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

node_pres(Node) ->
    ?NODE_PRES(Node).
