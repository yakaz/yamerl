-module(yaml_node_erlang_atom).

-include("yaml_errors.hrl").
-include("yaml_tokens.hrl").
-include("yaml_nodes.hrl").
-include("internal/yaml_constr.hrl").

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

construct_token(#yaml_constr{simple_structs = true},
  undefined, #yaml_scalar{text = Text}) ->
    {finished, list_to_atom(Text)};
construct_token(#yaml_constr{simple_structs = false},
  undefined, #yaml_scalar{text = Text} = Token) ->
    Pres = yaml_constr:get_pres_details(Token),
    Node = #yaml_erlang_atom{
      module   = ?MODULE,
      tag      = ?TAG,
      pres     = Pres,
      name     = list_to_atom(Text)
    },
    {finished, Node};

construct_token(_, _, Token) ->
    Error = #yaml_parsing_error{
      name   = not_an_erlang_atom,
      token  = Token,
      text   = "Invalid Erlang atom",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

node_pres(Node) ->
    ?NODE_PRES(Node).
