-module(yamerl_node_null).

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

-define(TAG, "tag:yaml.org,2002:null").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yamerl_scalar{tag = #yamerl_tag{uri = {non_specific, "?"}},
  text = Text} = Token) when
  Text == "" orelse
  Text == "null" orelse
  Text == "Null" orelse
  Text == "NULL" orelse
  Text == "~" ->
    construct_token(Constr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yamerl_constr{detailed_constr = false},
  undefined, #yamerl_scalar{}) ->
    {finished, null};
construct_token(#yamerl_constr{detailed_constr = true},
  undefined, #yamerl_scalar{} = Token) ->
    Pres = yamerl_constr:get_pres_details(Token),
    Node = #yamerl_null{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres
    },
    {finished, Node}.

node_pres(Node) ->
    ?NODE_PRES(Node).
