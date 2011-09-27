-module(yaml_node_null).

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

-define(TAG, "tag:yaml.org,2002:null").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yaml_scalar{tag = #yaml_tag{uri = {non_specific, "?"}},
  text = Text} = Token) when
  Text == "" orelse
  Text == "null" orelse
  Text == "Null" orelse
  Text == "NULL" orelse
  Text == "~" ->
    construct_token(Constr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yaml_constr{simple_structs = true},
  undefined, #yaml_scalar{}) ->
    {finished, null};
construct_token(#yaml_constr{simple_structs = false},
  undefined, #yaml_scalar{} = Token) ->
    Pres = yaml_constr:get_pres_details(Token),
    Node = #yaml_null{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres
    },
    {finished, Node}.

node_pres(Node) ->
    ?NODE_PRES(Node).
