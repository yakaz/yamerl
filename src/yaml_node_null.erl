-module(yaml_node_null).

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

-define(TAG, "tag:yaml.org,2002:null").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_represent_token(Repr, Node,
  #yaml_scalar{tag = #yaml_tag{uri = {non_specific, "?"}},
  text = Text} = Token) when
  Text == "" orelse
  Text == "null" orelse
  Text == "Null" orelse
  Text == "NULL" orelse
  Text == "~" ->
    represent_token(Repr, Node, Token);
try_represent_token(_, _, _) ->
    unrecognized.

represent_token(#yaml_repr{simple_structs = true},
  undefined, #yaml_scalar{}) ->
    {finished, null};
represent_token(#yaml_repr{simple_structs = false},
  undefined, #yaml_scalar{} = Token) ->
    Pres = yaml_repr:get_pres_details(Token),
    Node = #yaml_null{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres
    },
    {finished, Node}.

node_pres(Node) ->
    ?NODE_PRES(Node).
