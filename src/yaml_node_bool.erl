-module(yaml_node_bool).

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

-define(TAG, "tag:yaml.org,2002:bool").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_represent_token(Repr, Node,
  #yaml_scalar{tag = #yaml_tag{uri = {non_specific, "?"}},
  text = Text} = Token) when
  Text == "true" orelse
  Text == "True" orelse
  Text == "TRUE" orelse
  Text == "false" orelse
  Text == "False" orelse
  Text == "FALSE" ->
    represent_token(Repr, Node, Token);
try_represent_token(_, _, _) ->
    unrecognized.

represent_token(#yaml_repr{simple_structs = true},
  undefined, #yaml_scalar{text = Text}) when
  Text == "true" orelse
  Text == "True" orelse
  Text == "TRUE" ->
    {finished, true};
represent_token(#yaml_repr{simple_structs = true},
  undefined, #yaml_scalar{text = Text}) when
  Text == "false" orelse
  Text == "False" orelse
  Text == "FALSE" ->
    {finished, false};
represent_token(#yaml_repr{simple_structs = false},
  undefined, #yaml_scalar{text = Text} = Token) when
  Text == "true" orelse
  Text == "True" orelse
  Text == "TRUE" ->
    Pres = yaml_repr:get_pres_details(Token),
    Node = #yaml_bool{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres,
      value  = true
    },
    {finished, Node};
represent_token(#yaml_repr{simple_structs = false},
  undefined, #yaml_scalar{text = Text} = Token) when
  Text == "false" orelse
  Text == "False" orelse
  Text == "FALSE" ->
    Pres = yaml_repr:get_pres_details(Token),
    Node = #yaml_bool{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres,
      value  = false
    },
    {finished, Node}.

node_pres(Node) ->
    ?NODE_PRES(Node).
