-module(yaml_node_map).

-include("yaml_errors.hrl").
-include("yaml_tokens.hrl").
-include("yaml_nodes.hrl").
-include("internal/yaml_repr.hrl").

%% Public API.
-export([
    tags/0,
    try_represent_token/3,
    represent_token/3,
    represent_node/3,
    node_pres/1
  ]).

-define(TAG, "tag:yaml.org,2002:map").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_represent_token(Repr, Node,
  #yaml_collection_start{kind = mapping} = Token) ->
    represent_token(Repr, Node, Token);
try_represent_token(_, _, _) ->
    unrecognized.

represent_token(_, undefined, #yaml_collection_start{} = Token) ->
    Pres = yaml_repr:get_pres_details(Token),
    Node = #unfinished_node{
      path = {map, undefined},
      pres = Pres,
      priv = []
    },
    {unfinished, Node, false};
represent_token(_, #unfinished_node{priv = Pairs} = Node,
  #yaml_mapping_key{}) ->
    Node1 = Node#unfinished_node{
      priv = [{'$insert_here', undefined} | Pairs]
    },
    {unfinished, Node1, false};
represent_token(_, #unfinished_node{priv = [{Key, undefined} | Pairs]} = Node,
  #yaml_mapping_value{}) ->
    Node1 = Node#unfinished_node{
      priv = [{Key, '$insert_here'} | Pairs]
    },
    {unfinished, Node1, false};

represent_token(#yaml_repr{simple_structs = true},
  #unfinished_node{priv = Pairs}, #yaml_collection_end{}) ->
    Node = lists:reverse(Pairs),
    {finished, Node};
represent_token(#yaml_repr{simple_structs = false},
  #unfinished_node{pres = Pres, priv = Pairs}, #yaml_collection_end{}) ->
    Node = #yaml_map{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres,
      pairs  = lists:reverse(Pairs)
    },
    {finished, Node};

represent_token(_, _, Token) ->
    Error = #yaml_parsing_error{
      name   = not_a_mapping,
      token  = Token,
      text   = "Invalid mapping",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

represent_node(_,
  #unfinished_node{path = {map, undefined},
    priv = [{'$insert_here', undefined} | Pairs]} = Node,
  Key) ->
    Node1 = Node#unfinished_node{
      path = {map, Key},
      priv = [{Key, undefined} | Pairs]
    },
    {unfinished, Node1, false};
represent_node(_,
  #unfinished_node{path = {map, _},
    priv = [{Key, '$insert_here'} | Pairs]} = Node,
  Value) ->
    Node1 = Node#unfinished_node{
      path = {map, undefined},
      priv = [{Key, Value} | Pairs]
    },
    {unfinished, Node1, false}.

node_pres(Node) ->
    ?NODE_PRES(Node).
