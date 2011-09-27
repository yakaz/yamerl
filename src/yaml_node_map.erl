-module(yaml_node_map).

-include("yaml_errors.hrl").
-include("yaml_tokens.hrl").
-include("yaml_nodes.hrl").
-include("internal/yaml_constr.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    construct_node/3,
    node_pres/1
  ]).

-define(TAG, "tag:yaml.org,2002:map").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yaml_collection_start{kind = mapping} = Token) ->
    construct_token(Constr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(_, undefined, #yaml_collection_start{} = Token) ->
    Pres = yaml_constr:get_pres_details(Token),
    Node = #unfinished_node{
      path = {map, undefined},
      pres = Pres,
      priv = []
    },
    {unfinished, Node, false};
construct_token(_, #unfinished_node{priv = Pairs} = Node,
  #yaml_mapping_key{}) ->
    Node1 = Node#unfinished_node{
      priv = [{'$insert_here', undefined} | Pairs]
    },
    {unfinished, Node1, false};
construct_token(_, #unfinished_node{priv = [{Key, undefined} | Pairs]} = Node,
  #yaml_mapping_value{}) ->
    Node1 = Node#unfinished_node{
      priv = [{Key, '$insert_here'} | Pairs]
    },
    {unfinished, Node1, false};

construct_token(#yaml_constr{simple_structs = true},
  #unfinished_node{priv = Pairs}, #yaml_collection_end{}) ->
    Node = lists:reverse(Pairs),
    {finished, Node};
construct_token(#yaml_constr{simple_structs = false},
  #unfinished_node{pres = Pres, priv = Pairs}, #yaml_collection_end{}) ->
    Node = #yaml_map{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres,
      pairs  = lists:reverse(Pairs)
    },
    {finished, Node};

construct_token(_, _, Token) ->
    Error = #yaml_parsing_error{
      name   = not_a_mapping,
      token  = Token,
      text   = "Invalid mapping",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

construct_node(_,
  #unfinished_node{path = {map, undefined},
    priv = [{'$insert_here', undefined} | Pairs]} = Node,
  Key) ->
    Node1 = Node#unfinished_node{
      path = {map, Key},
      priv = [{Key, undefined} | Pairs]
    },
    {unfinished, Node1, false};
construct_node(_,
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
