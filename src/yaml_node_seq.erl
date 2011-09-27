-module(yaml_node_seq).

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

-define(TAG, "tag:yaml.org,2002:seq").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Repr, Node,
  #yaml_collection_start{kind = sequence} = Token) ->
    construct_token(Repr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(_, undefined, #yaml_collection_start{} = Token) ->
    Pres = yaml_constr:get_pres_details(Token),
    Node = #unfinished_node{
      path = {seq, 0},
      pres = Pres,
      priv = []
    },
    {unfinished, Node, false};
construct_token(_, #unfinished_node{path = {seq, Count}, priv = Entries} = Node,
  #yaml_sequence_entry{}) ->
    Node1 = Node#unfinished_node{
      path = {seq, Count + 1},
      priv = ['$insert_here' | Entries]
    },
    {unfinished, Node1, false};

construct_token(#yaml_constr{simple_structs = true},
  #unfinished_node{priv = Entries}, #yaml_collection_end{}) ->
    Node = lists:reverse(Entries),
    {finished, Node};
construct_token(#yaml_constr{simple_structs = false},
  #unfinished_node{path = {_, Count}, pres = Pres, priv = Entries},
  #yaml_collection_end{}) ->
    Node = #yaml_seq{
      module  = ?MODULE,
      tag     = ?TAG,
      pres    = Pres,
      entries = lists:reverse(Entries),
      count   = Count
    },
    {finished, Node};

construct_token(_, _, Token) ->
    Error = #yaml_parsing_error{
      name   = not_a_sequence,
      token  = Token,
      text   = "Invalid sequence",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

construct_node(_, #unfinished_node{priv = [_ | Entries]} = Node, Entry) ->
    Node1 = Node#unfinished_node{
      priv = [Entry | Entries]
    },
    {unfinished, Node1, false}.

node_pres(Node) ->
    ?NODE_PRES(Node).
