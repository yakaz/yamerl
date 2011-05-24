-module(yaml_node_seq).

-include("yaml_parser.hrl").
-include("yaml_tokens.hrl").
-include("yaml_repr.hrl").
-include("yaml_nodes.hrl").

%% Public API.
-export([
    tags/0,
    try_represent_token/3,
    represent_token/3,
    represent_node/3,
    node_pres/1
  ]).

-define(TAG, "tag:yaml.org,2002:seq").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_represent_token(Repr, Node,
  #yaml_collection_start{kind = sequence} = Token) ->
    represent_token(Repr, Node, Token);
try_represent_token(_, _, _) ->
    unrecognized.

represent_token(_, undefined, #yaml_collection_start{} = Token) ->
    Pres = yaml_repr:get_pres_details(Token),
    Node = #unfinished_node{
      path = {seq, 0},
      pres = Pres,
      priv = []
    },
    {unfinished, Node, false};
represent_token(_, #unfinished_node{path = {seq, Count}, priv = Entries} = Node,
  #yaml_sequence_entry{}) ->
    Node1 = Node#unfinished_node{
      path = {seq, Count + 1},
      priv = ['$insert_here' | Entries]
    },
    {unfinished, Node1, false};

represent_token(#yaml_repr{simple_structs = true},
  #unfinished_node{priv = Entries}, #yaml_collection_end{}) ->
    Node = lists:reverse(Entries),
    {finished, Node};
represent_token(#yaml_repr{simple_structs = false},
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

represent_token(_, _, Token) ->
    Error = #yaml_parser_error{
      name   = not_a_sequence,
      token  = Token,
      text   = "Invalid sequence",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

represent_node(_, #unfinished_node{priv = [_ | Entries]} = Node, Entry) ->
    Node1 = Node#unfinished_node{
      priv = [Entry | Entries]
    },
    {unfinished, Node1, false}.

node_pres(Node) ->
    ?NODE_PRES(Node).
