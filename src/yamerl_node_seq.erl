-module(yamerl_node_seq).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

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

try_construct_token(Constr, Node,
  #yamerl_collection_start{kind = sequence} = Token) ->
    construct_token(Constr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(_, undefined, #yamerl_collection_start{} = Token) ->
    Pres = yamerl_constr:get_pres_details(Token),
    Node = #unfinished_node{
      path = {seq, 0},
      pres = Pres,
      priv = []
    },
    {unfinished, Node, false};
construct_token(_, #unfinished_node{path = {seq, Count}, priv = Entries} = Node,
  #yamerl_sequence_entry{}) ->
    Node1 = Node#unfinished_node{
      path = {seq, Count + 1},
      priv = ['$insert_here' | Entries]
    },
    {unfinished, Node1, false};

construct_token(#yamerl_constr{detailed_constr = false},
  #unfinished_node{priv = Entries}, #yamerl_collection_end{}) ->
    Node = lists:reverse(Entries),
    {finished, Node};
construct_token(#yamerl_constr{detailed_constr = true},
  #unfinished_node{path = {_, Count}, pres = Pres, priv = Entries},
  #yamerl_collection_end{}) ->
    Node = #yamerl_seq{
      module  = ?MODULE,
      tag     = ?TAG,
      pres    = Pres,
      entries = lists:reverse(Entries),
      count   = Count
    },
    {finished, Node};

construct_token(_, _, Token) ->
    Error = #yamerl_parsing_error{
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
