-module(yaml_node_seq).

-include("yaml_tokens.hrl").
-include("yaml_repr.hrl").
-include("yaml_nodes.hrl").

%% Public API.
-export([
    tags/0,
    matches/2,
    represent_token/3,
    represent_node/3
  ]).

-define(TAG, "tag:yaml.org,2002:seq").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

matches(_, #yaml_collection_start{kind = sequence}) -> true;
matches(_, _)                                       -> false.

represent_token(_, undefined, #yaml_collection_start{}) ->
    Node = {?MODULE, {seq, 0}, []},
    {unfinished, Node, false};
represent_token(_, {Mod, {seq, Count}, Entries}, #yaml_sequence_entry{}) ->
    Node = {Mod, {seq, Count + 1}, ['$insert_here' | Entries]},
    {unfinished, Node, false};

represent_token(#yaml_repr{simple_structs = true},
  {_, _, Entries}, #yaml_collection_end{}) ->
    Node = lists:reverse(Entries),
    {finished, Node};
represent_token(#yaml_repr{simple_structs = false},
  {_, {_, Count}, Entries}, #yaml_collection_end{}) ->
    Node = #yaml_sequence{
      module  = ?MODULE,
      tag     = ?TAG,
      entries = lists:reverse(Entries),
      count   = Count
    },
    {finished, Node}.

represent_node(_, {Mod, Count, [_ | Entries]}, Entry) ->
    Node = {Mod, Count, [Entry | Entries]},
    {unfinished, Node, false}.
