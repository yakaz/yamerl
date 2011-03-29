-module(yaml_node_map).

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

-define(TAG, "tag:yaml.org,2002:map").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

matches(_, #yaml_collection_start{kind = mapping}) -> true;
matches(_, _)                                      -> false.

represent_token(_, undefined, #yaml_collection_start{}) ->
    Node = {?MODULE, {map, undefined}, []},
    {unfinished, Node, false};
represent_token(_, {Mod, Path_Comp, Pairs},
  #yaml_mapping_key{}) ->
    Node = {Mod, Path_Comp, [{'$insert_here', undefined} | Pairs]},
    {unfinished, Node, false};
represent_token(_, {Mod, Path_Comp, [{Key, undefined} | Pairs]},
  #yaml_mapping_value{}) ->
    Node = {Mod, Path_Comp, [{Key, '$insert_here'} | Pairs]},
    {unfinished, Node, false};

represent_token(#yaml_repr{simple_structs = true},
  {_, _, Pairs}, #yaml_collection_end{}) ->
    Node = lists:reverse(Pairs),
    {finished, Node};
represent_token(#yaml_repr{simple_structs = false},
  {_, _, Pairs}, #yaml_collection_end{}) ->
    Node = #yaml_mapping{
      module = ?MODULE,
      tag    = ?TAG,
      pairs  = lists:reverse(Pairs)
    },
    {finished, Node}.

represent_node(_,
  {Mod, {map, undefined}, [{'$insert_here', undefined} | Pairs]},
  Key) ->
    Node = {Mod, {map, Key}, [{Key, undefined} | Pairs]},
    {unfinished, Node, false};
represent_node(_,
  {Mod, {map, _}, [{Key, '$insert_here'} | Pairs]},
  Value) ->
    Node = {Mod, {map, undefined}, [{Key, Value} | Pairs]},
    {unfinished, Node, false}.
