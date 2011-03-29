-module(yaml_node_str).

-include("yaml_tokens.hrl").
-include("yaml_repr.hrl").
-include("yaml_nodes.hrl").

%% Public API.
-export([
    tags/0,
    matches/2,
    represent_token/3
  ]).

-define(TAG, "tag:yaml.org,2002:str").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

matches(_, #yaml_scalar{}) -> true;
matches(_, _)              -> false.

represent_token(#yaml_repr{simple_structs = true},
  undefined, #yaml_scalar{text = Text}) ->
    Node = Text,
    {finished, Node};
represent_token(#yaml_repr{simple_structs = false},
  undefined, #yaml_scalar{text = Text}) ->
    Node = #yaml_string{
      module = ?MODULE,
      tag    = ?TAG,
      text   = Text
    },
    {finished, Node}.
