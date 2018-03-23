%-
% Copyright (c) 2012-2014 Yakaz
% Copyright (c) 2016-2018 Jean-Sébastien Pédron <jean-sebastien.pedron@dumbbell.fr>
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
% SUCH DAMAGE.

%% @private

-module(yamerl_node_map).

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

-define(TAG, "tag:yaml.org,2002:map").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yamerl_collection_start{kind = mapping} = Token) ->
    construct_token(Constr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(Constr, undefined, #yamerl_collection_start{} = Token) ->
    Map = case node_as_proplist_or_map(Constr) of
        proplist -> [];
        map      -> maps:new()
    end,
    Pres = yamerl_constr:get_pres_details(Token),
    Node = #unfinished_node{
      path = {map, undefined},
      pres = Pres,
      priv = Map
    },
    {unfinished, Node, false};
construct_token(_, #unfinished_node{priv = Map} = Node,
  #yamerl_mapping_key{}) ->
    Node1 = Node#unfinished_node{
      priv = {'$expecting_key', Map}
    },
    {unfinished, Node1, false};
construct_token(_, #unfinished_node{priv = {Key, Map}} = Node,
  #yamerl_mapping_value{}) when Key =/= '$expecting_key' ->
    Node1 = Node#unfinished_node{
      priv = {Key, '$expecting_value', Map}
    },
    {unfinished, Node1, false};

construct_token(#yamerl_constr{detailed_constr = false},
  #unfinished_node{priv = Map}, #yamerl_collection_end{})
  when not is_tuple(Map) ->
    Node = case is_list(Map) of
        true  -> lists:reverse(Map);
        false -> Map
    end,
    {finished, Node};
construct_token(#yamerl_constr{detailed_constr = true},
  #unfinished_node{pres = Pres, priv = Map}, #yamerl_collection_end{})
  when not is_tuple(Map) ->
    Map1 = case is_list(Map) of
        true  -> lists:reverse(Map);
        false -> Map
    end,
    Node = #yamerl_map{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres,
      pairs  = Map1
    },
    {finished, Node};

construct_token(_, _, Token) ->
    Error = #yamerl_parsing_error{
      name   = not_a_mapping,
      token  = Token,
      text   = "Invalid mapping",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

construct_node(_,
  #unfinished_node{path = {map, undefined},
    priv = {'$expecting_key', Map}} = Node,
  Key) ->
    Node1 = Node#unfinished_node{
      path = {map, Key},
      priv = {Key, Map}
    },
    {unfinished, Node1, false};
construct_node(_,
  #unfinished_node{path = {map, _},
    priv = {Key, '$expecting_value', Map}} = Node,
  Value) ->
    Map1 = case is_list(Map) of
        true  -> [{Key, Value} | Map];
        false -> maps:put(Key, Value, Map)
    end,
    Node1 = Node#unfinished_node{
      path = {map, undefined},
      priv = Map1
    },
    {unfinished, Node1, false}.

node_pres(Node) ->
    ?NODE_PRES(Node).

node_as_proplist_or_map(#yamerl_constr{ext_options = Options}) ->
    proplists:get_value(map_node_format, Options, proplist).
