%-
% Copyright (c) 2012 Yakaz
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

construct_token(_, undefined, #yamerl_collection_start{} = Token) ->
    Pres = yamerl_constr:get_pres_details(Token),
    Node = #unfinished_node{
      path = {map, undefined},
      pres = Pres,
      priv = []
    },
    {unfinished, Node, false};
construct_token(_, #unfinished_node{priv = Pairs} = Node,
  #yamerl_mapping_key{}) ->
    Node1 = Node#unfinished_node{
      priv = [{'$insert_here', undefined} | Pairs]
    },
    {unfinished, Node1, false};
construct_token(_, #unfinished_node{priv = [{Key, undefined} | Pairs]} = Node,
  #yamerl_mapping_value{}) ->
    Node1 = Node#unfinished_node{
      priv = [{Key, '$insert_here'} | Pairs]
    },
    {unfinished, Node1, false};

construct_token(#yamerl_constr{detailed_constr = false},
  #unfinished_node{priv = Pairs}, #yamerl_collection_end{}) ->
    Node = lists:reverse(Pairs),
    {finished, Node};
construct_token(#yamerl_constr{detailed_constr = true},
  #unfinished_node{pres = Pres, priv = Pairs}, #yamerl_collection_end{}) ->
    Node = #yamerl_map{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres,
      pairs  = lists:reverse(Pairs)
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
