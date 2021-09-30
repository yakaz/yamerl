%-
% Copyright (c) 2012-2014 Yakaz
% Copyright (c) 2016-2021 Jean-Sébastien Pédron <jean-sebastien.pedron@dumbbell.fr>
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

-module(yamerl_node_merge).

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

-define(TAG, "tag:yaml.org,2002:merge").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yamerl_collection_start{kind = sequence} = Token) ->
    construct_token(Constr, Node, Token);
try_construct_token(Constr, Node,
  #yamerl_collection_start{kind = mapping} = Token) ->
    construct_token(Constr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(_Constr,
                undefined,
                #yamerl_scalar{tag = #yamerl_tag{uri = "tag:yaml.org,2002:merge"},
                               text = Text}) ->
    {finished, {'$merge', Text}}; 
construct_token(_Constr,
                undefined,
                #yamerl_collection_start{kind = sequence,
                                         tag = #yamerl_tag{uri = "tag:yaml.org,2002:merge"}}) ->
    {finished, '$merge'}; 
construct_token(Constr, undefined, #yamerl_collection_start{kind = mapping} = Token) ->
    {_, Node0, Is_Leaf} = yamerl_node_map:construct_token(Constr, undefined, Token),
    {unfinished, Node0#unfinished_node{module = ?MODULE}, Is_Leaf};
construct_token(Constr, undefined, #yamerl_collection_start{kind = sequence} = Token) ->
    {_, Node0, Is_Leaf} = yamerl_node_seq:construct_token(Constr, undefined, Token),
    {unfinished, Node0#unfinished_node{module = ?MODULE}, Is_Leaf};
construct_token(Constr, #unfinished_node{path = {map, _}} = Node, Token) ->
    yamerl_node_map:construct_token(Constr, Node, Token);
construct_token(Constr, #unfinished_node{path = {seq, _}} = Node, Token) ->
    yamerl_node_seq:construct_token(Constr, Node, Token);

construct_token(_, _, Token) ->
    Error = #yamerl_parsing_error{
      name   = not_a_merge,
      token  = Token,
      text   = "Invalid merge mapping or sequence",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

construct_node(_,
  #unfinished_node{path = {map, _},
    priv = {"<<", '$expecting_value', Map}} = Node,
  Value) ->
    Map1 = case is_list(Map) of
        true  -> Value ++ Map;
        false -> maps:merge(Value, Map)
    end,
    Node1 = Node#unfinished_node{
      path = {map, undefined},
      priv = Map1
    },
    {unfinished, Node1, false};
construct_node(Constr,
  #unfinished_node{path = {map, _},
    priv = {{'$merge', _}, '$expecting_value', Map}} = Node,
  Value) ->
    Priv = {"<<", '$expecting_value', Map},
    construct_node(Constr, Node#unfinished_node{priv = Priv}, Value);
construct_node(_,
  #unfinished_node{path = {seq, Length},
                   priv = ['$insert_here', L | T]} = Node,
  {'$merge', Value}) ->
    Node1 = Node#unfinished_node{
      path = {seq, Length + length(L) - 1},
      priv = [Value | lists:reverse(L) ++ T]
    },
    {unfinished, Node1, false};
construct_node(Constr,
  #unfinished_node{path = {seq, _}} = Node,
  Value) ->
    yamerl_node_seq:construct_node(Constr, Node, Value);
construct_node(Constr,
  #unfinished_node{path = {map, _}} = Node,
  Value) ->
    yamerl_node_map:construct_node(Constr, Node, Value).

node_pres(Node) ->
    ?NODE_PRES(Node).
