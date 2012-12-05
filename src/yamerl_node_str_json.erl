-module(yamerl_node_str_json).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    node_pres/1
  ]).

-define(TAG, "tag:yaml.org,2002:str").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node, #yamerl_scalar{} = Token) ->
    construct_token(Constr, Node, Token);
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yamerl_constr{simple_structs = true, ext_options = Options},
  undefined, #yamerl_scalar{text = Text, tag = #yamerl_tag{uri = Uri}})
when Uri /= {non_specific, "?"} ->
    Node = case proplists:get_value(str_node_as_binary, Options, false) of
        false    -> Text;
        true     -> unicode:characters_to_binary(Text);
        Encoding -> unicode:characters_to_binary(Text, unicode, Encoding)
    end,
    {finished, Node};
construct_token(#yamerl_constr{simple_structs = false, ext_options = Options},
  undefined, #yamerl_scalar{text = Text, tag = #yamerl_tag{uri = Uri}} = Token)
when Uri /= {non_specific, "?"} ->
    Text1 = case proplists:get_value(str_node_as_binary, Options, false) of
        false    -> Text;
        true     -> unicode:characters_to_binary(Text);
        Encoding -> unicode:characters_to_binary(Text, unicode, Encoding)
    end,
    Pres = yamerl_constr:get_pres_details(Token),
    Node = #yamerl_str{
      module = ?MODULE,
      tag    = ?TAG,
      pres   = Pres,
      text   = Text1
    },
    {finished, Node};

construct_token(_, _, Token) ->
    Error = #yamerl_parsing_error{
      name   = not_a_string,
      token  = Token,
      text   = "Invalid string",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

node_pres(Node) ->
    ?NODE_PRES(Node).
