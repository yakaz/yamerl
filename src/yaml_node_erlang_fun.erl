-module(yaml_node_erlang_fun).

-include("yaml_parser.hrl").
-include("yaml_tokens.hrl").
-include("yaml_repr.hrl").
-include("yaml_nodes.hrl").

%% Public API.
-export([
    tags/0,
    matches/2,
    represent_token/3
  ]).

-define(TAG, "tag:erlang.org,2011:fun").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

matches(_, _) -> false.

represent_token(#yaml_repr{simple_structs = Simple},
  undefined, #yaml_scalar{text = Text} = Token) ->
    case erl_scan:string(Text) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Exprs} ->
                    {value, Fun, _} = erl_eval:exprs(Exprs, []),
                    Node = if
                        Simple ->
                            Fun;
                        true ->
                            #yaml_erlang_fun{
                              module   = ?MODULE,
                              tag      = ?TAG,
                              function = Fun,
                              text     = Text
                            }
                    end,
                    {finished, Node};
                {error, {Line, _, Desc}} ->
                    Error = #yaml_parser_error{
                      name   = invalid_erlang_fun2,
                      token  = Token,
                      text   = lists:flatten(erl_parse:format_error(Desc)),
                      line   = ?TOKEN_LINE(Token) + Line - 1,
                      column = 0
                    },
                    throw(Error)
            end;
        {error, {Line, _, Desc}, _} ->
            Error = #yaml_parser_error{
              name   = invalid_erlang_fun1,
              token  = Token,
              text   = lists:flatten(erl_scan:format_error(Desc)),
              line   = ?TOKEN_LINE(Token) + Line - 1,
              column = 0
            },
            throw(Error)
    end.
