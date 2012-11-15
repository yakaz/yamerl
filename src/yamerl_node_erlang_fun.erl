-module(yamerl_node_erlang_fun).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

%% Public API.
-export([
    tags/0,
    construct_token/3,
    node_pres/1
  ]).

-define(TAG, "tag:erlang.org,2011:fun").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

construct_token(#yamerl_constr{simple_structs = Simple},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case erl_scan:string(Text) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Exprs} ->
                    {value, Fun, _} = erl_eval:exprs(Exprs, []),
                    Node = if
                        Simple ->
                            Fun;
                        true ->
                            Pres = yamerl_constr:get_pres_details(Token),
                            #yamerl_erlang_fun{
                              module   = ?MODULE,
                              tag      = ?TAG,
                              pres     = Pres,
                              function = Fun,
                              text     = Text
                            }
                    end,
                    {finished, Node};
                {error, {Line, _, Desc}} when Line >= 1 ->
                    Error = #yamerl_parsing_error{
                      name   = invalid_erlang_fun2,
                      token  = Token,
                      text   = lists:flatten(erl_parse:format_error(Desc)),
                      line   = ?TOKEN_LINE(Token) + Line - 1
                    },
                    throw(Error)
            end;
        {error, {Line, _, Desc}, _} when Line >= 1 ->
            Error = #yamerl_parsing_error{
              name   = invalid_erlang_fun1,
              token  = Token,
              text   = lists:flatten(erl_scan:format_error(Desc)),
              line   = ?TOKEN_LINE(Token) + Line - 1
            },
            throw(Error)
    end;
construct_token(_, _, Token) ->
    Error = #yamerl_parsing_error{
      name   = not_an_erlang_fun,
      token  = Token,
      text   = "Invalid Erlang anonymous function",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

node_pres(Node) ->
    ?NODE_PRES(Node).
