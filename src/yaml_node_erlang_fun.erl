-module(yaml_node_erlang_fun).

-include("yaml_parser.hrl").
-include("yaml_tokens.hrl").
-include("yaml_repr.hrl").
-include("yaml_nodes.hrl").

%% Public API.
-export([
    tags/0,
    represent_token/3,
    node_pres/1
  ]).

-define(TAG, "tag:erlang.org,2011:fun").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

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
                            Pres = yaml_repr:get_pres_details(Token),
                            #yaml_erlang_fun{
                              module   = ?MODULE,
                              tag      = ?TAG,
                              pres     = Pres,
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
    end;
represent_token(_, _, Token) ->
    Error = #yaml_parser_error{
      name   = not_an_erlang_fun,
      token  = Token,
      text   = "Invalid Erlang anonymous function",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

node_pres(Node) ->
    ?NODE_PRES(Node).
