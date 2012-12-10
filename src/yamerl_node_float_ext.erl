-module(yamerl_node_float_ext).

-include("yamerl_errors.hrl").
-include("yamerl_tokens.hrl").
-include("yamerl_nodes.hrl").
-include("internal/yamerl_constr.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    node_pres/1,
    string_to_float/1
  ]).

-define(TAG, "tag:yaml.org,2002:float").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yamerl_scalar{tag = #yamerl_tag{uri = {non_specific, "?"}}} = Token) ->
    try
        construct_token(Constr, Node, Token)
    catch
        _:#yamerl_parsing_error{name = not_a_float} ->
            unrecognized
    end;
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yamerl_constr{detailed_constr = false, ext_options = Options},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case string_to_float(Text) of
        error ->
            exception(Token);
        Int ->
            Inf_As_Yamler = proplists:get_bool(inf_float_node_like_yamler,
              Options),
            Int1 = case Inf_As_Yamler of
                false ->
                    Int;
                true ->
                    case Int of
                        '+inf' -> inf;
                        '-inf' -> ninf;
                        _      -> Int
                    end
            end,
            {finished, Int1}
    end;
construct_token(#yamerl_constr{detailed_constr = true, ext_options = Options},
  undefined, #yamerl_scalar{text = Text} = Token) ->
    case string_to_float(Text) of
        error ->
            exception(Token);
        Int ->
            Inf_As_Yamler = proplists:get_bool(inf_float_node_like_yamler,
              Options),
            Int1 = case Inf_As_Yamler of
                false ->
                    Int;
                true ->
                    case Int of
                        '+inf' -> inf;
                        '-inf' -> ninf;
                        _      -> Int
                    end
            end,
            Pres = yamerl_constr:get_pres_details(Token),
            Node = #yamerl_float{
              module = ?MODULE,
              tag    = ?TAG,
              pres   = Pres,
              value  = Int1
            },
            {finished, Node}
    end;

construct_token(_, _, Token) ->
    exception(Token).

node_pres(Node) ->
    ?NODE_PRES(Node).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

string_to_float(".nan")      -> 'nan';
string_to_float(".NaN")      -> 'nan';
string_to_float(".NAN")      -> 'nan';
string_to_float(".inf")      -> '+inf';
string_to_float(".Inf")      -> '+inf';
string_to_float(".INF")      -> '+inf';
string_to_float("+.inf")     -> '+inf';
string_to_float("+.Inf")     -> '+inf';
string_to_float("+.INF")     -> '+inf';
string_to_float("-.inf")     -> '-inf';
string_to_float("-.Inf")     -> '-inf';
string_to_float("-.INF")     -> '-inf';

string_to_float([$+ | Text]) ->
    string_to_float2(Text);
string_to_float([$- | Text]) ->
    case string_to_float2(Text) of
        error -> error;
        Float -> -Float
    end;
string_to_float(Text) ->
    string_to_float2(Text).

%% The expressions given in the "Canonical:" and "Regexp:" sections in
%% the "float" type specification [1] from the type repository have some
%% inconsistencies:
%%     o  "0" is accepted by "Canonical:", but not by "Regexp:".
%%     o  Multiple "." are accepted by the base-10 expression in
%%        "Regexp:". It probably should have been "_" inside the square
%%        brackets.
%%     o  base-10 and base-60 decimal parts don't have the same format
%%        (base-60 accepts an "_" as the first character).
%%     o  A "." whole alone is accepted.
%%
%% This module tries to implement a sensible mix of the specified expressions:
%%     o  "0" is accepted, but not any other integer.
%%     o  Multiple "." are denied.
%%     o  "_" are accepted after ".".
%%     o  base-10 and base-60 supports the same decimal format (ie. the
%%        first character after "." must be a digit).
%%     o  "." alone is denied.
%%
%% [1] http://yaml.org/type/float.html

string_to_float2("0") ->
    %% "0" is the only integer accepted by the "Canonical:" section.
    0.0;
string_to_float2(".") ->
    error;
string_to_float2(Text) ->
    %% Try base 10.
    Opts1 = [{capture, none}],
    Ret1 = re:run(Text,
      "^([0-9][0-9_]*)?\\.([0-9][0-9_]*)?([eE][-+][0-9]+)?$",
      Opts1),
    case Ret1 of
        match ->
            yamerl_node_float:erlang_list_to_float(Text);
        nomatch ->
            %% Try base 60.
            Opts2 = [{capture, all_but_first, list}],
            Ret2 = re:run(Text,
              "^((?:[0-9][0-9_]*)(?::[0-5]?[0-9])+)\\.([0-9][0-9_]*)?$",
              Opts2),
            case Ret2 of
                {match, [Base60, Dec]} ->
                    case yamerl_node_int_ext:base60_to_integer(Base60, 0, 0) of
                        error ->
                            error;
                        Int ->
                            yamerl_node_float:erlang_list_to_float(
                              erlang:integer_to_list(Int) ++ "." ++ Dec)
                    end;
                nomatch ->
                    error
            end
    end.

exception(Token) ->
    Error = #yamerl_parsing_error{
      name   = not_a_float,
      token  = Token,
      text   = "Invalid float",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).
