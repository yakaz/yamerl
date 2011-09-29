-module(yaml_node_float).

-include("yaml_errors.hrl").
-include("yaml_tokens.hrl").
-include("yaml_nodes.hrl").
-include("internal/yaml_constr.hrl").

%% Public API.
-export([
    tags/0,
    try_construct_token/3,
    construct_token/3,
    node_pres/1,
    string_to_float/1,
    erlang_list_to_float/1
  ]).

-define(TAG, "tag:yaml.org,2002:float").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Constr, Node,
  #yaml_scalar{tag = #yaml_tag{uri = {non_specific, "?"}}} = Token) ->
    try
        construct_token(Constr, Node, Token)
    catch
        _:#yaml_parsing_error{name = not_a_float} ->
            unrecognized
    end;
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yaml_constr{simple_structs = true},
  undefined, #yaml_scalar{text = Text} = Token) ->
    case string_to_float(Text) of
        error ->
            exception(Token);
        Int ->
            {finished, Int}
    end;
construct_token(#yaml_constr{simple_structs = false},
  undefined, #yaml_scalar{text = Text} = Token) ->
    case string_to_float(Text) of
        error ->
            exception(Token);
        Int ->
            Pres = yaml_constr:get_pres_details(Token),
            Node = #yaml_int{
              module = ?MODULE,
              tag    = ?TAG,
              pres   = Pres,
              value  = Int
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
string_to_float(".Nan")      -> 'nan';
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

string_to_float2(Text) ->
    Opts = [{capture, none}],
    case re:run(Text, "(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?", Opts) of
        match   -> string_to_float3(Text);
        nomatch -> error
    end.

erlang_list_to_float(Text) ->
    try
        Text1 = re:replace(Text, "^([-+]?[0-9]+)([eE]|$)", "\\1.0\\2",
          [{return, list}]),
        erlang:list_to_float(Text1)
    catch
        error:badarg -> error
    end.

exception(Token) ->
    Error = #yaml_parsing_error{
      name   = not_a_float,
      token  = Token,
      text   = "Invalid float",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).
