-module(yaml_node_int).

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
    string_to_integer/1
  ]).

-define(TAG, "tag:yaml.org,2002:int").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

try_construct_token(Repr, Node,
  #yaml_scalar{tag = #yaml_tag{uri = {non_specific, "?"}}} = Token) ->
    try
        construct_token(Repr, Node, Token)
    catch
        _:#yaml_parsing_error{name = not_an_integer} ->
            unrecognized
    end;
try_construct_token(_, _, _) ->
    unrecognized.

construct_token(#yaml_constr{simple_structs = true},
  undefined, #yaml_scalar{text = Text} = Token) ->
    case string_to_integer(Text) of
        error ->
            exception(Token);
        Int ->
            {finished, Int}
    end;
construct_token(#yaml_constr{simple_structs = false},
  undefined, #yaml_scalar{text = Text} = Token) ->
    case string_to_integer(Text) of
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

%% Sign.
string_to_integer([$+ | Text]) -> string_to_integer2(Text);
string_to_integer([$- | Text]) -> -string_to_integer2(Text);
string_to_integer(Text)        -> string_to_integer2(Text).

%% Base.
string_to_integer2("0b" ++ Text) ->
    base2_to_integer(Text, 0);
string_to_integer2("0x" ++ Text) ->
    base16_to_integer(Text, 0);
string_to_integer2("0o" ++ Text) ->
    base8_to_integer(Text, 0);
string_to_integer2("0" ++ Text) ->
    base8_to_integer(Text, 0);
string_to_integer2(Text) ->
    case re:run(Text, "[1-9][0-9_]*(?::[0-5]?[0-9])+") of
        {match, _} -> base60_to_integer(Text, 0, 0);
        nomatch    -> base10_to_integer(Text, 0)
    end.

%% Parsing.
base10_to_integer([C | Rest], Int) when C >= $0 andalso C =< $9 ->
    Int1 = (Int * 10) + (C - $0),
    base10_to_integer(Rest, Int1);
base10_to_integer([$_ | Rest], Int) ->
    base10_to_integer(Rest, Int);
base10_to_integer([], Int) ->
    Int;
base10_to_integer(_, _) ->
    error.

base2_to_integer([C | Rest], Int) when C == $0 orelse C == $1 ->
    Int1 = (Int * 2) + (C - $0),
    base2_to_integer(Rest, Int1);
base2_to_integer([$_ | Rest], Int) ->
    base2_to_integer(Rest, Int);
base2_to_integer([], Int) ->
    Int;
base2_to_integer(_, _) ->
    error.

base8_to_integer([C | Rest], Int) when C >= $0 andalso C =< $7 ->
    Int1 = (Int * 8) + (C - $0),
    base8_to_integer(Rest, Int1);
base8_to_integer([$_ | Rest], Int) ->
    base8_to_integer(Rest, Int);
base8_to_integer([], Int) ->
    Int;
base8_to_integer(_, _) ->
    error.

base16_to_integer([C | Rest], Int) when C >= $0 andalso C =< $9 ->
    Int1 = (Int * 16) + (C - $0),
    base16_to_integer(Rest, Int1);
base16_to_integer([C | Rest], Int) when C >= $a andalso C =< $f ->
    Int1 = (Int * 16) + (C - $a + 10),
    base16_to_integer(Rest, Int1);
base16_to_integer([C | Rest], Int) when C >= $A andalso C =< $F ->
    Int1 = (Int * 16) + (C - $A + 10),
    base16_to_integer(Rest, Int1);
base16_to_integer([$_ | Rest], Int) ->
    base16_to_integer(Rest, Int);
base16_to_integer([], Int) ->
    Int;
base16_to_integer(_, _) ->
    error.

base60_to_integer([C | Rest], Current, Int) when C >= $0 andalso C =< $9 ->
    Current1 = (Current * 10) + (C - $0),
    base60_to_integer(Rest, Current1, Int);
base60_to_integer([$: | Rest], Current, Int) ->
    Int1 = (Int * 60) + Current,
    base60_to_integer(Rest, 0, Int1);
base60_to_integer([$_ | Rest], Current, Int) ->
    base60_to_integer(Rest, Current, Int);
base60_to_integer([], Current, Int) ->
    (Int * 60) + Current.

exception(Token) ->
    Error = #yaml_parsing_error{
      name   = not_an_integer,
      token  = Token,
      text   = "Invalid integer",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).
