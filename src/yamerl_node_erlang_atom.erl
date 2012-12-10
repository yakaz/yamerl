-module(yamerl_node_erlang_atom).

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

-define(TAG, "tag:yamerl,2012:atom").

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

tags() -> [?TAG].

%% Atom auto-detection:
%%   o  double-quoted flow scalar: not an atom
%%   o  single-quoted flow scalar: an atom
%%   o  plain flow scalar:         an atom IF acceptable characters
%%   o  block scalar:              an atom IF acceptable characters or
%%                                         IF single-quoted

try_construct_token(#yamerl_constr{ext_options = Options} = Constr, undefined,
  #yamerl_scalar{tag = #yamerl_tag{uri = {non_specific, "?"}}} = Token) ->
    case proplists:get_bool(erlang_atom_autodetection, Options) of
        true  -> try_construct_token2(Constr, Token);
        false -> unrecognized
    end;
try_construct_token(_, _, _) ->
    unrecognized.

try_construct_token2(Constr,
  #yamerl_scalar{substyle = plain, text = Text} = Token) ->
    %% Check characters to see if it's valid atom.
    case is_valid_atom(Text) of
        true  -> try_construct_token3(Constr, Token, Text);
        false -> unrecognized
    end.

try_construct_token3(#yamerl_constr{ext_options = Options} = Constr,
  Token, Text) ->
    case proplists:get_bool(erlang_atom_only_if_exist, Options) of
        false ->
            Atom = list_to_atom(Text),
            construct_token2(Constr, Token, Atom);
        true ->
            try
                Atom = list_to_existing_atom(Text),
                construct_token2(Constr, Token, Atom)
            catch
                error:badarg ->
                    unrecognized
            end
    end.

is_valid_atom(Text) ->
    Opts = [{capture, none}],
    case re:run(Text, "^[a-z][a-zA-Z0-9_@]*$", Opts) of
        match   -> true;
        nomatch -> false
    end.

construct_token(#yamerl_constr{ext_options = Options} = Constr, undefined,
  #yamerl_scalar{text = Text} = Token) ->
    case proplists:get_bool(erlang_atom_only_if_exist, Options) of
        false ->
            Atom = list_to_atom(Text),
            construct_token2(Constr, Token, Atom);
        true ->
            try
                Atom = list_to_existing_atom(Text),
                construct_token2(Constr, Token, Atom)
            catch
                error:badarg ->
                    Error = #yamerl_parsing_error{
                      name   = non_existing_erlang_atom,
                      token  = Token,
                      text   = "Non-existing Erlang atom",
                      line   = ?TOKEN_LINE(Token),
                      column = ?TOKEN_COLUMN(Token)
                    },
                    throw(Error)
            end
    end;
construct_token(_, _, Token) ->
    Error = #yamerl_parsing_error{
      name   = not_an_erlang_atom,
      token  = Token,
      text   = "Invalid Erlang atom",
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    throw(Error).

construct_token2(#yamerl_constr{detailed_constr = false}, _, Atom) ->
    {finished, Atom};
construct_token2(#yamerl_constr{detailed_constr = true}, Token, Atom) ->
    Pres = yamerl_constr:get_pres_details(Token),
    Node = #yamerl_erlang_atom{
      module   = ?MODULE,
      tag      = ?TAG,
      pres     = Pres,
      name     = Atom
    },
    {finished, Node}.

node_pres(Node) ->
    ?NODE_PRES(Node).
