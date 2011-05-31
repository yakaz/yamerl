-module(yaml_errors).

-include("yaml_errors.hrl").
-include("internal/yaml_parser.hrl").

%% Public API.
-export([
    get_all/1,
    get_errors/1,
    get_warnings/1
  ]).

%% Internal API.
-export([
    format/2,
    format/3,
    throw/1
  ]).

-compile({no_auto_import, [get/1, throw/1]}).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

-spec get_all(yaml_exception()) -> [yaml_error()].

get_all(#yaml_exception{errors = Errors}) ->
    Errors;
get_all(#yaml_parser{errors = Errors}) ->
    Errors.

get_errors(#yaml_exception{errors = Errors}) ->
    filter_errors(Errors, error, []).

get_warnings(#yaml_exception{errors = Errors}) ->
    filter_errors(Errors, warning, []);
get_warnings(#yaml_parser{errors = Errors}) ->
    %% The list won't contain errors, otherwise, an exception would have
    %% been thrown.
    Errors.

filter_errors([Error | Rest], Type, Result) ->
    case ?ERROR_TYPE(Error) of
        Type -> filter_errors(Rest, Type, [Error | Result]);
        _    -> filter_errors(Rest, Type, Result)
    end;
filter_errors([], _, Result) ->
    lists:reverse(Result).

%% -------------------------------------------------------------------
%% Internal API.
%% -------------------------------------------------------------------

format(Error, Text) ->
    erlang:setelement(#yaml_parsing_error.text, Error, Text).

format(Error, Format, Args) ->
    Text = lists:flatten(io_lib:format(Format, Args)),
    format(Error, Text).

throw(Error) when not is_list(Error) ->
    throw([Error]);
throw(Errors) ->
    Exception = #yaml_exception{
      errors = Errors
    },
    erlang:throw(Exception).
