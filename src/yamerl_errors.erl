-module(yamerl_errors).

-include("yamerl_errors.hrl").

%% Internal API.
-export([
    format/2,
    format/3,
    throw/1
  ]).

-compile({no_auto_import, [get/1, throw/1]}).

%% -------------------------------------------------------------------
%% Internal API.
%% -------------------------------------------------------------------

format(Error, Text) ->
    erlang:setelement(#yamerl_parsing_error.text, Error, Text).

format(Error, Format, Args) ->
    Text = lists:flatten(io_lib:format(Format, Args)),
    format(Error, Text).

throw(Error) when not is_list(Error) ->
    throw([Error]);
throw(Errors) ->
    Exception = #yamerl_exception{
      errors = Errors
    },
    erlang:throw(Exception).
