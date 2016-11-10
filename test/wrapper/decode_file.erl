-module(decode_file).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/wrapper/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertEqual(
          yamerl:decode_file(?FILENAME),
          yamerl_constr:file(?FILENAME)
        )
      ]
    }.

explicit_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertEqual(
          yamerl:decode_file(?FILENAME, [{detailed_constr, false}]),
          yamerl_constr:file(?FILENAME, [{detailed_constr, false}])
        )
      ]
    }.

detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertEqual(
          yamerl:decode_file(?FILENAME, [{detailed_constr, true}]),
          yamerl_constr:file(?FILENAME, [{detailed_constr, true}])
        )
      ]
    }.
