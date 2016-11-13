-module(empty).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [],
          yamerl_constr:file(?FILENAME, [{detailed_constr, false}])
        )
      ]
    }.

detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [],
          yamerl_constr:file(?FILENAME, [{detailed_constr, true}])
        )
      ]
    }.
