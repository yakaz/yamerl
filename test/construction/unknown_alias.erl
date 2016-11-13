-module(unknown_alias).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertThrow(
          {yamerl_exception,
            [{yamerl_parsing_error,error,
                "No anchor corresponds to alias \"anchor\"",
                2,3,no_matching_anchor,
                {yamerl_alias,2,3,"anchor"},
                []}
            ]
          },
          yamerl_constr:file(?FILENAME, [{detailed_constr, false}])
        )
      ]
    }.

detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertThrow(
          {yamerl_exception,
            [{yamerl_parsing_error,error,
                "No anchor corresponds to alias \"anchor\"",
                2,3,no_matching_anchor,
                {yamerl_alias,2,3,"anchor"},
                []}
            ]
          },
          yamerl_constr:file(?FILENAME, [{detailed_constr, true}])
        )
      ]
    }.
