-module(decode).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    application:start(yamerl).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertEqual(
          yamerl:decode("[one, two]"),
          yamerl_constr:string("[one, two]")
        )
      ]
    }.

explicit_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertEqual(
          yamerl:decode("[one, two]", [{detailed_constr, false}]),
          yamerl_constr:string("[one, two]", [{detailed_constr, false}])
        )
      ]
    }.

detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertEqual(
          yamerl:decode("[one, two]", [{detailed_constr, true}]),
          yamerl_constr:string("[one, two]", [{detailed_constr, true}])
        )
      ]
    }.
