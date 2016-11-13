-module(erlang_fun).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl),
    yamerl_app:set_param(node_mods, [yamerl_node_erlang_fun]).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        {generator,
          fun() ->
              [Erlang_Fun_Simple] = yamerl_constr:file(?FILENAME,
                [{detailed_constr, false}]),
              ?_assertMatch(
                "Hello World!",
                Erlang_Fun_Simple()
              )
          end}
      ]
    }.

detailed_test_() ->
    {setup,
      fun setup/0,
      [
        {generator,
          fun() ->
              [{yamerl_doc,
                  {yamerl_erlang_fun,yamerl_node_erlang_fun,
                    "tag:yamerl,2012:fun",
                    [{line,1},{column,24}],
                    Erlang_Fun_Normal,
                    "fun() -> \"Hello World!\" end.\n"}}
              ] = yamerl_constr:file(?FILENAME,
                [{detailed_constr, true}]),
              ?_assertMatch(
                "Hello World!",
                Erlang_Fun_Normal()
              )
          end}
      ]
    }.
