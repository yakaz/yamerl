-module(bool_json).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            [
              true,
              false
            ]
          ],
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, false}, {schema, json}])
        )
      ]
    }.

detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            {yamerl_doc,
              {yamerl_seq,yamerl_node_seq,"tag:yaml.org,2002:seq",
                [{line,1},{column,1}],
                [{yamerl_bool,yamerl_node_bool_json,
                    "tag:yaml.org,2002:bool",
                    [{line,1},{column,3}],
                    true},
                  {yamerl_bool,yamerl_node_bool_json,
                    "tag:yaml.org,2002:bool",
                    [{line,2},{column,3}],
                    false}],
                2}
            }
          ],
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, true}, {schema, json}])
        )
      ]
    }.
