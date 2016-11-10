-module(null_json).

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
            [null]
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
                [{yamerl_null,yamerl_node_null_json,
                    "tag:yaml.org,2002:null",
                    [{line,1},{column,3}]}],
                1}}
          ],
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, true}, {schema, json}])
        )
      ]
    }.
