-module(alias).

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
              "Duplicate",
              "Duplicate"
            ]
          ],
          yamerl_constr:file(?FILENAME, [{detailed_constr, false}])
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
              {yamerl_seq,yamerl_node_seq, "tag:yaml.org,2002:seq",
                [{line,1},{column,1}],
                [
                  {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,1},{column,11}],
                    "Duplicate"},
                  {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,1},{column,11}],
                    "Duplicate"}
                ],
                2}
            }
          ],
          yamerl_constr:file(?FILENAME, [{detailed_constr, true}])
        )
      ]
    }.
