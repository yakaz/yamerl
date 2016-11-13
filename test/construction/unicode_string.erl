-module(unicode_string).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl).

schema_failsafe_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            [[12356, 12429, 12399]]
          ],
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, false}, {schema, failsafe}])
        )
      ]
    }.

schema_json_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertThrow(
          {yamerl_exception, [
              {yamerl_parsing_error,error, "Invalid string", 1, 3, not_a_string,
                {yamerl_scalar, 1, 3,
                  {yamerl_tag, 1, 3, {non_specific, "?"}},
                  flow, plain, [12356, 12429, 12399]},
                []}]},
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, false}, {schema, json}])
        )
      ]
    }.

schema_core_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            [[12356, 12429, 12399]]
          ],
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, false}, {schema, core}])
        )
      ]
    }.

schema_failsafe_detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            {yamerl_doc,
              {yamerl_seq,yamerl_node_seq,"tag:yaml.org,2002:seq",
                [{line,1},{column,1}],
                [{yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,1},{column,3}],
                    [12356, 12429, 12399]}],
                1}}
          ],
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, true}, {schema, failsafe}])
        )
      ]
    }.

schema_json_detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertThrow(
          {yamerl_exception, [
              {yamerl_parsing_error,error, "Invalid string", 1, 3, not_a_string,
                {yamerl_scalar, 1, 3,
                  {yamerl_tag, 1, 3, {non_specific, "?"}},
                  flow, plain, [12356, 12429, 12399]},
                []}]},
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, true}, {schema, json}])
        )
      ]
    }.

schema_core_detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            {yamerl_doc,
              {yamerl_seq,yamerl_node_seq,"tag:yaml.org,2002:seq",
                [{line,1},{column,1}],
                [{yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,1},{column,3}],
                    [12356, 12429, 12399]}],
                1}}
          ],
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, true}, {schema, core}])
        )
      ]
    }.
