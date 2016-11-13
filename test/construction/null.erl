-module(null).

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
            ["~", "null", "Null", "NULL", "", "Not a null"]
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
                  flow, plain, "~"},
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
            [null, null, null, null, null, "Not a null"]
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
                    "~"},
                  {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,2},{column,3}],
                    "null"},
                  {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,3},{column,3}],
                    "Null"},
                  {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,4},{column,3}],
                    "NULL"},
                  {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,5},{column,1}],
                    ""},
                  {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,7},{column,3}],
                    "Not a null"}],
                6}}
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
                  flow, plain, "~"},
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
                [{yamerl_null,yamerl_node_null,"tag:yaml.org,2002:null",
                    [{line,1},{column,3}]},
                  {yamerl_null,yamerl_node_null,"tag:yaml.org,2002:null",
                    [{line,2},{column,3}]},
                  {yamerl_null,yamerl_node_null,"tag:yaml.org,2002:null",
                    [{line,3},{column,3}]},
                  {yamerl_null,yamerl_node_null,"tag:yaml.org,2002:null",
                    [{line,4},{column,3}]},
                  {yamerl_null,yamerl_node_null,"tag:yaml.org,2002:null",
                    [{line,5},{column,1}]},
                  {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                    [{line,7},{column,3}],
                    "Not a null"}],
                6}}
          ],
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, true}, {schema, core}])
        )
      ]
    }.
