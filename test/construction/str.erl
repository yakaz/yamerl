-module(str).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl).

list_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            [$2, $\s, 8364] % "2 €"
          ],
          yamerl_constr:file(?FILENAME, [{detailed_constr, false}])
        )
      ]
    }.

list_detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            {yamerl_doc,
              {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                [{line,1},{column,1}],
                [$2, $\s, 8364]} % "2 €"
            }
          ],
          yamerl_constr:file(?FILENAME, [{detailed_constr, true}])
        )
      ]
    }.

binary_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            <<50,32,226,130,172>> % "2 €"
          ],
          yamerl_constr:file(?FILENAME, [
              {detailed_constr, false},
              str_node_as_binary
            ])
        )
      ]
    }.

binary_detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            {yamerl_doc,
              {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                [{line,1},{column,1}],
                <<50,32,226,130,172>>} % "2 €"
            }
          ],
          yamerl_constr:file(?FILENAME, [
              {detailed_constr, true},
              str_node_as_binary
            ])
        )
      ]
    }.

binary_utf32be_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            <<0,0,0,50, 0,0,0,32, 0,0,32,172>> % "2 €"
          ],
          yamerl_constr:file(?FILENAME, [
              {detailed_constr, false},
              {str_node_as_binary, {utf32, big}}
            ])
        )
      ]
    }.

binary_utf32be_simple_detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            {yamerl_doc,
              {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                [{line,1},{column,1}],
                <<0,0,0,50, 0,0,0,32, 0,0,32,172>>} % "2 €"
            }
          ],
          yamerl_constr:file(?FILENAME, [
              {detailed_constr, true},
              {str_node_as_binary, {utf32, big}}
            ])
        )
      ]
    }.

list_schema_json_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            [$2, $\s, 8364] % "2 €"
          ],
          yamerl_constr:file(?FILENAME, [
              {schema, json},
              {detailed_constr, false}
            ])
        )
      ]
    }.

list_schema_json_detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            {yamerl_doc,
              {yamerl_str,yamerl_node_str_json,"tag:yaml.org,2002:str",
                [{line,1},{column,1}],
                [$2, $\s, 8364]} % "2 €"
            }
          ],
          yamerl_constr:file(?FILENAME, [
              {schema, json},
              {detailed_constr, true}
            ])
        )
      ]
    }.

binary_schema_json_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            <<50,32,226,130,172>> % "2 €"
          ],
          yamerl_constr:file(?FILENAME, [
              {schema, json},
              {detailed_constr, false},
              str_node_as_binary
            ])
        )
      ]
    }.

binary_schema_json_detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            {yamerl_doc,
              {yamerl_str,yamerl_node_str_json,"tag:yaml.org,2002:str",
                [{line,1},{column,1}],
                <<50,32,226,130,172>>} % "2 €"
            }
          ],
          yamerl_constr:file(?FILENAME, [
              {schema, json},
              {detailed_constr, true},
              str_node_as_binary
            ])
        )
      ]
    }.

binary_utf32be_schema_json_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            <<0,0,0,50, 0,0,0,32, 0,0,32,172>> % "2 €"
          ],
          yamerl_constr:file(?FILENAME, [
              {schema, json},
              {detailed_constr, false},
              {str_node_as_binary, {utf32, big}}
            ])
        )
      ]
    }.

binary_utf32be_schema_json_detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            {yamerl_doc,
              {yamerl_str,yamerl_node_str_json,"tag:yaml.org,2002:str",
                [{line,1},{column,1}],
                <<0,0,0,50, 0,0,0,32, 0,0,32,172>>} % "2 €"
            }
          ],
          yamerl_constr:file(?FILENAME, [
              {schema, json},
              {detailed_constr, true},
              {str_node_as_binary, {utf32, big}}
            ])
        )
      ]
    }.
