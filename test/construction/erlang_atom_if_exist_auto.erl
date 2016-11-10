-module(erlang_atom_if_exist_auto).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl),
    yamerl_app:set_param(node_mods, [yamerl_node_erlang_atom]).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [
            "inexistent_atom"
          ],
          yamerl_constr:file(?FILENAME, [
              {detailed_constr, false},
              {erlang_atom_autodetection, true},
              {erlang_atom_only_if_exist, true}
            ])
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
              {yamerl_str,yamerl_node_str,"tag:yaml.org,2002:str",
                [{line,1},{column,1}],
                "inexistent_atom"}
            }
          ],
          yamerl_constr:file(?FILENAME, [
              {detailed_constr, true},
              {erlang_atom_autodetection, true},
              {erlang_atom_only_if_exist, true}
            ])
        )
      ]
    }.
