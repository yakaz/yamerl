-module(erlang_atom_if_exist).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl),
    yamerl_app:set_param(node_mods, [yamerl_node_erlang_atom]).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertThrow(
          {yamerl_exception,
            [{yamerl_parsing_error, error,
                "Non-existing Erlang atom",
                1, 27,
                non_existing_erlang_atom,
                {yamerl_scalar, 1, 27,
                  {yamerl_tag, 1, 3, "tag:yamerl,2012:atom"},
                  flow, plain, "inexistent_atom"
                },
                []
              }]
          },
          yamerl_constr:file(?FILENAME, [
              {detailed_constr, false},
              {erlang_atom_only_if_exist, true}
            ])
        )
      ]
    }.

detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertThrow(
          {yamerl_exception,
            [{yamerl_parsing_error, error,
                "Non-existing Erlang atom",
                1, 27,
                non_existing_erlang_atom,
                {yamerl_scalar, 1, 27,
                  {yamerl_tag, 1, 3, "tag:yamerl,2012:atom"},
                  flow, plain, "inexistent_atom"
                },
                []
              }]
          },
          yamerl_constr:file(?FILENAME, [
              {detailed_constr, true},
              {erlang_atom_only_if_exist, true}
            ])
        )
      ]
    }.
