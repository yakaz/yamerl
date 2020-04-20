-module(document_marker_only).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          [null],
          yamerl_constr:string("---", [{detailed_constr, false}])
        )
      ]
    }.

detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
           [{yamerl_doc,
             {yamerl_null, yamerl_node_null, "tag:yaml.org,2002:null",
              [{line, 1}, {column, 4}]}}],
          yamerl_constr:string("---", [{detailed_constr, true}])
        )
      ]
    }.
