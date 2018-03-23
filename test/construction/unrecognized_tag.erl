-module(unrecognized_tag).

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/construction/" ?MODULE_STRING ".yaml").

setup() ->
    application:start(yamerl).

ignore_unrecognized_tag_false_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertThrow(
          {yamerl_exception, [
              {yamerl_parsing_error,error, "Tag \"!unrecognized\" unrecognized by any module", 1, 1, unrecognized_node,
                {yamerl_tag, 1, 1, "!unrecognized"},
                []}]},
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, false}])
        )
      ]
    }.

ignore_unrecognized_tag_true_simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          ["foobar"],
          yamerl_constr:file(?FILENAME,
            [{detailed_constr, false}, {ignore_unrecognized_tags, true}])
        )
      ]
    }.
