-module('unfinished_flow_collection').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception, [
          {yamerl_parsing_error,error,
            "Unfinished flow collection",
            1,6,unfinished_flow_collection,
            {yamerl_doc_end,1,6},
            []}]},
      yamerl_parser:file(?FILENAME)
    ).
