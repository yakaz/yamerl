-module('flow_mapping_empty_2').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception,
        [
          {yamerl_parsing_error,error,
            "Empty flow collection entry not allowed",
            2,1,flow_collection_entry_not_allowed,undefined,[]}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
