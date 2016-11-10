-module('option_doc_version_too_high').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception,
        [
          {yamerl_parsing_error,error,
            "Version 2.0 not supported (maximum version 1.2)",
            1,1,version_not_supported,
            {yamerl_doc_start,1,1,{2,0},_},
            []}
        ]
      },
      yamerl_parser:file(?FILENAME, [{doc_version, {2,0}}])
    ).
