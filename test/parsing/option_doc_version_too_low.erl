-module('option_doc_version_too_low').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception,
        [
          {yamerl_parsing_error,error,
            "Version 0.9 not supported (minimum version 1.1)",
            1,1,version_not_supported,
            {yamerl_doc_start,1,1,{0,9},_},
            []}
        ]
      },
      yamerl_parser:file(?FILENAME, [{doc_version, {0,9}}])
    ).
