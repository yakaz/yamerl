-module('option_doc_version_invalid').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception,
        [
          {yamerl_invalid_option,error,
            "Invalid value for option \"doc_version\": it must be a tuple of the form {Major, Minor} where Major and Minor are positive integers",
            {doc_version,invalid}}
        ]
      },
      yamerl_parser:file(?FILENAME, [{doc_version, invalid}])
    ).
