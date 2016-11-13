-module('enc_invalid_unicode').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception, [
          {yamerl_parsing_error,error,
            "Invalid Unicode character at byte #4",
            undefined,undefined,invalid_unicode,undefined,
            [{byte,4}]}
        ]
      },
      yamerl_parser:file(?FILENAME, [{io_blocksize, 1}])
    ).
