-module('option_io_blocksize_invalid').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception, [
          {yamerl_invalid_option,error,
            "Invalid value for option \"io_blocksize\": it must be a positive integer, expressed in bytes",
            {io_blocksize,invalid}}
        ]
      },
      yamerl_parser:file(?FILENAME, [{io_blocksize, invalid}])
    ).
