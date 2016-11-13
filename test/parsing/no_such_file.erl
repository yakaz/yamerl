-module('no_such_file').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception,
        [
          {yamerl_parsing_error,error,
            _,
            undefined,undefined,file_open_failure,undefined,[{error,enoent}]}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
