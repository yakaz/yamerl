-module('ex_6.25_invalid_verbatim_tags').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception,
        [
          {yamerl_parsing_error,warning,
            "Invalid character in URI scheme",
            2,3,invalid_uri,{yamerl_tag,2,3,"$:?"},
            []},
          {yamerl_parsing_error,error,
            "Local tag suffix mandatory",
            1,7,invalid_tag_handle,
            {yamerl_tag,1,3,"!"},
            []}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
