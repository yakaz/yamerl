-module('ex_6.27_invalid_tag_shorthands').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception,
        [
          {yamerl_parsing_error,error,
            "Tag handle \"!h!\" never declared",
            4,9,undeclared_tag_handle,{yamerl_tag,4,3,"!h!bar"},
            []},
          {yamerl_parsing_error,error,
            "Tag suffix mandatory",
            3,6,invalid_tag_handle,{yamerl_tag,3,3,"!e!"},
            []}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
