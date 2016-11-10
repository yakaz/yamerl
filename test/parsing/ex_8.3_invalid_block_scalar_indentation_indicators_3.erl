-module('ex_8.3_invalid_block_scalar_indentation_indicators_3').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception,
        [
          {yamerl_parsing_error,error,
            "Invalid block scalar indentation",
            2,2,invalid_block_scalar_indentation,
            {yamerl_scalar,1,3,{yamerl_tag,1,3,{non_specific,"!"}},block,literal,[]},
            []}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
