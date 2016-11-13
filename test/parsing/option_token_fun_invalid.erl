-module('option_token_fun_invalid').

-include_lib("eunit/include/eunit.hrl").

single_test_() ->
    ?_assertThrow(
      {yamerl_exception, [
          {yamerl_invalid_option,error,
            "Invalid value for option \"token_fun\": it must be a function taking the next token as its sole argument, or the atom 'acc' or 'drop'",
            {token_fun,invalid}}
        ]
      },
      yamerl_parser:string(<<>>, [{token_fun, invalid}])
    ).
