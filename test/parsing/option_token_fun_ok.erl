-module('option_token_fun_ok').

-include_lib("eunit/include/eunit.hrl").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        string,
        [{token_fun, _}],
        <<>>,
        0,
        true,
        [],
        0,
        1,
        1,
        1,
        false,
        1,
        1,
        utf8,
        false,
        undefined,
        _,
        _,
        [],
        {bcoll,root,0,-1,1,1,-1,1,1},
        false,
        false,
        false,
        [{impl_key,false,false,undefined,undefined,1,1}],
        false,
        false,
        _,
        [],
        0,
        3,
        2,
        undefined,
        undefined,
        _,
        false,
        [],
        []
      },
      yamerl_parser:string(<<>>, [{token_fun, fun(_) -> ok end}])
    ).
