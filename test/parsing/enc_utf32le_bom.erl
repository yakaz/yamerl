-module('enc_utf32le_bom').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [{io_blocksize, 1}],
        <<>>,
        104,
        true,
        [],
        0,
        26,
        2,
        1,
        false,
        1,
        25,
        {utf32,little},
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
        [
          {yamerl_stream_end,1,25},
          {yamerl_stream_start,1,1,{utf32,little}}
        ]
      },
      yamerl_parser:file(?FILENAME, [{io_blocksize, 1}])
    ).
