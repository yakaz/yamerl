-module('ex_8.8_literal_content').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        43,
        true,
        [],
        0,
        44,
        10,
        1,
        false,
        9,
        11,
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
        [{impl_key,false,undefined,undefined,undefined,undefined,undefined}],
        false,
        false,
        _,
        [],
        0,
        6,
        5,
        undefined,
        undefined,
        _,
        false,
        [],
        [
          {yamerl_stream_end,9,11},
          {yamerl_doc_end,9,11},
          {yamerl_scalar,1,1,
            {yamerl_tag,1,1,{non_specific,"!"}},
            block,literal,"\n\nliteral\n \n\ntext\n"},
          {yamerl_doc_start,1,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
