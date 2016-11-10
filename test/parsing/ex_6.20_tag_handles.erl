-module('ex_6.20_tag_handles').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        52,
        true,
        [],
        0,
        53,
        4,
        1,
        false,
        3,
        13,
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
        8,
        6,
        undefined,
        undefined,
        _,
        false,
        [],
        [
          {yamerl_stream_end,3,13},
          {yamerl_doc_end,3,13},
          {yamerl_scalar,3,8,
            {yamerl_tag,3,1,"tag:example.com,2000:app/foo"},
            flow,double_quoted,"bar"},
          {yamerl_doc_start,2,1,{1,2},_},
          {yamerl_tag_directive,1,1,"!e!","tag:example.com,2000:app/"},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
