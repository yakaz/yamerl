-module('block_scalar_followed_by_end_of_doc').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
       {yamerl_parser,
        {file, ?FILENAME},
        [],
        <<>>,
        41,
        true,
        [],
        0,
        42,
        7,
        1,
        false,
        6,
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
        [{impl_key,false,false,undefined,undefined,1,1}],
        false,
        false,
        _,
        [],
        0,
        9,
        8,
        undefined,
        undefined,
        _,
        false,
        [],
        [
         {yamerl_stream_end,6,13},
         {yamerl_doc_end,6,13},
         {yamerl_scalar,6,1,
          {yamerl_tag,6,1,{non_specific,"?"}},
          flow,plain,"new document"},
         {yamerl_doc_start,5,1,{1,2}, _},
         {yamerl_doc_end,5,1},
         {yamerl_scalar,1,5,
          {yamerl_tag,1,5,{non_specific,"!"}},
          block,literal,"foo\nbar\nbaz\n"},
         {yamerl_doc_start,1,1,{1,2}, _},
         {yamerl_stream_start,1,1,utf8}
        ]
       },
      yamerl_parser:file(?FILENAME)
    ).
