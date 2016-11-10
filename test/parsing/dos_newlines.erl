-module('dos_newlines').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [{io_blocksize, 1}],
        <<>>,
        70,
        true,
        [],
        0,
        71,
        7,
        1,
        false,
        6,
        10,
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
        11,
        10,
        undefined,
        undefined,
        _,
        false,
        [],
        [
          {yamerl_stream_end,6,10},
          {yamerl_doc_end,6,10},
          {yamerl_collection_end,6,10,block,sequence},
          {yamerl_scalar,5,3,
            {yamerl_tag,5,3,{non_specific,"!"}},
            flow,double_quoted,"Hello World"},
          {yamerl_sequence_entry,5,1},
          {yamerl_scalar,2,3,
            {yamerl_tag,2,3,{non_specific,"!"}},
            block,folded,"Hello World\n"},
          {yamerl_sequence_entry,2,1},
          {yamerl_collection_start,2,1,
            {yamerl_tag,2,1,{non_specific,"?"}},
            block,sequence},
          {yamerl_doc_start,2,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME, [{io_blocksize, 1}])
    ).
