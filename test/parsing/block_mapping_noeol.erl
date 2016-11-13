-module('block_mapping_noeol').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [{io_blocksize, 1}],
        <<>>,
        24,
        true,
        [],
        0,
        25,
        2,
        5,
        false,
        2,
        5,
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
        11,
        10,
        undefined,
        undefined,
        _,
        false,
        [],
        [
          {yamerl_stream_end,2,5},
          {yamerl_doc_end,2,5},
          {yamerl_collection_end,2,5,block,mapping},
          {yamerl_scalar,2,4,
            {yamerl_tag,2,4,{non_specific,"?"}},
            flow,plain,[]},
          {yamerl_mapping_value,2,4},
          {yamerl_scalar,2,1,
            {yamerl_tag,2,1,{non_specific,"?"}},
            flow,plain,"key"},
          {yamerl_mapping_key,2,1},
          {yamerl_collection_start,2,1,
            {yamerl_tag,2,1,{non_specific,"?"}},
            block,mapping},
          {yamerl_doc_start,2,1,{1,2}, _},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME, [{io_blocksize, 1}])
    ).
