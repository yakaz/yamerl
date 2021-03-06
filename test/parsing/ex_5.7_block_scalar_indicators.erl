-module('ex_5.7_block_scalar_indicators').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        49,
        true,
        [],
        0,
        50,
        7,
        1,
        false,
        7,
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
        15,
        14,
        undefined,
        undefined,
        _,
        false,
        [],
        [
          {yamerl_stream_end,7,1},
          {yamerl_doc_end,7,1},
          {yamerl_collection_end,7,1,block,mapping},
          {yamerl_scalar,4,9,
            {yamerl_tag,4,9,{non_specific,"!"}},
            block,folded,"some text\n"},
          {yamerl_mapping_value,4,7},
          {yamerl_scalar,4,1,
            {yamerl_tag,4,1,{non_specific,"?"}},
            flow,plain,"folded"},
          {yamerl_mapping_key,4,1},
          {yamerl_scalar,1,10,
            {yamerl_tag,1,10,{non_specific,"!"}},
            block,literal,"some\ntext\n"},
          {yamerl_mapping_value,1,8},
          {yamerl_scalar,1,1,
            {yamerl_tag,1,1,{non_specific,"?"}},
            flow,plain,"literal"},
          {yamerl_mapping_key,1,1},
          {yamerl_collection_start,1,1,
            {yamerl_tag,1,1,{non_specific,"?"}},
            block,mapping},
          {yamerl_doc_start,1,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
