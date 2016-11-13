-module('flow_mapping_empty_12').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        4,
        true,
        [],
        0,
        5,
        3,
        1,
        false,
        2,
        2,
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
        7,
        6,
        undefined,
        undefined,
        _,
        false,
        [],
        [
          {yamerl_stream_end,2,2},
          {yamerl_doc_end,2,2},
          {yamerl_collection_end,2,1,flow,mapping},
          {yamerl_collection_start,1,1,
            {yamerl_tag,1,1,{non_specific,"?"}},
            flow,mapping},
          {yamerl_doc_start,1,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
