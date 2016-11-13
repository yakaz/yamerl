-module('bom_between_docs').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [{io_blocksize, 1}],
        <<>>,
        55,
        true,
        [],
        0,
        51,
        6,
        1,
        false,
        5,
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
          {yamerl_stream_end,5,11},
          {yamerl_doc_end,5,11},
          {yamerl_scalar,5,1,
            {yamerl_tag,5,1,{non_specific,"?"}},
            flow,plain,"Document 2"},
          {yamerl_doc_start,5,1,{1,2},_},
          {yamerl_doc_end,3,1},
          {yamerl_scalar,2,1,
            {yamerl_tag,2,1,{non_specific,"?"}},
            flow,plain,"Document 1"},
          {yamerl_doc_start,2,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME, [{io_blocksize, 1}])
    ).
