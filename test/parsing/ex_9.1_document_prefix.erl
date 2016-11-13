-module('ex_9.1_document_prefix').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        30,
        true,
        [],
        0,
        28,
        4,
        1,
        false,
        3,
        9,
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
        6,
        5,
        undefined,
        undefined,
        _,
        false,
        [],
        [
          {yamerl_stream_end,3,9},
          {yamerl_doc_end,3,9},
          {yamerl_scalar,3,1,
            {yamerl_tag,3,1,{non_specific,"?"}},
            flow,plain,"Document"},
          {yamerl_doc_start,3,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
