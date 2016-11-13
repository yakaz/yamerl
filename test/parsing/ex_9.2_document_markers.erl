-module('ex_9.2_document_markers').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        36,
        true,
        [],
        0,
        37,
        5,
        1,
        false,
        4,
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
        7,
        6,
        undefined,
        undefined,
        _,
        false,
        [],
        [
          {yamerl_stream_end,4,13},
          {yamerl_doc_end,4,1},
          {yamerl_scalar,3,1,
            {yamerl_tag,3,1,{non_specific,"?"}},
            flow,plain,"Document"},
          {yamerl_doc_start,2,1,{1,2},_},
          {yamerl_yaml_directive,1,1,{1,2}},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
