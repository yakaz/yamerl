-module('ex_5.9_directive_indicator').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        19,
        true,
        [],
        0,
        20,
        3,
        1,
        false,
        2,
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
        [{impl_key,false,undefined,undefined,undefined,undefined,undefined}],
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
          {yamerl_stream_end,2,9},
          {yamerl_doc_end,2,9},
          {yamerl_scalar,2,5,
            {yamerl_tag,2,5,{non_specific,"?"}},
            flow,plain,"text"},
          {yamerl_doc_start,2,1,{1,2},_},
          {yamerl_yaml_directive,1,1,{1,2}},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
