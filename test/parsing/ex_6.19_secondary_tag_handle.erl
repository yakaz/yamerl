-module('ex_6.19_secondary_tag_handle').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        74,
        true,
        [],
        0,
        75,
        4,
        1,
        false,
        3,
        36,
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
          {yamerl_stream_end,3,36},
          {yamerl_doc_end,3,36},
          {yamerl_scalar,3,7,
            {yamerl_tag,3,1,"tag:example.com,2000:app/int"},
            flow,plain,"1 - 3"},
          {yamerl_doc_start,2,1,{1,2},_},
          {yamerl_tag_directive,1,1,"!!","tag:example.com,2000:app/"},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
