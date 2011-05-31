%% $Id$

-module(yaml_parser).

-include("yaml_parser.hrl").
-include("yaml_tokens.hrl").

%% Public API.
-export([
    new/1,
    new/2,
    string/1,
    string/2,
    file/1,
    file/2,
    next_chunk/3,
    next_chunk/2,
    last_chunk/2,
    get_token_fun/1,
    set_token_fun/2,
    option_names/0,
    get_errors/1,
    throw_error/1
  ]).

%% -------------------------------------------------------------------
%% Main record to store the scanner state.
%% -------------------------------------------------------------------

-record(impl_key, {
    possible = false  :: boolean(),
    required          :: boolean(),
    line              :: position(),
    col               :: position(),
    chars_idx         :: pos_integer(),
    token_idx         :: pos_integer()
  }).

-record(bcoll, {
    kind   = root     :: root | sequence | mapping,
    indent = 0        :: 0    | position(),
    kidx   = -1       :: pos_integer() | -1, %% Last key index.
    kline  = 1        :: position(),         %% Last key line.
    kcol   = 1        :: position(),         %% Last key column.
    vidx   = -1       :: pos_integer() | -1, %% Last value index.
    vline  = 1        :: position(),         %% Last value line.
    vcol   = 1        :: position()          %% Last value column.
  }).

-record(fcoll, {
    kind   = sequence :: sequence | mapping | single_mapping,
    kidx   = -1       :: pos_integer() | -1, %% Last key/entry index.
    kline  = 1        :: position(),         %% Last key/entry line.
    kcol   = 1        :: position(),         %% Last key/entry column.
    vidx   = -1       :: pos_integer() | -1, %% Last value index.
    vline  = 1        :: position(),         %% Last value line.
    vcol   = 1        :: position()          %% Last value column.
  }).

-record(yaml_parser, {
    %%
    %% Buffer management.
    %%

    %% An indication of the source of the stream, eg. a file.
    source       :: any(),
    options = [] :: [yaml_parser_option()],

    %% Raw data corresponds to Unicode characters not decoded yet. The
    %% raw index indicates where the raw data is in the stream; it
    %% equals the amount of data already decoded in bytes. The raw_eos
    %% is a flag indicating the end of stream; this flag is set when the
    %% last chunk is seen.
    raw_data = <<>>  :: binary(),
    raw_idx  = 0     :: non_neg_integer(),
    raw_eos  = false :: boolean(),

    %% Characters data are the decoded Unicode characters but not
    %% scanned yet. The length corresponds to the number of characters
    %% in this list. The index indicates where the characters data is in
    %% the stream; it equals to the amount of data already scanned in
    %% characters.
    chars     = "" :: string(),
    chars_len = 0  :: non_neg_integer(),
    chars_idx = 0  :: non_neg_integer(),

    %% Cursor "position". While scanning the stream, we keep the line
    %% and column number. We also need to remember the last token end
    %% line and column.
    line                = 1     :: pos_integer(),
    col                 = 1     :: pos_integer(),
    endpos_set_by_token = false :: boolean(),
    last_token_endline  = 1     :: pos_integer(),
    last_token_endcol   = 1     :: pos_integer(),

    %%
    %% Stream informations.
    %%

    %% Character encoding of the stream. It must be a valid Unicode
    %% character encoding and it must not change after stream start.
    encoding :: encoding() | undefined,

    %%
    %% Document informations.
    %% Those informations are reset between each document.
    %%

    %% "doc_started" indicates if the document is started or not.
    %% This is used to know when directives are allowed for example.
    %% The document version is set by a YAML directive or to
    %% ?IMPLICIT_YAML_VERSION when a document starts if there were no
    %% directive.
    doc_started = false :: boolean(),
    doc_version         :: document_version() | undefined,

    %% "tags" is a dictionary containing default tag handles and those
    %% defined by TAG directives. It's used during tag resolution. The
    %% last tag property is stored in "last_tag" and is attached to a
    %% node only when this node is about to be emitted.
    tags = dict:new() :: tags_table(),

    %%
    %% Parsing state.
    %%

    %% The stream state corresponds to the name of the function to call
    %% when more data is available. This state is influenced by several
    %% parameters:
    %%   o  the block-context indentation prefixes (a stack);
    %%   o  the level of flow-context nesting (a stack too).
    stream_state = fun start_stream/1 :: fun((#yaml_parser{}) ->
                                               {ok, #yaml_parser{}}
                                             | {continue, #yaml_parser{}}
                                             | none()),
    parent_colls = []                 :: [#bcoll{} | #fcoll{}],
    cur_coll     = #bcoll{}           :: #bcoll{} | #fcoll{},

    %% When parsing a flow collection, we keep a "pending_entry" flag.
    %% The next token may trigger the queueing of a sequence entry token.
    pending_entry = false :: boolean(),

    %% We also keep a flag to know if the next expected token is a key:
    %% value pair.
    waiting_for_kvpair = false :: boolean(),

    %% Implicit keys handling.
    %% We need to know if the next token could be an implicit key.
    %% Furthermore, while searching for ':', marking the "end" of an
    %% implicit key, we need to store the positions where an implicit
    %% key could appear. In block context, an implicit key can't contain
    %% an implicit key. But in flow context, an implicit key can embed
    %% another implicit key.
    ik_allowed = false :: boolean(),
    ik_stack   = []    :: [#impl_key{}],

    %% We remember if the last queued token is JSON-like. JSON-like
    %% nodes are single- and double-quoted scalars and flow collections.
    %% Therefore, the JSON-like tokens are single- and double-quoted
    %% scalar tokens and flow collection end tokens.
    last_is_json_like = false :: boolean(),

    %% Did the last parsing eat the newline?
    missed_nl = false :: boolean(),

    %%
    %% Parsing output.
    %%

    %% Callbacks.
    token_fun :: yaml_parser_token_fun() | undefined,

    %% List of scanned tokens with counters.
    tokens        = []  :: [yaml_token()],
    tks_queued    = 0   :: non_neg_integer(),
    tks_first_idx = 1   :: pos_integer(),
    tks_emitted   = 0   :: non_neg_integer(),

    %% We keep a copy of the last emitted token. This is used for
    %% verification purpose. For instance, two scalars in a row is an
    %% error.
    last_tag            :: yaml_tag()    | undefined,
    last_anchor         :: yaml_anchor() | undefined,
    last_token          :: yaml_token()  | undefined,

    %% List of warnings and errors. The scan won't necessarily stop at
    %% the first error.
    has_errors = false  :: boolean(),
    errors     = []     :: [#yaml_parser_error{}],

    %% When the user doesn't specify a "token_fun" callback module all
    %% tokens to be emitted are stored in the following list.
    tks_ready  = []     :: [yaml_token()]
  }).

-record(directive_ctx, {
    line      :: position(),
    col       :: position(),
    name = "" :: string()
  }).

-record(yaml_directive_ctx, {
    line  :: position(),
    col   :: position(),
    major :: non_neg_integer(),
    minor :: non_neg_integer()
  }).

-record(tag_directive_ctx, {
    line   :: position(),
    col    :: position(),
    handle :: tag_handle() | [] | undefined,
    prefix :: tag_prefix() | [] | undefined
  }).

-record(reserved_directive_ctx, {
    line           :: position(),
    col            :: position(),
    name           :: string(),
    current        :: string(),
    args = []      :: [string()],
    args_count = 0 :: non_neg_integer()
  }).

-record(block_scalar_hd_ctx, {
    style      = literal  :: literal | folded,
    line       = 1        :: position(),      %% Line where the token starts.
    col        = 1        :: position(),      %% Column where the token starts.
    chomp                 :: strip | keep | undefined, %% Chomping indicator.
    indent                :: pos_integer()    %% Indentation indicator.
                           | {tmp, pos_integer()} | undefined,
    in_comment = false    :: boolean()        %% Trailing comment.
  }).

-record(block_scalar_ctx, {
    style         = literal :: literal | folded,
    line          = 1       :: position(),    %% Line where the token starts.
    col           = 1       :: position(),    %% Column where the token starts.
    endline       = 1       :: position(),    %% Line where the token ends.
    endcol        = 1       :: position(),    %% Column where the token ends.
    chomp         = clip    :: strip | keep | clip, %% Chomping method.
    indent                  :: pos_integer() | undefined, %% Block indent.
    longest_empty = 0       :: non_neg_integer(), %% Longest leading empty line.
    newline       = false   :: boolean(),     %% Met a newline character.
    spaces        = ""      :: string(),      %% Last white spaces seen.
    more_indent   = false   :: boolean(),     %% Last line is more indented.
    output        = ""      :: string()       %% Already parsed characters.
  }).

-record(flow_scalar_ctx, {
    style        = plain :: double_quoted | single_quoted | plain,
    line         = 1     :: position(), %% Line where the token starts.
    col          = 1     :: position(), %% Column where the token starts.
    endline      = 1     :: position(), %% Line where the token ends.
    endcol       = 1     :: position(), %% Column where the token ends.
    next_escaped = false :: boolean(),  %% Is next character escaped?
    surrogate            :: 16#d800..16#dbff | undefined, %% High surrogate.
    newline      = false :: boolean(),  %% Met a newline character.
    spaces       = ""    :: string(),   %% Last white spaces seen.
    output       = ""    :: string()    %% Already parsed characters.
  }).

-record(anchor_ctx, {
    type        :: anchor | alias,
    line        :: position(),
    col         :: position(),
    output = "" :: string()
  }).

-record(tag_ctx, {
    line   :: position(),
    col    :: position(),
    prefix :: string() | undefined,
    suffix :: string() | tag_uri()
  }).

-define(IO_BLOCKSIZE, 4096). %% Common filesystem blocksize.

-define(FAKE_IMPL_KEY, #impl_key{}).

-define(IN_BLOCK_CTX(P),      (is_record(P#yaml_parser.cur_coll, bcoll))).
-define(IN_FLOW_CTX(P),       (is_record(P#yaml_parser.cur_coll, fcoll))).

-define(IS_SPACE(C),          (C == $\s orelse C == $\t)).
-define(IS_NEWLINE(C),        (C == $\n orelse C == $\r)).
-define(IS_NEWLINE_11(C),
  (C == 16#85 orelse C == 16#2028 orelse C == 16#2029)).

-define(IS_FLOW_INDICATOR(C), (
    C == $[ orelse C == $] orelse
    C == ${ orelse C == $} orelse
    C == $,)).

-define(IS_HEXADECIMAL(C), (
    (O1 >= $0 andalso O1 =< $9) orelse
    (O1 >= $a andalso O1 =< $f) orelse
    (O1 >= $A andalso O1 =< $F)
  )).

-define(IS_URI_CHAR(C),
  (
    (C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $0 andalso C =< $9) orelse
    C == $- orelse
    C == $% orelse C == $# orelse C == $; orelse C == $/ orelse C == $? orelse
    C == $: orelse C == $@ orelse C == $& orelse C == $= orelse C == $+ orelse
    C == $$ orelse C == $, orelse C == $_ orelse C == $. orelse C == $! orelse
    C == $~ orelse C == $* orelse C == $' orelse C == $( orelse C == $) orelse
    C == $[ orelse C == $]
  )).

-define(IS_BOM(C),            (C == 16#feff)).
-define(IS_HIGH_SURROGATE(C), (C >= 16#d800 andalso C =< 16#dbff)).
-define(IS_LOW_SURROGATE(C),  (C >= 16#dc00 andalso C =< 16#dfff)).

-define(CURSOR_LINE(P),   P#yaml_parser.line).
-define(CURSOR_COLUMN(P), P#yaml_parser.col).

-define(MISSING_ENTRY(S), (
    S#yaml_parser.pending_entry andalso
    S#yaml_parser.last_tag == undefined
  )).
-define(MISSING_KVPAIR(S), (
    S#yaml_parser.waiting_for_kvpair andalso
    not element(#impl_key.possible, hd(S#yaml_parser.ik_stack))
  )).

-define(IS_JSON_LIKE(T), (
    (is_record(T, yaml_scalar) andalso
     (T#yaml_scalar.substyle == single_quoted orelse
      T#yaml_scalar.substyle == double_quoted)) orelse
    (is_record(T, yaml_collection_end) andalso
     T#yaml_collection_end.style == flow)
  )).

-define(DEFAULT_TAG(U, L, C),
  #yaml_tag{
    uri    = U,
    line   = L,
    column = C
  }).

-define(SUSPEND_SUBPARSING(S, C, F),
  return(S#yaml_parser{
      stream_state = fun(S1) -> F(S1, C) end
    })).

%% -------------------------------------------------------------------
%% Public API: chunked stream scanning.
%% -------------------------------------------------------------------

new(Source) ->
    new(Source, []).

new(Source, Options) ->
    check_options(Options),
    #yaml_parser{
      source    = Source,
      options   = Options,
      token_fun = proplists:get_value(token_fun, Options)
    }.

next_chunk(Parser, <<>>, false) ->
    %% No need to proceed further without any data.
    return(Parser);
next_chunk(#yaml_parser{raw_data = Data} = Parser, More_Data, EOS) ->
    %% Append new data to the remaining data. Those data must then be
    %% decoded to Unicode characters.
    New_Data = list_to_binary([Data, More_Data]),
    Parser1  = Parser#yaml_parser{
      raw_data = New_Data,
      raw_eos  = EOS
    },
    decode_unicode(Parser1).

next_chunk(Parser, More_Data) ->
    next_chunk(Parser, More_Data, false).

last_chunk(Parser, More_Data) ->
    next_chunk(Parser, More_Data, true).

%% -------------------------------------------------------------------
%% Public API: common stream sources.
%% -------------------------------------------------------------------

string(String) ->
    string(String, []).

string(String, Options) when is_binary(String) ->
    Parser = new(string, Options),
    next_chunk(Parser, String, true);
string(String, Options) when is_list(String) ->
    string(list_to_binary(String), Options).

file(Filename) ->
    file(Filename, []).

file(Filename, Options) ->
    Parser    = new({file, Filename}, Options),
    Blocksize = proplists:get_value(io_blocksize, Options, ?IO_BLOCKSIZE),
    case file:open(Filename, [read, binary]) of
        {ok, FD} ->
            %% The file is read in binary mode. The scanner is
            %% responsible for determining the encoding and converting
            %% the stream accordingly.
            file2(Parser, FD, Blocksize);
        {error, Reason} ->
            Error2 = #yaml_parser_error{
              name  = file_open_failure,
              extra = [{error, Reason}]
            },
            Parser2 = add_error(Parser, Error2,
              "Failed to open file \"~s\": ~s",
              [Filename, file:format_error(Reason)]),
            return(Parser2)
    end.

file2(#yaml_parser{source = {file, Filename}} = Parser, FD, Blocksize) ->
    case file:read(FD, Blocksize) of
        {ok, Data} ->
            %% If the chunk is smaller than the requested size, we
            %% reached EOS.
            EOS = size(Data) < Blocksize,
            if
                EOS  -> file:close(FD);
                true -> ok
            end,
            try
                case next_chunk(Parser, Data, EOS) of
                    {continue, Parser1} ->
                        file2(Parser1, FD, Blocksize);
                    Parser1 ->
                        Parser1
                end
            catch
                throw:{yaml_parser, _} = Exception ->
                    %% Close the file and throw the exception again.
                    file:close(FD),
                    throw(Exception)
            end;
        eof ->
            file:close(FD),
            next_chunk(Parser, <<>>, true);
        {error, Reason} ->
            Error = #yaml_parser_error{
              name = file_read_failure,
              extra = [{error, Reason}]
            },
            Parser1 = add_error(Parser, Error,
              "Failed to read file \"~s\": ~s",
              [Filename, file:format_error(Reason)]),
            return(Parser1)
    end.

%% -------------------------------------------------------------------
%% Public API: get/set the token function.
%% -------------------------------------------------------------------

get_token_fun(#yaml_parser{token_fun = Fun}) ->
    Fun.

set_token_fun(Parser, Fun) when is_function(Fun, 1) ->
    Parser#yaml_parser{token_fun = Fun}.

%% -------------------------------------------------------------------
%% Errors and warnings handling.
%% -------------------------------------------------------------------

get_errors(#yaml_parser{errors = Errors}) ->
    lists:reverse(Errors);
get_errors(#yaml_parser_error{} = Error) ->
    [Error].

%% -------------------------------------------------------------------
%% Determine encoding and decode Unicode.
%% -------------------------------------------------------------------

decode_unicode(#yaml_parser{stream_state = State,
    encoding = Encoding, raw_data = Data, raw_idx = Raw_Index,
    chars = Chars, chars_len = Chars_Count} = Parser)
  when Encoding /= undefined ->
    %% We have previously determined the encoding of the stream. We can
    %% decode the Unicode characters from the raw data.
    Parser2 = case unicode:characters_to_list(Data, Encoding) of
        {Reason, New_Chars, Remaining_Data} ->
            %% Ok, we have more characters to scan!
            Raw_Index1 = Raw_Index + (size(Data) - size(Remaining_Data)),
            Parser1    = Parser#yaml_parser{
              raw_data   = Remaining_Data,
              raw_idx    = Raw_Index1,
              chars      = Chars ++ New_Chars,
              chars_len  = Chars_Count + length(New_Chars)
            },
            case Reason of
                incomplete ->
                    Parser1;
                error ->
                    Error = #yaml_parser_error{
                      name  = invalid_unicode,
                      extra = [{byte, Raw_Index1 + 1}]
                    },
                    add_error(Parser1, Error,
                      "Invalid Unicode character at byte #~b",
                      [Raw_Index1 + 1])
            end;
        New_Chars ->
            %% Ok, we have more characters to scan!
            Raw_Index1 = Raw_Index + size(Data),
            Parser#yaml_parser{
              raw_data   = <<>>,
              raw_idx    = Raw_Index1,
              chars      = Chars ++ New_Chars,
              chars_len  = Chars_Count + length(New_Chars)
            }
    end,
    State(Parser2);
decode_unicode(#yaml_parser{raw_data = Data, raw_eos = EOS} = Parser)
  when ((EOS == false andalso size(Data) >= 4) orelse EOS == true) ->
    %% We have enough (maybe even all) data to determine the encoding.
    %% Let's check if the stream starts with a BOM.
    {Encoding, Length} = get_encoding(Data),
    %% The stream may start with a BOM: remove it.
    <<_:Length/binary, New_Data/binary>> = Data,
    Parser1 = Parser#yaml_parser{
      encoding  = Encoding,
      raw_data  = New_Data,
      raw_idx   = Length,
      chars_idx = 1
    },
    decode_unicode(Parser1);
decode_unicode(Parser) ->
    %% We don't have enough data to determine the encoding. We ask for
    %% more data.
    return(Parser).

get_encoding(<<16#00, 16#00, 16#fe, 16#ff, _/binary>>) -> {{utf32, big},    4};
get_encoding(<<16#00, 16#00, 16#00, _,     _/binary>>) -> {{utf32, big},    0};
get_encoding(<<16#ff, 16#fe, 16#00, 16#00, _/binary>>) -> {{utf32, little}, 4};
get_encoding(<<_,     16#00, 16#00, 16#00, _/binary>>) -> {{utf32, little}, 0};
get_encoding(<<16#fe, 16#ff, _,     _,     _/binary>>) -> {{utf16, big},    2};
get_encoding(<<16#00, _,     _,     _,     _/binary>>) -> {{utf16, big},    0};
get_encoding(<<16#ff, 16#fe, _,     _,     _/binary>>) -> {{utf16, little}, 2};
get_encoding(<<_,     16#00, _,     _,     _/binary>>) -> {{utf16, little}, 0};
get_encoding(<<16#ef, 16#bb, 16#bf, _,     _/binary>>) -> {utf8,            3};
get_encoding(_)                                        -> {utf8,            0}.

%% -------------------------------------------------------------------
%% Scan characters and emit tokens.
%% -------------------------------------------------------------------

%%
%% Stream start/end.
%%

start_stream(#yaml_parser{encoding = Encoding} = Parser) ->
    %% The very first token to emit is the stream start. The stream
    %% encoding is provided as an attribute. The encoding may appear at
    %% the start of each document but can't be changed: all documents
    %% must have the same encoding!
    Parser1 = push_fake_impl_key(Parser),
    Parser2 = allow_impl_key(Parser1, true),
    Parser3 = setup_default_tags(Parser2),
    Token   = #yaml_stream_start{
      encoding = Encoding,
      line     = ?CURSOR_LINE(Parser3),
      column   = ?CURSOR_COLUMN(Parser3)
    },
    Parser4 = queue_token(Parser3, Token),
    next_state(Parser4, fun find_next_token/1).

end_stream(#yaml_parser{last_token_endline = Line,
  last_token_endcol = Col} = Parser) ->
    %% Reset cursor on column 0 to close all opened block collections.
    Parser1 = check_for_closed_block_collections(Parser, 0),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = allow_impl_key(Parser2, false),
    %% Set the line and column number to the last token endline/endcol
    %% number. This is useful when parsing a file: the last line is
    %% often terminated by a newline character. Thanks to this, the
    %% stream_end token will be on the last token line.
    Token    = #yaml_stream_end{
      line   = Line,
      column = Col
    },
    Parser4 = queue_token(Parser3, Token),
    return(Parser4).

%%
%% Next token.
%%

find_next_token(#yaml_parser{endpos_set_by_token = true} = Parser) ->
    %% The line and column numbers where the last token ends was already
    %% set during token parsing.
    Parser1 = Parser#yaml_parser{
      endpos_set_by_token = false
    },
    next_state(Parser1, fun do_find_next_token/1);
find_next_token(#yaml_parser{line = Line, col = Col} = Parser) ->
    %% Record the line and columns numbers where the last token ends.
    %% It's used to determine if an implicit key would span several
    %% lines and therefore would be unacceptable.
    Parser1 = Parser#yaml_parser{
      endpos_set_by_token = false,
      last_token_endline  = Line,
      last_token_endcol   = Col
    },
    next_state(Parser1, fun do_find_next_token/1).

%% Skip spaces.
do_find_next_token(#yaml_parser{chars = [$\s | Rest]} = Parser) ->
    Parser1 = next_col(Parser, 1, Rest),
    do_find_next_token(Parser1);

%% Skip tabs only when they're separation spaces, not indentation.
do_find_next_token(
  #yaml_parser{chars = [$\t | Rest], ik_allowed = IK_Allowed} = Parser)
  when ?IN_FLOW_CTX(Parser) orelse not IK_Allowed ->
    Parser1 = next_col(Parser, 1, Rest),
    do_find_next_token(Parser1);

%% Skip comments.
do_find_next_token(#yaml_parser{chars = [$# | _]} = Parser) ->
    next_state(Parser, fun parse_comment/1);

%% Continue with next line.
do_find_next_token(
  #yaml_parser{missed_nl = true} = Parser) ->
    Parser1 = Parser#yaml_parser{
      missed_nl = false
    },
    Parser2 = if
        ?IN_BLOCK_CTX(Parser1) -> allow_impl_key(Parser1, true);
        true                   -> Parser1
    end,
    do_find_next_token(Parser2);

do_find_next_token(
  #yaml_parser{chars = [$\r], raw_eos = false} = Parser) ->
    %% Can't be sure it's a newline. It may be followed by a LF.
    return(Parser);
do_find_next_token(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    Parser2 = if
        ?IN_BLOCK_CTX(Parser1) -> allow_impl_key(Parser1, true);
        true                   -> Parser1
    end,
    do_find_next_token(Parser2);

%% End-of-stream reached.
do_find_next_token(#yaml_parser{chars = [], raw_eos = true} = Parser) ->
    next_state(Parser, fun end_stream/1);

%% Wait for more data.
do_find_next_token(#yaml_parser{chars = []} = Parser) ->
    return(Parser);

%% Next token found!
do_find_next_token(Parser) ->
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Parser2 = check_for_closed_block_collections(Parser1),
    next_state(Parser2, fun determine_token_type/1).

%%
%% Token type.
%%

%% Not enough data to determine the token type.
determine_token_type(#yaml_parser{chars_len = Len, raw_eos = false} = Parser)
  when Len < 4 ->
    return(Parser);

%% BOM, before a document only!
determine_token_type(
  #yaml_parser{chars = [C | Rest], doc_started = false} = Parser)
  when ?IS_BOM(C) ->
    Parser1 = next_col(Parser, 1, Rest),
    next_state(Parser1, fun find_next_token/1);
determine_token_type(
  #yaml_parser{chars = [C | Rest], doc_started = true} = Parser)
  when ?IS_BOM(C) ->
    %% A BOM is forbidden after the document start. Because it's not
    %% fatal during parsing, we only add a warning. Note that the YAML
    %% specification considers this to be an error.
    Error = #yaml_parser_error{
      type   = warning,
      name   = bom_after_doc_start,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "A BOM must not appear inside a document", []),
    Parser2 = next_col(Parser1, 1, Rest),
    next_state(Parser2, fun find_next_token/1);

%% Directives end indicator.
determine_token_type(
  #yaml_parser{doc_version = Version,
    chars = [$-, $-, $-, C | _], col = 1} = Parser)
  when ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Fun = fun(S) ->
        parse_document_sep(S, directives_end)
    end,
    next_state(Parser, Fun);

%% Document end indicator.
determine_token_type(
  #yaml_parser{doc_version = Version,
    chars = [$., $., $., C | _], col = 1} = Parser)
  when ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Fun = fun(S) ->
        parse_document_sep(S, document_end)
    end,
    next_state(Parser, Fun);

%% Directive indicator.
determine_token_type(
  #yaml_parser{chars = [$% | _], col = 1,
  doc_started = false} = Parser) ->
    next_state(Parser, fun parse_directive/1);

%% Flow sequence indicators.
determine_token_type(#yaml_parser{chars = [$[ | _]} = Parser) ->
    Fun = fun(S) ->
        parse_flow_collection_start(S, sequence)
    end,
    next_state(Parser, Fun);
determine_token_type(#yaml_parser{chars = [$] | _]} = Parser) ->
    Fun = fun(S) ->
        parse_flow_collection_end(S, sequence)
    end,
    next_state(Parser, Fun);

%% Flow mapping indicators.
determine_token_type(#yaml_parser{chars = [${ | _]} = Parser) ->
    Fun = fun(S) ->
        parse_flow_collection_start(S, mapping)
    end,
    next_state(Parser, Fun);
determine_token_type(#yaml_parser{chars = [$} | _]} = Parser) ->
    Fun = fun(S) ->
        parse_flow_collection_end(S, mapping)
    end,
    next_state(Parser, Fun);

%% Flow collection entry indicator.
determine_token_type(#yaml_parser{chars = [$, | _]} = Parser) ->
    next_state(Parser, fun parse_flow_entry/1);

%% Block collection entry indicator.
determine_token_type(
  #yaml_parser{doc_version = Version, chars = [$-, C | _]} = Parser)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    next_state(Parser, fun parse_block_entry/1);

%% Mapping key indicator.
determine_token_type(
  #yaml_parser{doc_version = Version, chars = [$?, C | _]} = Parser)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    next_state(Parser, fun parse_mapping_key/1);

%% Mapping value indicator.
determine_token_type(
  #yaml_parser{doc_version = Version, chars = [$:, C | _]} = Parser)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse ?IS_FLOW_INDICATOR(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    next_state(Parser, fun parse_mapping_value/1);
determine_token_type(#yaml_parser{chars = [$: | _],
    last_is_json_like = true} = Parser)
  when ?IN_FLOW_CTX(Parser) ->
    %% This is a key: value pair indicator only when the last token is
    %% JSON-like and we're in flow context.
    next_state(Parser, fun parse_mapping_value/1);

%% Anchor and alias indicator.
determine_token_type(#yaml_parser{chars = [$& | _]} = Parser) ->
    Fun = fun(S) ->
        parse_anchor_or_alias(S, anchor)
    end,
    next_state(Parser, Fun);
determine_token_type(#yaml_parser{chars = [$* | _]} = Parser) ->
    Fun = fun(S) ->
        parse_anchor_or_alias(S, alias)
    end,
    next_state(Parser, Fun);

%% Tag indicator.
determine_token_type(#yaml_parser{chars = [$! | _]} = Parser) ->
    next_state(Parser, fun parse_tag/1);

%% Block scalar.
determine_token_type(#yaml_parser{chars = [$| | _]} = Parser) ->
    Fun = fun(S) ->
        parse_block_scalar(S, literal)
    end,
    next_state(Parser, Fun);
determine_token_type(#yaml_parser{chars = [$> | _]} = Parser) ->
    Fun = fun(S) ->
        parse_block_scalar(S, folded)
    end,
    next_state(Parser, Fun);

%% Single-quoted flow scalar.
determine_token_type(#yaml_parser{chars = [$' | _]} = Parser) ->
    Fun = fun(S) ->
        parse_flow_scalar(S, single_quoted)
    end,
    next_state(Parser, Fun);

%% Double-quoted flow scalar.
determine_token_type(#yaml_parser{chars = [$" | _]} = Parser) ->
    Fun = fun(S) ->
        parse_flow_scalar(S, double_quoted)
    end,
    next_state(Parser, Fun);

%% Reserved indicators.
%% We add a warning and parse it as a plain scalar.
determine_token_type(#yaml_parser{chars = [C | _]} = Parser)
  when C == $@ orelse C == $` ->
    Error = #yaml_parser_error{
      name   = reserved_indicator,
      type   = warning,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "The reserved indicator \"~c\" is not allowed at the "
      "beginning of a plain scalar", [C]),
    Fun = fun(S) ->
        parse_flow_scalar(S, plain)
    end,
    next_state(Parser1, Fun);

%% Plain flow scalar.
determine_token_type(Parser) ->
    Fun = fun(S) ->
        parse_flow_scalar(S, plain)
    end,
    next_state(Parser, Fun).

%% -------------------------------------------------------------------
%% Directives and document ends.
%% -------------------------------------------------------------------

parse_document_sep(#yaml_parser{chars = [_, _, _ | Rest]} = Parser, Type) ->
    %% Reset cursor on column 0 to close all opened block collections.
    Parser1 = check_for_closed_block_collections(Parser, 0),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = allow_impl_key(Parser2, false),
    Parser4 = case Type of
        directives_end ->
            start_doc(Parser3,
              ?CURSOR_LINE(Parser3), ?CURSOR_COLUMN(Parser3), tail);
        document_end ->
            end_doc(Parser3,
              ?CURSOR_LINE(Parser3), ?CURSOR_COLUMN(Parser3), tail)
    end,
    Parser5 = next_col(Parser4, 3, Rest),
    next_state(Parser5, fun find_next_token/1).

start_doc(#yaml_parser{doc_started = true} = Parser,
  Line, Col, Insert_At) ->
    %% A document is already opened: we must close it before starting a
    %% new one.
    Parser1 = end_doc(Parser, Line, Col, Insert_At),
    start_doc(Parser1, Line, Col, next_insert_at(Insert_At, 1));
start_doc(
  #yaml_parser{options = Options, doc_version = Version, tags = Tags} = Parser,
  Line, Col, Insert_At) ->
    %% When a document starts, we set the version to
    %% ?IMPLICIT_DOC_VERSION if no YAML directive were specified.
    Forced   = proplists:get_value(doc_version, Options),
    Version1 = case Version of
        _ when Forced /= undefined -> Forced;
        undefined                  -> ?IMPLICIT_YAML_VERSION;
        _                          -> Version
    end,
    Token = #yaml_doc_start{
      version = Version1,
      tags    = Tags,
      line    = Line,
      column  = Col
    },
    Parser1 = case Version1 of
        {Major, Minor} when Major < ?MIN_YAML_MAJOR_VERSION_SUPPORTED orelse
        (Major == ?MIN_YAML_MAJOR_VERSION_SUPPORTED andalso
         Minor < ?MIN_YAML_MINOR_VERSION_SUPPORTED) ->
            %% The document's version is not supported at all (below
            %% minimum supported version).
            Error = #yaml_parser_error{
              name   = version_not_supported,
              token  = Token,
              line   = Line,
              column = Col
            },
            Parser0 = add_error(Parser, Error,
              "Version ~b.~b not supported (minimum version ~b.~b)",
              [
                Major, Minor,
                ?MIN_YAML_MAJOR_VERSION_SUPPORTED,
                ?MIN_YAML_MINOR_VERSION_SUPPORTED
              ]),
            return(Parser0);
        {Major, Minor} when
        Major <  ?MAX_YAML_MAJOR_VERSION_SUPPORTED orelse
        (Major == ?MAX_YAML_MAJOR_VERSION_SUPPORTED andalso
         Minor =< ?MAX_YAML_MINOR_VERSION_SUPPORTED) ->
            %% Version supported.
            Parser;
        {Major, Minor} when Major > ?MAX_YAML_MAJOR_VERSION_SUPPORTED ->
            %% The document's version is not supported at all (major
            %% above maximum supporter major).
            Error = #yaml_parser_error{
              name   = version_not_supported,
              token  = Token,
              line   = Line,
              column = Col
            },
            Parser0 = add_error(Parser, Error,
              "Version ~b.~b not supported (maximum version ~b.~b)",
              [
                Major, Minor,
                ?MAX_YAML_MAJOR_VERSION_SUPPORTED,
                ?MAX_YAML_MINOR_VERSION_SUPPORTED
              ]),
            return(Parser0);
        {Major, Minor} when Minor > ?MAX_YAML_MINOR_VERSION_SUPPORTED ->
            %% The document's minor version is greater than the
            %% supported version. Add a warning and continue anyway.
            Error = #yaml_parser_error{
              name   = version_not_supported,
              type   = warning,
              token  = Token,
              line   = Line,
              column = Col
            },
            Parser0 = add_error(Parser, Error,
              "Version ~b.~b not supported (maximum version ~b.~b); "
              "parsing may fail",
              [
                Major, Minor,
                ?MAX_YAML_MAJOR_VERSION_SUPPORTED,
                ?MAX_YAML_MINOR_VERSION_SUPPORTED
              ]),
            Parser0
    end,
    Parser2 = Parser1#yaml_parser{
      doc_started = true,
      doc_version = Version1
    },
    %% Emit a token with the determined version and the tags table.
    queue_token(Parser2, Token, Insert_At).

end_doc(#yaml_parser{doc_started = false} = Parser, _, _, _) ->
    %% No document to end.
    Parser;
end_doc(Parser, Line, Col, Insert_At) ->
    %% At the end of the document, we reset the version and the tags
    %% table.
    Parser1 = Parser#yaml_parser{
      doc_started = false,
      doc_version = undefined
    },
    Parser2 = setup_default_tags(Parser1),
    Token = #yaml_doc_end{
      line   = Line,
      column = Col
    },
    queue_token(Parser2, Token, Insert_At).

%% -------------------------------------------------------------------
%% Directives.
%% -------------------------------------------------------------------

parse_directive(
  #yaml_parser{line = Line, col = Col, chars = [_ | Rest]} = Parser) ->
    Ctx = #directive_ctx{
      line = Line,
      col  = Col
    },
    %% Reset cursor on column 0 to close all opened block collections.
    Parser1 = check_for_closed_block_collections(Parser, 0),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = allow_impl_key(Parser2, false),
    Parser4 = next_col(Parser3, 1, Rest),
    do_parse_directive(Parser4, Ctx).

do_parse_directive(
  #yaml_parser{doc_version = Version, chars = [C | _]} = Parser, Ctx)
  when ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    parse_directive2(Parser, Ctx);

do_parse_directive(#yaml_parser{chars = [C | Rest]} = Parser, Ctx) ->
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Parser2 = next_col(Parser1, 1, Rest),
    Ctx1    = Ctx#directive_ctx{
      name = [C | Ctx#directive_ctx.name]
    },
    do_parse_directive(Parser2, Ctx1);

do_parse_directive(
  #yaml_parser{chars = [], raw_eos = true} = Parser, Ctx) ->
    parse_directive2(Parser, Ctx);
do_parse_directive(
  #yaml_parser{chars = [], raw_eos = false} = Parser, Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_directive).

parse_directive2(Parser, Ctx) ->
    Name = lists:reverse(Ctx#directive_ctx.name),
    Ctx1 = Ctx#directive_ctx{
      name = Name
    },
    case Name of
        "YAML" -> parse_yaml_directive(Parser, Ctx1);
        "TAG"  -> parse_tag_directive(Parser, Ctx1);
        _      -> parse_reserved_directive(Parser, Ctx1)
    end.

skip_directive_trailing_ws(#yaml_parser{chars = [C | Rest]} = Parser)
  when ?IS_SPACE(C) ->
    Parser1 = next_col(Parser, 1, Rest),
    skip_directive_trailing_ws(Parser1);
skip_directive_trailing_ws(
  #yaml_parser{doc_version = Version, chars = [C | _]} = Parser)
  when ?IS_NEWLINE(C) orelse C == $# orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    next_state(Parser, fun find_next_token/1);
skip_directive_trailing_ws(
  #yaml_parser{chars = []} = Parser) ->
    return(Parser);

skip_directive_trailing_ws(#yaml_parser{chars = [_ | _]} = Parser) ->
    Error = #yaml_parser_error{
      name   = unexpected_directive_extra_params,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected directive extra parameters", []),
    return(Parser1).

%%
%% YAML directive.
%%

parse_yaml_directive(Parser, #directive_ctx{line = Line, col = Col}) ->
    Ctx = #yaml_directive_ctx{
      line = Line,
      col  = Col
    },
    parse_yaml_directive_major(Parser, Ctx).

%% Major version number.
parse_yaml_directive_major(#yaml_parser{chars = [C | Rest]} = Parser,
  #yaml_directive_ctx{major = Major} = Ctx) when C >= $0 andalso C =< $9 ->
    Major1 = case Major of
        undefined -> C - $0;
        _         -> Major * 10 + C - $0
    end,
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#yaml_directive_ctx{
      major = Major1
    },
    parse_yaml_directive_major(Parser1, Ctx1);
parse_yaml_directive_major(#yaml_parser{chars = [$. | Rest]} = Parser,
  #yaml_directive_ctx{major = Major} = Ctx) when is_integer(Major) ->
    Parser1 = next_col(Parser, 1, Rest),
    %% End of the major part. Continue with the minor version number.
    parse_yaml_directive_minor(Parser1, Ctx);
parse_yaml_directive_major(#yaml_parser{chars = [C | Rest]} = Parser,
  #yaml_directive_ctx{major = undefined} = Ctx) when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    Parser1 = next_col(Parser, 1, Rest),
    parse_yaml_directive_major(Parser1, Ctx);
parse_yaml_directive_major(#yaml_parser{chars = [], raw_eos = false} = Parser,
  Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, parse_yaml_directive_major);

parse_yaml_directive_major(#yaml_parser{chars = [_ | _]} = Parser,
  #yaml_directive_ctx{line = Line, col = Col}) ->
    %% Invalid character (or end of directive) while parsing major
    %% version number.
    Token = #yaml_yaml_directive{
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_yaml_directive,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Invalid major version number in YAML directive", []),
    return(Parser1);
parse_yaml_directive_major(#yaml_parser{chars = [], raw_eos = true} = Parser,
  #yaml_directive_ctx{line = Line, col = Col}) ->
    %% Invalid end-of-stream while parsing major version number.
    Token = #yaml_yaml_directive{
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_yaml_directive,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing YAML directive", []),
    return(Parser1).

%% Minor version number.
parse_yaml_directive_minor(#yaml_parser{chars = [C | Rest]} = Parser,
  #yaml_directive_ctx{minor = Minor} = Ctx) when C >= $0 andalso C =< $9 ->
    Minor1 = case Minor of
        undefined -> C - $0;
        _         -> Minor * 10 + C - $0
    end,
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#yaml_directive_ctx{
      minor = Minor1
    },
    parse_yaml_directive_minor(Parser1, Ctx1);
parse_yaml_directive_minor(
  #yaml_parser{doc_version = Version, chars = [C | _]} = Parser,
  #yaml_directive_ctx{minor = Minor} = Ctx)
  when is_integer(Minor) andalso
  (?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
   (Version == {1,1} andalso ?IS_NEWLINE_11(C))) ->
    %% Directive end.
    queue_yaml_directive(Parser, Ctx);
parse_yaml_directive_minor(#yaml_parser{chars = [], raw_eos = true} = Parser,
  Ctx) ->
    %% Directive end.
    queue_yaml_directive(Parser, Ctx);
parse_yaml_directive_minor(#yaml_parser{chars = [], raw_eos = false} = Parser,
  Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, parse_yaml_directive_minor);

parse_yaml_directive_minor(#yaml_parser{chars = [_ | _]} = Parser,
  #yaml_directive_ctx{line = Line, col = Col}) ->
    %% Invalid character while parsing minor version number.
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Token = #yaml_yaml_directive{
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_yaml_directive,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser2 = add_error(Parser1, Error,
      "Invalid minor version number in YAML directive", []),
    return(Parser2).

%% Queue token.
queue_yaml_directive(#yaml_parser{doc_version = undefined} = Parser,
  #yaml_directive_ctx{major = Major, minor = Minor, line = Line, col = Col}) ->
    Version = {Major, Minor},
    Token = #yaml_yaml_directive{
      version = Version,
      line    = Line,
      column  = Col
    },
    Parser1 = queue_token(Parser, Token),
    Parser2 = Parser1#yaml_parser{
      doc_version = Version
    },
    next_state(Parser2, fun skip_directive_trailing_ws/1);
queue_yaml_directive(Parser,
  #yaml_directive_ctx{major = Major, minor = Minor,
  line = Line, col = Col} = Ctx) ->
    %% Warning: repeated YAML directive.
    Version = {Major, Minor},
    Token   = #yaml_yaml_directive{
      version = Version,
      line    = Line,
      column  = Col
    },
    Error = #yaml_parser_error{
      type   = warning,
      name   = multiple_yaml_directives,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Multiple YAML directives found: the last one will be used", []),
    Parser2 = Parser1#yaml_parser{
      doc_version = undefined
    },
    queue_yaml_directive(Parser2, Ctx).

%%
%% TAG directive.
%%

parse_tag_directive(Parser, #directive_ctx{line = Line, col = Col}) ->
    Ctx = #tag_directive_ctx{
      line = Line,
      col  = Col
    },
    parse_tag_directive_handle(Parser, Ctx).

%% Tag handle.
parse_tag_directive_handle(#yaml_parser{chars = [$! | Rest]} = Parser,
  #tag_directive_ctx{handle = undefined} = Ctx) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_directive_ctx{
      handle = "!"
    },
    parse_tag_directive_handle(Parser1, Ctx1);
parse_tag_directive_handle(#yaml_parser{chars = [C | Rest]} = Parser,
  #tag_directive_ctx{handle = Handle} = Ctx)
  when is_list(Handle) andalso 
  ((C >= $a andalso C =< $z) orelse
   (C >= $A andalso C =< $Z) orelse
   (C >= $0 andalso C =< $9) orelse
   (C == $-)) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_directive_ctx{
      handle = [C | Handle]
    },
    parse_tag_directive_handle(Parser1, Ctx1);
parse_tag_directive_handle(#yaml_parser{chars = [$! | Rest]} = Parser,
  #tag_directive_ctx{handle = Handle} = Ctx) when is_list(Handle) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_directive_ctx{
      handle = [$! | Handle]
    },
    parse_tag_directive_prefix(Parser1, Ctx1);
parse_tag_directive_handle(#yaml_parser{chars = [C | _]} = Parser,
  #tag_directive_ctx{handle = "!"} = Ctx) when ?IS_SPACE(C) ->
    parse_tag_directive_prefix(Parser, Ctx);
parse_tag_directive_handle(#yaml_parser{chars = [C | Rest]} = Parser,
  #tag_directive_ctx{handle = undefined} = Ctx) when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    Parser1 = next_col(Parser, 1, Rest),
    parse_tag_directive_handle(Parser1, Ctx);
parse_tag_directive_handle(#yaml_parser{chars = [], raw_eos = false} = Parser,
  Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, parse_tag_directive_handle);

parse_tag_directive_handle(#yaml_parser{chars = [_ | _]} = Parser,
  #tag_directive_ctx{handle = Handle, line = Line, col = Col}) ->
    %% Invalid character (or end of directive) while parsing tag handle.
    Handle1 = case Handle of
        undefined -> Handle;
        _         -> lists:reverse(Handle)
    end,
    Token = #yaml_tag_directive{
      handle = Handle1,
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_tag_directive,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Invalid tag handle in TAG directive", []),
    return(Parser1);
parse_tag_directive_handle(#yaml_parser{chars = [], raw_eos = true} = Parser,
  #tag_directive_ctx{handle = Handle, line = Line, col = Col}) ->
    %% Invalid end-of-stream while parsing major version number.
    Handle1 = case Handle of
        undefined -> Handle;
        _         -> lists:reverse(Handle)
    end,
    Token = #yaml_tag_directive{
      handle = Handle1,
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_tag_directive,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing TAG directive", []),
    return(Parser1).

%% Tag prefix.
parse_tag_directive_prefix(#yaml_parser{chars = [C | Rest]} = Parser,
  #tag_directive_ctx{prefix = Prefix} = Ctx)
  when is_list(Prefix) andalso ?IS_URI_CHAR(C) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_directive_ctx{
      prefix = [C | Prefix]
    },
    parse_tag_directive_prefix(Parser1, Ctx1);
parse_tag_directive_prefix(#yaml_parser{chars = [C | Rest]} = Parser,
  #tag_directive_ctx{prefix = undefined} = Ctx) when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_directive_ctx{
      prefix = ""
    },
    parse_tag_directive_prefix(Parser1, Ctx1);
parse_tag_directive_prefix(#yaml_parser{chars = [C | Rest]} = Parser,
  #tag_directive_ctx{prefix = ""} = Ctx) when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    Parser1 = next_col(Parser, 1, Rest),
    parse_tag_directive_prefix(Parser1, Ctx);
parse_tag_directive_prefix(
  #yaml_parser{doc_version = Version, chars = [C | _]} = Parser,
  #tag_directive_ctx{prefix = Prefix} = Ctx)
  when is_list(Prefix) andalso Prefix /= "" andalso
  (?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
   (Version == {1,1} andalso ?IS_NEWLINE_11(C))) ->
    queue_tag_directive(Parser, Ctx);
parse_tag_directive_prefix(#yaml_parser{chars = [], raw_eos = true} = Parser,
  Ctx) ->
    queue_tag_directive(Parser, Ctx);
parse_tag_directive_prefix(#yaml_parser{chars = [], raw_eos = false} = Parser,
  Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, parse_tag_directive_prefix);

parse_tag_directive_prefix(#yaml_parser{chars = [_ | _]} = Parser,
  #tag_directive_ctx{handle = Handle, prefix = Prefix,
  line = Line, col = Col}) ->
    %% Invalid character while parsing tag prefix.
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Token = #yaml_tag_directive{
      handle = lists:reverse(Handle),
      prefix = lists:reverse(Prefix),
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_tag_directive,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser2 = add_error(Parser1, Error,
      "Invalid tag prefix in TAG directive", []),
    return(Parser2).

%% Queue token.
queue_tag_directive(#yaml_parser{tags = Tags} = Parser,
  #tag_directive_ctx{handle = Handle, prefix = Prefix,
  line = Line, col = Col}) ->
    Handle1 = lists:reverse(Handle),
    Prefix1 = lists:reverse(Prefix),
    Token   = #yaml_tag_directive{
      handle = Handle1,
      prefix = Prefix1,
      line   = Line,
      column = Col
    },
    Parser1 = is_uri_valid(Parser, Token),
    Parser2 = case dict:is_key(Handle1, Tags) of
        false ->
            Parser1;
        true ->
            Error = #yaml_parser_error{
              type   = warning,
              name   = multiple_tag_handle_declarations,
              token  = Token,
              line   = Line,
              column = Col
            },
            add_error(Parser1, Error,
              "Multiple declarations of the same handle found: "
              "the last one will be used", [])
    end,
    Parser3 = queue_token(Parser2, Token),
    Tags1   = dict:store(Handle1, Prefix1, Tags),
    Parser4 = Parser3#yaml_parser{
      tags = Tags1
    },
    next_state(Parser4, fun skip_directive_trailing_ws/1).

%%
%% Reserved directive.
%%

parse_reserved_directive(Parser,
  #directive_ctx{name = Name, line = Line, col = Col}) ->
    Ctx = #reserved_directive_ctx{
      name = Name,
      line = Line,
      col  = Col
    },
    parse_reserved_directive_arg(Parser, Ctx).

parse_reserved_directive_arg(#yaml_parser{chars = [C | Rest]} = Parser,
  #reserved_directive_ctx{current = undefined} = Ctx)
  when ?IS_SPACE(C) ->
    %% Skip leading white spaces.
    Parser1 = next_col(Parser, 1, Rest),
    parse_reserved_directive_arg(Parser1, Ctx);
parse_reserved_directive_arg(
  #yaml_parser{doc_version = Version, chars = [C | _]} = Parser,
  #reserved_directive_ctx{current = undefined} = Ctx)
  when ?IS_NEWLINE(C) orelse C == $# orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% End of directive.
    queue_reserved_directive(Parser, Ctx);
parse_reserved_directive_arg(
  #yaml_parser{doc_version = Version, chars = [C | _]} = Parser,
  #reserved_directive_ctx{current = Current,
  args = Args, args_count = Count} = Ctx)
  when is_list(Current) andalso
  (?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
   (Version == {1,1} andalso ?IS_NEWLINE_11(C))) ->
    %% Current argument finished.
    Current1 = lists:reverse(Current),
    Ctx1     = Ctx#reserved_directive_ctx{
      current    = undefined,
      args       = [Current1 | Args],
      args_count = Count + 1
    },
    parse_reserved_directive_arg(Parser, Ctx1);
parse_reserved_directive_arg(#yaml_parser{chars = [C | Rest]} = Parser,
  #reserved_directive_ctx{current = Current} = Ctx) ->
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Parser2  = next_col(Parser1, 1, Rest),
    Current1 = case Current of
        undefined -> [C];
        _         -> [C | Current]
    end,
    Ctx1 = Ctx#reserved_directive_ctx{
      current = Current1
    },
    parse_reserved_directive_arg(Parser2, Ctx1);
parse_reserved_directive_arg(
  #yaml_parser{chars = [], raw_eos = true} = Parser, Ctx) ->
  %% End of directive.
    queue_reserved_directive(Parser, Ctx);
parse_reserved_directive_arg(
  #yaml_parser{chars = [], raw_eos = false} = Parser, Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, parse_reserved_directive_arg).

queue_reserved_directive(Parser,
  #reserved_directive_ctx{name = Name, current = Current,
  args = Args, args_count = Count, line = Line, col = Col}) ->
    {Args1, Count1} = case Current of
        undefined -> {Args, Count};
        _         -> {[lists:reverse(Current) | Args], Count + 1}
    end,
    Token = #yaml_reserved_directive{
      name       = Name,
      args       = lists:reverse(Args1),
      args_count = Count1,
      line       = Line,
      column     = Col
    },
    Error = #yaml_parser_error{
      type   = warning,
      name   = reserved_directive,
      token  = Token,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Reserved directive \"~s\" ignored", [Name]),
    Parser2 = queue_token(Parser1, Token),
    next_state(Parser2, fun skip_directive_trailing_ws/1).

%% -------------------------------------------------------------------
%% Block sequences.
%% -------------------------------------------------------------------

%% We found a new block sequence entry.
parse_block_entry(#yaml_parser{ik_allowed = true} = Parser)
  when ?IN_BLOCK_CTX(Parser) ->
    queue_block_sequence_entry_token(Parser);
parse_block_entry(Parser) when ?IN_BLOCK_CTX(Parser) ->
    Error = #yaml_parser_error{
      name   = block_sequence_entry_not_allowed,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Block sequence entry not allowed here", []),
    return(Parser1);
parse_block_entry(Parser) when ?IN_FLOW_CTX(Parser) ->
    Error = #yaml_parser_error{
      name   = block_collection_in_flow_context,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Block collection not allowed inside flow collection", []),
    return(Parser1).

queue_block_sequence_entry_token(
  #yaml_parser{chars = [_ | Rest]} = Parser) when ?IN_BLOCK_CTX(Parser) ->
    Parser1 = remove_impl_key_pos(Parser),
    Parser2 = allow_impl_key(Parser1, true),
    Token   = #yaml_sequence_entry{
      line   = ?CURSOR_LINE(Parser2),
      column = ?CURSOR_COLUMN(Parser2)
    },
    Parser3 = queue_token(Parser2, Token),
    Parser4 = next_col(Parser3, 1, Rest),
    next_state(Parser4, fun find_next_token/1).

%% -------------------------------------------------------------------
%% Flow collections.
%% -------------------------------------------------------------------

parse_flow_collection_start(
  #yaml_parser{chars = [_ | Rest],
    cur_coll = Cur_Coll, parent_colls = Colls} = Parser,
  Kind) ->
    Parser1 = save_impl_key_pos(Parser),
    Parser2 = push_fake_impl_key(Parser1),
    Parser3 = allow_impl_key(Parser2, true),
    Token = #yaml_collection_start{
      style  = flow,
      kind   = Kind,
      line   = ?CURSOR_LINE(Parser3),
      column = ?CURSOR_COLUMN(Parser3)
    },
    Token1   = set_default_tag(Token),
    Parser4  = queue_token(Parser3, Token1),
    New_Coll = #fcoll{kind = Kind},
    Parser5  = case Kind of
        sequence ->
            Parser4#yaml_parser{
              cur_coll      = New_Coll,
              parent_colls  = [Cur_Coll | Colls],
              pending_entry = true
            };
        mapping ->
            Parser4#yaml_parser{
              cur_coll           = New_Coll,
              parent_colls       = [Cur_Coll | Colls],
              waiting_for_kvpair = true
            }
    end,
    Parser6 = next_col(Parser5, 1, Rest),
    next_state(Parser6, fun find_next_token/1).

parse_flow_collection_end(
  #yaml_parser{cur_coll = #fcoll{kind = single_mapping}} = Parser,
  Kind) ->
    Parser1 = finish_incomplete_flow_entries(Parser),
    parse_flow_collection_end(Parser1, Kind);
parse_flow_collection_end(
  #yaml_parser{chars = [_ | Rest],
    cur_coll = #fcoll{kind = Kind}, parent_colls = [Coll | Colls]} = Parser,
  Kind) ->
    Parser1 = finish_incomplete_flow_entries(Parser),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = pop_impl_key(Parser2),
    Parser4 = Parser3#yaml_parser{
      cur_coll           = Coll,
      parent_colls       = Colls,
      pending_entry      = false,
      waiting_for_kvpair = false
    },
    Parser5 = allow_impl_key(Parser4, false),
    Token    = #yaml_collection_end{
      style  = flow,
      kind   = Kind,
      line   = ?CURSOR_LINE(Parser5),
      column = ?CURSOR_COLUMN(Parser5)
    },
    Parser6 = queue_token(Parser5, Token),
    Parser7 = next_col(Parser6, 1, Rest),
    next_state(Parser7, fun find_next_token/1);
parse_flow_collection_end(
  #yaml_parser{cur_coll = #fcoll{kind = Expected}} = Parser, Kind) ->
    %% Closing a different-type collection.
    Error = #yaml_parser_error{
      name   = closing_non_matching_flow_collection_type,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "A ~s closing character is used to close a ~s collection",
      [Kind, Expected]),
    parse_flow_collection_end(Parser1, Expected);
parse_flow_collection_end(
  #yaml_parser{chars = [_ | Rest], cur_coll = #bcoll{}} = Parser, Kind) ->
    %% Closing a never-opened collection.
    Error = #yaml_parser_error{
      name   = closing_never_opened_flow_collection,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "The ~s closing character doesn't match any opening character",
      [Kind]),
    Parser2 = next_col(Parser1, 1, Rest),
    next_state(Parser2, fun find_next_token/1).

parse_flow_entry(
  #yaml_parser{cur_coll = #fcoll{kind = single_mapping}} = Parser) ->
    Parser1 = finish_incomplete_flow_entries(Parser),
    parse_flow_entry(Parser1);
parse_flow_entry(
  #yaml_parser{chars = [_ | Rest],
  cur_coll = #fcoll{kind = Kind}} = Parser) when
  (Kind == sequence andalso ?MISSING_ENTRY(Parser)) orelse
  (Kind == mapping  andalso ?MISSING_KVPAIR(Parser)) ->
    %% In a flow collection, the "," entry indicator immediatly follows a
    %% collection-start or a previous entry indicator.
    Error = #yaml_parser_error{
      name   = flow_collection_entry_not_allowed,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Empty flow collection entry not allowed", []),
    Parser2 = next_col(Parser1, 1, Rest),
    next_state(Parser2, fun find_next_token/1);
parse_flow_entry(#yaml_parser{chars = [_ | Rest],
  cur_coll = #fcoll{kind = Kind}} = Parser) ->
    Parser1 = finish_incomplete_flow_entries(Parser),
    Parser2 = remove_impl_key_pos(Parser1),
    Parser3 = allow_impl_key(Parser2, true),
    Parser4 = case Kind of
        sequence ->
            Parser3#yaml_parser{
              pending_entry = true
            };
        mapping ->
            Parser3#yaml_parser{
              waiting_for_kvpair = true
            }
    end,
    Parser5 = next_col(Parser4, 1, Rest),
    next_state(Parser5, fun find_next_token/1);
parse_flow_entry(Parser) when ?IN_BLOCK_CTX(Parser) ->
    Error = #yaml_parser_error{
      name   = flow_collection_entry_not_allowed,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Flow collection entry not allowed outside any flow collection", []),
    return(Parser1).

%% -------------------------------------------------------------------
%% Block or flow mapping key.
%% -------------------------------------------------------------------

parse_mapping_key(#yaml_parser{ik_allowed = true} = Parser)
  when ?IN_BLOCK_CTX(Parser) ->
    %% A mapping key is allowed here.
    Parser1 = finish_incomplete_block_entries(Parser),
    queue_mapping_key_token(Parser1);
parse_mapping_key(Parser)
  when ?IN_FLOW_CTX(Parser) ->
    %% A mapping key is always allowed in flow context.
    queue_mapping_key_token(Parser);
parse_mapping_key(Parser) ->
    %% A mapping key is NOT allowed here.
    Error = #yaml_parser_error{
      name   = block_mapping_key_not_allowed,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Block mapping key not allowed here", []),
    return(Parser1).

queue_mapping_key_token(#yaml_parser{chars = [_ | Rest]} = Parser) ->
    Parser1 = remove_impl_key_pos(Parser),
    Parser2 = allow_impl_key(Parser1, ?IN_BLOCK_CTX(Parser1)),
    Token   = #yaml_mapping_key{
      line   = ?CURSOR_LINE(Parser2),
      column = ?CURSOR_COLUMN(Parser2)
    },
    Parser3 = queue_token(Parser2, Token),
    Parser4 = next_col(Parser3, 1, Rest),
    next_state(Parser4, fun find_next_token/1).

%% -------------------------------------------------------------------
%% Block or flow mapping value.
%% -------------------------------------------------------------------

%% We found a new block mapping value. We must check if an implicit key
%% is pending.
parse_mapping_value(
  #yaml_parser{ik_stack = [#impl_key{possible = true, line = Impl_Line} | _],
  line = Line, last_token_endline = Endline} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso
  Impl_Line < Endline andalso Endline == Line ->
    %% The key of this mapping is an implicit key spanning over several lines.
    %% This will raise a warning.
    Parser1 = queue_impl_key(Parser),
    queue_mapping_value_token(Parser1);
parse_mapping_value(
  #yaml_parser{
  ik_stack = [#impl_key{possible = true, line = Impl_Line} = Impl_Key | Rest],
  line = Line} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso
  Impl_Line < Line ->
    %% This is not an implicit key.
    Impl_Key1 = Impl_Key#impl_key{possible = false},
    Parser1   = Parser#yaml_parser{
      ik_stack = [Impl_Key1 | Rest]
    },
    parse_mapping_value(Parser1);
parse_mapping_value(
  #yaml_parser{ik_stack = [#impl_key{possible = true} | _]} = Parser) ->
    %% The key of this mapping is an implicit key.
    Parser1 = queue_impl_key(Parser),
    queue_mapping_value_token(Parser1);

parse_mapping_value(
  #yaml_parser{ik_allowed = true,
    cur_coll = #bcoll{kind = mapping, kidx = KIdx, vidx = VIdx}} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso KIdx > VIdx ->
    %% The key of this mapping is an explicit key, already queued.
    %% In block context, an implicit key may follow.
    Parser1 = allow_impl_key(Parser, true),
    queue_mapping_value_token(Parser1);
parse_mapping_value(
  #yaml_parser{ik_allowed = true,
    cur_coll = #bcoll{kind = mapping, kidx = KIdx, vidx = VIdx}} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso KIdx =< VIdx ->
    %% The key of this mapping is an empty node. We queue a mapping-key
    %% token followed; the empty scalar will be automatically queued.
    Token = #yaml_mapping_key{
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = queue_token(Parser, Token),
    %% In block context, an implicit key may follow.
    Parser2 = allow_impl_key(Parser1, true),
    queue_mapping_value_token(Parser2);
parse_mapping_value(
  #yaml_parser{ik_allowed = false} = Parser) when ?IN_BLOCK_CTX(Parser) ->
    Error = #yaml_parser_error{
      name   = block_mapping_value_not_allowed,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Block mapping value not allowed here", []),
    return(Parser1);

parse_mapping_value(
  #yaml_parser{cur_coll =
    #fcoll{kind = Kind, kidx = KIdx, vidx = VIdx}} = Parser)
  when ?IN_FLOW_CTX(Parser) andalso
  (Kind == mapping orelse Kind == single_mapping) andalso KIdx > VIdx ->
    %% The key of this mapping is an explicit key, already queued.
    %% In flow context, an implicit key may not follow.
    Parser1 = allow_impl_key(Parser, false),
    queue_mapping_value_token(Parser1);
parse_mapping_value(
  #yaml_parser{cur_coll =
    #fcoll{kind = Kind, kidx = KIdx, vidx = VIdx}} = Parser)
  when ?IN_FLOW_CTX(Parser) andalso
  (((Kind == mapping orelse Kind == single_mapping) andalso KIdx =< VIdx) orelse
   Kind == sequence) ->
    %% The key of this mapping is an empty node.
    Token = #yaml_mapping_key{
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = queue_token(Parser, Token),
    %% In flow context, an implicit key may not follow.
    Parser2 = allow_impl_key(Parser1, false),
    queue_mapping_value_token(Parser2).

queue_mapping_value_token(
  #yaml_parser{line = Line, col = Col, last_token_endline = Endline,
    cur_coll = #bcoll{indent = Indent}} = Parser)
  when ?IN_BLOCK_CTX(Parser) andalso Line > Endline andalso Col > Indent ->
    Token = #yaml_mapping_value{
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Error = #yaml_parser_error{
      name   = invalid_block_mapping_value_indentation,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Block mapping value's indentation (column #~b) "
      "greater than expected (column #~b)",
      [Col, Indent]),
    queue_mapping_value_token2(Parser1);
queue_mapping_value_token(Parser) ->
    queue_mapping_value_token2(Parser).

queue_mapping_value_token2(
  #yaml_parser{chars = [_ | Rest]} = Parser) ->
    Token = #yaml_mapping_value{
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = queue_token(Parser, Token),
    Parser2 = next_col(Parser1, 1, Rest),
    next_state(Parser2, fun find_next_token/1).

finish_incomplete_block_entries(
  #yaml_parser{cur_coll =
    #bcoll{kind = mapping, kidx = KIdx, vidx = VIdx}} = Parser)
  when KIdx > VIdx ->
    %% The last block mapping key has an empty node as value (and the
    %% ":" value indicator was never used). Queue a value token. The
    %% empty scalar will be automatically queued.
    Token = #yaml_mapping_value{
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    queue_token(Parser, Token);
finish_incomplete_block_entries(Parser) ->
    Parser.

finish_incomplete_flow_entries(
  #yaml_parser{cur_coll = #fcoll{kind = single_mapping},
  parent_colls = [Coll | Colls]} = Parser) ->
    %% Close single key: value pair.
    Token = #yaml_collection_end{
      style  = flow,
      kind   = mapping,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = queue_token(Parser, Token),
    Parser1#yaml_parser{
      cur_coll     = Coll,
      parent_colls = Colls
    };
finish_incomplete_flow_entries(
  #yaml_parser{cur_coll = #fcoll{kind = mapping},
  ik_stack = [#impl_key{possible = true} | _]} = Parser) ->
    %% Queue implicit key token.
    Parser1 = queue_impl_key(Parser),
    finish_incomplete_flow_entries(Parser1);
finish_incomplete_flow_entries(
  #yaml_parser{
    cur_coll = #fcoll{kind = Kind, kidx = KIdx, vidx = VIdx}} = Parser)
  when (Kind == mapping orelse Kind == single_mapping) andalso KIdx > VIdx ->
    %% In a flow mapping, the last entry was a key without the ":" value
    %% indicator. Queue the value token and the implicit empty node. If
    %% the key was an empty node (ie. only a tag node property), it'll
    %% be added automatically by queue_token/2.
    Token = #yaml_mapping_value{
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = queue_token(Parser, Token),
    finish_incomplete_flow_entries(Parser1);
finish_incomplete_flow_entries(Parser) ->
    Parser.

%% -------------------------------------------------------------------
%% Anchors and aliases.
%% -------------------------------------------------------------------

parse_anchor_or_alias(
  #yaml_parser{line = Line, col = Col, chars = [_ | Rest]} = Parser,
  Type) ->
    Ctx = #anchor_ctx{
      type = Type,
      line = Line,
      col  = Col
    },
    Parser1 = save_impl_key_pos(Parser),
    Parser2 = allow_impl_key(Parser1, false),
    Parser3 = next_col(Parser2, 1, Rest),
    do_parse_anchor_or_alias(Parser3, Ctx).

%% White spaces and flow indicators are forbidden inside anchor or alias
%% names.
do_parse_anchor_or_alias(
  #yaml_parser{doc_version = Version, chars = [C | _]} = Parser, Ctx) when
  ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse ?IS_FLOW_INDICATOR(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    queue_anchor_or_alias_token(Parser, Ctx);

do_parse_anchor_or_alias(#yaml_parser{chars = [C | Rest]} = Parser, Ctx) ->
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Parser2 = next_col(Parser1, 1, Rest),
    Ctx1    = Ctx#anchor_ctx{
      output = [C | Ctx#anchor_ctx.output]
    },
    do_parse_anchor_or_alias(Parser2, Ctx1);

do_parse_anchor_or_alias(#yaml_parser{chars = [], raw_eos = true} = Parser,
  Ctx) ->
    queue_anchor_or_alias_token(Parser, Ctx);
do_parse_anchor_or_alias(#yaml_parser{chars = [], raw_eos = false} = Parser,
  Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_anchor_or_alias).

queue_anchor_or_alias_token(Parser,
  #anchor_ctx{type = Type, line = Line, col = Col, output = Name}) ->
    Token = case Type of
        anchor ->
            #yaml_anchor{
              name   = lists:reverse(Name),
              line   = Line,
              column = Col
            };
        alias ->
            #yaml_alias{
              name   = lists:reverse(Name),
              line   = Line,
              column = Col
            }
    end,
    Parser1 = queue_token(Parser, Token),
    next_state(Parser1, fun find_next_token/1).

%% -------------------------------------------------------------------
%% Tags.
%% -------------------------------------------------------------------

parse_tag(#yaml_parser{chars = [_ | Rest], line = Line, col = Col,
  last_tag = undefined} = Parser) ->
    Ctx = #tag_ctx{
      line   = Line,
      col    = Col,
      prefix = "!",
      suffix = ""
    },
    Parser1 = save_impl_key_pos(Parser),
    Parser2 = allow_impl_key(Parser1, false),
    Parser3 = next_col(Parser2, 1, Rest),
    determine_tag_type(Parser3, Ctx);
parse_tag(#yaml_parser{last_tag = Tag} = Parser)
  when Tag /= undefined ->
    Error = #yaml_parser_error{
      name   = multiple_tag_properties,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Multiple tag properties attached to one node: "
      "the last one will be used", []),
    Parser2 = Parser1#yaml_parser{
      last_tag = undefined
    },
    parse_tag(Parser2).

%% Determine token type: verbatim tag or tag shorthand.
determine_tag_type(#yaml_parser{chars = [$< | Rest]} = Parser, Ctx) ->
    %% Verbatim tag.
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_ctx{
      prefix = undefined
    },
    parse_verbatim_tag(Parser1, Ctx1);
determine_tag_type(#yaml_parser{chars = [_ | _]} = Parser, Ctx) ->
    parse_tag_shorthand(Parser, Ctx);
determine_tag_type(#yaml_parser{chars = [], raw_eos = true} = Parser, Ctx) ->
    expand_tag(Parser, Ctx);
determine_tag_type(#yaml_parser{chars = [], raw_eos = false} = Parser, Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, determine_tag_type).

%%
%% Verbatim tag
%%

parse_verbatim_tag(#yaml_parser{chars = [$> | Rest]} = Parser, Ctx) ->
    %% End of the verbatim tag.
    Parser1 = next_col(Parser, 1, Rest),
    expand_tag(Parser1, Ctx);
parse_verbatim_tag(#yaml_parser{chars = [$! | Rest]} = Parser,
  #tag_ctx{suffix = ""} = Ctx) ->
    %% Local tag.
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_ctx{
      suffix = "!"
    },
    parse_verbatim_tag(Parser1, Ctx1);
parse_verbatim_tag(#yaml_parser{chars = [$! | Rest]} = Parser,
  #tag_ctx{suffix = Suffix, line = Line, col = Col} = Ctx) ->
    %% "!" forbidden in verbatim tag.
    Token = #yaml_tag{
      uri    = lists:reverse(Suffix),
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Invalid character in tag handle", []),
    Parser2 = next_col(Parser1, 1, Rest),
    parse_verbatim_tag(Parser2, Ctx);
parse_verbatim_tag(#yaml_parser{chars = [C | Rest]} = Parser, Ctx)
  when ?IS_URI_CHAR(C) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_ctx{
      suffix = [C | Ctx#tag_ctx.suffix]
    },
    parse_verbatim_tag(Parser1, Ctx1);
parse_verbatim_tag(#yaml_parser{chars = [_ | Rest]} = Parser,
  #tag_ctx{suffix = Suffix, line = Line, col = Col} = Ctx) ->
    %% Character not allowed in a URI.
    Token = #yaml_tag{
      uri    = lists:reverse(Suffix),
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Invalid character in tag handle", []),
    Parser2 = next_col(Parser1, 1, Rest),
    parse_verbatim_tag(Parser2, Ctx);
parse_verbatim_tag(#yaml_parser{chars = [], raw_eos = false} = Parser, Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, parse_verbatim_tag);
parse_verbatim_tag(#yaml_parser{chars = [], raw_eos = true} = Parser,
  #tag_ctx{suffix = Suffix, line = Line, col = Col}) ->
    %% Unexpected end-of-stream
    Token = #yaml_tag{
      uri    = lists:reverse(Suffix),
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = unexpected_eos,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing tag handle", []),
    return(Parser1).

%%
%% Tag shorthand.
%%

%% Tag prefix.
parse_tag_shorthand(#yaml_parser{chars = [$! | Rest]} = Parser,
  #tag_ctx{prefix = "!", suffix = Suffix} = Ctx) ->
    %% Separator between the prefix and the suffix.
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_ctx{
      prefix = "!" ++ lists:reverse(Suffix) ++ "!",
      suffix = ""
    },
    parse_tag_shorthand(Parser1, Ctx1);
parse_tag_shorthand(#yaml_parser{chars = [$! | Rest]} = Parser,
  #tag_ctx{prefix = Prefix, suffix = Suffix, line = Line, col = Col} = Ctx) ->
    %% "!" forbidden in tag.
    Token = #yaml_tag{
      uri    = Prefix ++ lists:reverse(Suffix),
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Invalid character in tag handle", []),
    Parser2 = next_col(Parser1, 1, Rest),
    parse_tag_shorthand(Parser2, Ctx);

%% Tag suffix.
parse_tag_shorthand(#yaml_parser{chars = [C | _]} = Parser, Ctx)
  when ?IS_FLOW_INDICATOR(C) ->
    %% The next character starts another token.
    expand_tag(Parser, Ctx);
parse_tag_shorthand(
  #yaml_parser{doc_version = Version, chars = [C | _]} = Parser, Ctx)
  when ?IS_NEWLINE(C) orelse ?IS_SPACE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    expand_tag(Parser, Ctx);

parse_tag_shorthand(#yaml_parser{chars = [C | Rest]} = Parser,
  #tag_ctx{suffix = Suffix} = Ctx) when ?IS_URI_CHAR(C) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#tag_ctx{
      suffix = [C | Suffix]
    },
    parse_tag_shorthand(Parser1, Ctx1);

parse_tag_shorthand(#yaml_parser{chars = [_ | Rest]} = Parser,
  #tag_ctx{prefix = Prefix, suffix = Suffix, line = Line, col = Col} = Ctx) ->
    %% Character not allowed in a URI.
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Token = #yaml_tag{
      uri    = Prefix ++ lists:reverse(Suffix),
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser2 = add_error(Parser1, Error,
      "Invalid character in tag handle", []),
    Parser3 = next_col(Parser2, 1, Rest),
    parse_tag_shorthand(Parser3, Ctx);

parse_tag_shorthand(#yaml_parser{chars = [], raw_eos = true} = Parser, Ctx) ->
    expand_tag(Parser, Ctx);
parse_tag_shorthand(#yaml_parser{chars = [], raw_eos = false} = Parser,
  Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, parse_tag_shorthand).

%% Verbatim tag.
expand_tag(Parser,
  #tag_ctx{prefix = undefined, suffix = Suffix} = Ctx) ->
    Ctx1 = Ctx#tag_ctx{
      suffix = lists:reverse(Suffix)
    },
    queue_tag_token(Parser, Ctx1);

%% Non-specific tag.
expand_tag(Parser,
  #tag_ctx{prefix = "!", suffix = ""} = Ctx) ->
    Ctx1 = Ctx#tag_ctx{
      prefix = undefined,
      suffix = {non_specific, "!"}
    },
    queue_tag_token(Parser, Ctx1);

%% Tag shorthand.
expand_tag(Parser,
  #tag_ctx{line = Line, col = Col, prefix = Prefix, suffix = ""} = Ctx) ->
    Token = #yaml_tag{
      uri    = Prefix,
      line   = Line,
      column = Col
    },
    Error1 = #yaml_parser_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error1,
      "Tag suffix mandatory", []),
    expand_tag2(Parser1, Ctx);
expand_tag(Parser, Ctx) ->
    expand_tag2(Parser, Ctx).

expand_tag2(#yaml_parser{tags = Tags} = Parser,
  #tag_ctx{prefix = Prefix, suffix = Suffix, line = Line, col = Col} = Ctx) ->
    Suffix1 = lists:reverse(Suffix),
    {Parser1, URI} = try
        case dict:is_key(Prefix, Tags) of
            true  -> {Parser, dict:fetch(Prefix, Tags) ++ Suffix1};
            false -> {Parser, dict:fetch({default, Prefix}, Tags) ++ Suffix1}
        end
    catch
        _:_ ->
            Bad_URI = Prefix ++ Suffix1,
            Token = #yaml_tag{
              uri    = Bad_URI,
              line   = Line,
              column = Col
            },
            Error = #yaml_parser_error{
              name   = undeclared_tag_handle,
              token  = Token,
              line   = ?CURSOR_LINE(Parser),
              column = ?CURSOR_COLUMN(Parser)
            },
            Parser0 = add_error(Parser, Error,
              "Tag handle \"~s\" never declared", [Prefix]),
            {Parser0, Bad_URI}
    end,
    Ctx1 = Ctx#tag_ctx{
      prefix = undefined,
      suffix = URI
    },
    queue_tag_token(Parser1, Ctx1).

queue_tag_token(Parser,
  #tag_ctx{suffix = "!", line = Line, col = Col} = Ctx) ->
    Token = #yaml_tag{
      uri    = "!",
      line   = Line,
      column = Col
    },
    Error = #yaml_parser_error{
      name   = invalid_tag_handle,
      token  = Token,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Local tag suffix mandatory", []),
    queue_tag_token2(Parser1, Ctx);
queue_tag_token(Parser, Ctx) ->
    queue_tag_token2(Parser, Ctx).

queue_tag_token2(Parser, #tag_ctx{suffix = URI, line = Line, col = Col}) ->
    Token = #yaml_tag{
      uri    = URI,
      line   = Line,
      column = Col
    },
    Parser1 = is_uri_valid(Parser, Token),
    Parser2 = queue_token(Parser1, Token),
    next_state(Parser2, fun find_next_token/1).

%% -------------------------------------------------------------------
%% Block scalars.
%% -------------------------------------------------------------------

parse_block_scalar(
  #yaml_parser{line = Line, col = Col, chars = [_ | Rest]} = Parser,
  Style) ->
    Ctx = #block_scalar_hd_ctx{
      style = Style,
      line  = Line,
      col   = Col
    },
    Parser1 = remove_impl_key_pos(Parser),
    Parser2 = allow_impl_key(Parser1, true),
    Parser3 = next_col(Parser2, 1, Rest),
    do_parse_block_scalar_header(Parser3, Ctx).

%%
%% Header parsing.
%%

%% Newline, header termination.
do_parse_block_scalar_header(
  #yaml_parser{chars = [$\r], raw_eos = false} = Parser, Ctx) ->
    %% Can't be sure it's a newline. It may be followed by a LF.
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_block_scalar_header);

do_parse_block_scalar_header(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser, Ctx)
  when ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    prepare_parse_block_scalar(Parser1, Ctx);

%% Comments.
do_parse_block_scalar_header(
  #yaml_parser{chars = [$# | Rest]} = Parser, Ctx) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1 = Ctx#block_scalar_hd_ctx{
      in_comment = true
    },
    Ctx2 = final_indent(Parser1, Ctx1),
    do_parse_block_scalar_header(Parser1, Ctx2);
do_parse_block_scalar_header(
  #yaml_parser{chars = [_ | Rest]} = Parser,
  #block_scalar_hd_ctx{in_comment = true} = Ctx) ->
    Parser1 = next_col(Parser, 1, Rest),
    do_parse_block_scalar_header(Parser1, Ctx);

%% Chomping indicator.
do_parse_block_scalar_header(
  #yaml_parser{chars = [C | Rest]} = Parser,
  #block_scalar_hd_ctx{chomp = undefined} = Ctx)
  when C == $- orelse C == $+ ->
    Parser1 = next_col(Parser, 1, Rest),
    Chomp = case C of
        $- -> strip;
        $+ -> keep
    end,
    Ctx1 = Ctx#block_scalar_hd_ctx{
      chomp = Chomp
    },
    Ctx2 = final_indent(Parser1, Ctx1),
    do_parse_block_scalar_header(Parser1, Ctx2);
do_parse_block_scalar_header(
  #yaml_parser{chars = [C | Rest]} = Parser,
  #block_scalar_hd_ctx{style = Style, line = Line, col = Col} = Ctx)
  when C == $- orelse C == $+ ->
    Token = #yaml_scalar{
      style    = block,
      substyle = Style,
      text     = "",
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = multiple_chomping_indicators,
      type   = warning,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Multiple chomping indicators specified: the last one will be used",
      []),
    Parser2 = next_col(Parser1, 1, Rest),
    Chomp = case C of
        $- -> strip;
        $+ -> keep
    end,
    Ctx1 = Ctx#block_scalar_hd_ctx{
      chomp = Chomp
    },
    Ctx2 = final_indent(Parser2, Ctx1),
    do_parse_block_scalar_header(Parser2, Ctx2);

%% Explicit indentation indicator.
do_parse_block_scalar_header(
  #yaml_parser{chars = [C | Rest]} = Parser,
  #block_scalar_hd_ctx{indent = undefined} = Ctx)
  when C >= $1 andalso C =< $9 ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#block_scalar_hd_ctx{
      indent = {tmp, C - $0}
    },
    do_parse_block_scalar_header(Parser1, Ctx1);
do_parse_block_scalar_header(
  #yaml_parser{chars = [C | Rest]} = Parser,
  #block_scalar_hd_ctx{indent = {tmp, Indent}} = Ctx)
  when C >= $1 andalso C =< $9 ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#block_scalar_hd_ctx{
      indent = {tmp, Indent * 10 + C - $0}
    },
    do_parse_block_scalar_header(Parser1, Ctx1);
do_parse_block_scalar_header(
  #yaml_parser{chars = [C | Rest]} = Parser,
  #block_scalar_hd_ctx{style = Style, line = Line, col = Col} = Ctx) 
  when C >= $1 andalso C =< $9 ->
    Token = #yaml_scalar{
      style    = block,
      substyle = Style,
      text     = "",
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = multiple_indent_indicators,
      type   = warning,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Multiple indent indicators specified: the last one will be used",
      []),
    Parser2 = next_col(Parser1, 1, Rest),
    Ctx1 = Ctx#block_scalar_hd_ctx{
      indent = {tmp, C - $0}
    },
    Ctx2 = final_indent(Parser2, Ctx1),
    do_parse_block_scalar_header(Parser2, Ctx2);

%% Trailing spaces.
do_parse_block_scalar_header(
  #yaml_parser{chars = [C | Rest]} = Parser, Ctx)
  when ?IS_SPACE(C) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = final_indent(Parser1, Ctx),
    do_parse_block_scalar_header(Parser1, Ctx1);

%% Invalid characters.
do_parse_block_scalar_header(
  #yaml_parser{chars = [_ | Rest]} = Parser,
  #block_scalar_hd_ctx{style = Style, line = Line, col = Col} = Ctx) ->
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Token = #yaml_scalar{
      style    = block,
      substyle = Style,
      text     = "",
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = invalid_block_scalar_header,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser2 = add_error(Parser1, Error,
      "Invalid character in block scalar header", []),
    Parser3 = next_col(Parser2, 1, Rest),
    Ctx1 = final_indent(Parser3, Ctx),
    do_parse_block_scalar_header(Parser3, Ctx1);

do_parse_block_scalar_header(
  #yaml_parser{chars = [], raw_eos = true} = Parser, Ctx) ->
    %% End-of-stream reached while parsing block scalar header. Assume
    %% an empty string.
    prepare_parse_block_scalar(Parser, Ctx);
do_parse_block_scalar_header(
  #yaml_parser{chars = []} = Parser, Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_block_scalar_header).

final_indent(
  #yaml_parser{cur_coll = #bcoll{kind = root}},
  #block_scalar_hd_ctx{indent = {tmp, Add_Indent}} = Ctx) ->
    Ctx#block_scalar_hd_ctx{indent = 1 + Add_Indent};
final_indent(
  #yaml_parser{cur_coll = #bcoll{indent = Indent}},
  #block_scalar_hd_ctx{indent = {tmp, Add_Indent}} = Ctx) ->
    Ctx#block_scalar_hd_ctx{indent = Indent + Add_Indent};
final_indent(_, Ctx) ->
    Ctx.

prepare_parse_block_scalar(Parser, Ctx) ->
    Ctx1  = final_indent(Parser, Ctx),
    Chomp = case Ctx1#block_scalar_hd_ctx.chomp of
        undefined -> clip;
        C         -> C
    end,
    Next_Ctx = #block_scalar_ctx{
      style   = Ctx1#block_scalar_hd_ctx.style,
      line    = Ctx1#block_scalar_hd_ctx.line,
      col     = Ctx1#block_scalar_hd_ctx.col,
      endline = ?CURSOR_LINE(Parser),
      endcol  = ?CURSOR_COLUMN(Parser),
      chomp   = Chomp,
      indent  = Ctx1#block_scalar_hd_ctx.indent,
      newline = Ctx1#block_scalar_hd_ctx.indent /= undefined
    },
    do_parse_block_scalar(Parser, Next_Ctx).

%%
%% Newlines.
%%

%% Can't be sure it's a newline. It may be followed by a LF.
do_parse_block_scalar(
  #yaml_parser{chars = [$\r], raw_eos = false} = Parser, Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_block_scalar);

%% This is an empty line just after the header.
do_parse_block_scalar(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser,
  #block_scalar_ctx{newline = false, spaces = Spaces, output = ""} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    Ctx1    = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\n | Spaces]
    },
    do_parse_block_scalar(Parser1, Ctx1);

%% Literal style: no line folding.
do_parse_block_scalar(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser,
  #block_scalar_ctx{spaces = Spaces, style = literal} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    Ctx1    = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\n | Spaces]
    },
    do_parse_block_scalar(Parser1, Ctx1);

%% Folded style: a newline at the end of a normal-indented line.
do_parse_block_scalar(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser,
  #block_scalar_ctx{spaces = Spaces, newline = false,
  more_indent = false} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    Ctx1    = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\s | Spaces]
    },
    do_parse_block_scalar(Parser1, Ctx1);

%% Folded style: an empty line after a normal-indented line.
do_parse_block_scalar(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser,
  #block_scalar_ctx{spaces = Spaces, newline = true,
  more_indent = false} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    Spaces1 = case Spaces of
        [$\s | S] -> S;
        _         -> Spaces
    end,
    Ctx1 = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\n | Spaces1]
    },
    do_parse_block_scalar(Parser1, Ctx1);

%% Folded style: a newline in a more-indented paragraph.
do_parse_block_scalar(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser,
  #block_scalar_ctx{spaces = Spaces, more_indent = true} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    Ctx1    = Ctx#block_scalar_ctx{
      newline = true,
      spaces  = [$\n | Spaces]
    },
    do_parse_block_scalar(Parser1, Ctx1);

%%
%% Indentation.
%%

%% First non-space character: set indentation.
do_parse_block_scalar(
  #yaml_parser{chars = [C | _], col = Col} = Parser,
  #block_scalar_ctx{indent = undefined, longest_empty = Longest} = Ctx)
  when C /= $\s andalso Longest < Col ->
    Ctx1 = Ctx#block_scalar_ctx{
      indent  = Col,
      newline = true
    },
    do_parse_block_scalar(Parser, Ctx1);
do_parse_block_scalar(
  #yaml_parser{chars = [C | _], col = Indent} = Parser,
  #block_scalar_ctx{indent = undefined, longest_empty = Longest,
  style = Style, line = Line, col = Col} = Ctx)
  when C /= $\s andalso Longest >= Indent ->
    Token = #yaml_scalar{
      style    = block,
      substyle = Style,
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      type   = warning,
      name   = leading_empty_lines_too_long,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "A leading all-space line has too many spaces (~b) "
      "compared to detected indentation (~b)", [Longest, Indent - 1]),
    Ctx1 = Ctx#block_scalar_ctx{
      longest_empty = 0
    },
    do_parse_block_scalar(Parser1, Ctx1);
do_parse_block_scalar(
  #yaml_parser{chars = [$\s | Rest], col = Col} = Parser,
  #block_scalar_ctx{indent = undefined, longest_empty = Longest} = Ctx) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = if
        Col > Longest -> Ctx#block_scalar_ctx{longest_empty = Col};
        true          -> Ctx
    end,
    do_parse_block_scalar(Parser1, Ctx1);

%% Skip indentation spaces.
do_parse_block_scalar(
  #yaml_parser{chars = [$\s | Rest], col = Col} = Parser,
  #block_scalar_ctx{indent = Indent, newline = true} = Ctx)
  when Indent == undefined orelse Col < Indent ->
    Parser1 = next_col(Parser, 1, Rest),
    do_parse_block_scalar(Parser1, Ctx);

%% The next line is less indented than the block scalar: end it.
do_parse_block_scalar(
  #yaml_parser{chars = [C | _],
    cur_coll = #bcoll{indent = Indent}, col = Col} = Parser, Ctx)
  when C /= $\s andalso Col =< Indent ->
    queue_block_scalar_token(Parser, Ctx);

%% The next line is less indented than the block scalar, but more than
%% the parent node. However, it's a comment, so we end the scalar.
do_parse_block_scalar(
  #yaml_parser{chars = [$# | _], col = Col} = Parser,
  #block_scalar_ctx{indent = Indent} = Ctx)
  when Col < Indent ->
    queue_block_scalar_token(Parser, Ctx);

%% The next line is less indented than the block scalar, but more than
%% the parent node: it's an error.
do_parse_block_scalar(
  #yaml_parser{chars = [C | _], col = Col} = Parser,
  #block_scalar_ctx{indent = Indent, style = Style,
    line = Token_Line, col = Token_Col, output = Output})
  when C /= $\s andalso Col < Indent ->
    Token = #yaml_scalar{
      style    = block,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Token_Line,
      column   = Token_Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = invalid_block_scalar_indentation,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Invalid block scalar indentation", []),
    return(Parser1);

%% The next line has a directives end or document end marker: end the
%% scalar.
do_parse_block_scalar(
  #yaml_parser{chars = [C | _], chars_len = Len,
    raw_eos = false, col = 1} = Parser,
  Ctx) when (C == $- orelse C == $.) andalso Len < 4 ->
    %% We don't have enough data to determine if it's the end of the
    %% plain scalar.
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_block_scalar);
do_parse_block_scalar(
  #yaml_parser{doc_version = Version,
    chars = [$-, $-, $-, C | _], col = 1} = Parser, Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    queue_block_scalar_token(Parser, Ctx);
do_parse_block_scalar(
  #yaml_parser{doc_version = Version,
    chars = [$., $., $., C | _], col = 1} = Parser, Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    queue_block_scalar_token(Parser, Ctx);
do_parse_block_scalar(
  #yaml_parser{chars = [$-, $-, $-], raw_eos = true, col = 1} = Parser, Ctx) ->
    queue_block_scalar_token(Parser, Ctx);
do_parse_block_scalar(
  #yaml_parser{chars = [$., $., $.], raw_eos = true, col = 1} = Parser, Ctx) ->
    queue_block_scalar_token(Parser, Ctx);

%%
%% Content.
%%

%% Literal style: everything after the indentation spaces is kept.
do_parse_block_scalar(
  #yaml_parser{chars = [C | Rest]} = Parser,
  #block_scalar_ctx{style = literal, spaces = Spaces} = Ctx) ->
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Parser2 = next_col(Parser1, 1, Rest),
    Ctx1    = Ctx#block_scalar_ctx{
      spaces  = "",
      newline = false,
      output  = [C | Spaces ++ Ctx#block_scalar_ctx.output],
      endline = ?CURSOR_LINE(Parser2),
      endcol  = ?CURSOR_COLUMN(Parser2)
    },
    do_parse_block_scalar(Parser2, Ctx1);

%% Folded style: a normal-indented line.
do_parse_block_scalar(
  #yaml_parser{chars = [C | _], col = Col} = Parser,
  #block_scalar_ctx{style = folded, more_indent = true, indent = Indent} = Ctx)
  when not ?IS_SPACE(C) andalso Col == Indent ->
    %% This line uses the default indentation: end the more indented
    %% paragraph.
    Ctx1 = Ctx#block_scalar_ctx{
      more_indent = false
    },
    do_parse_block_scalar(Parser, Ctx1);
do_parse_block_scalar(
  #yaml_parser{chars = [C | Rest]} = Parser,
  #block_scalar_ctx{style = folded, newline = Newline, spaces = Spaces,
    output = Output} = Ctx)
  when not ?IS_SPACE(C) orelse
  (?IS_SPACE(C) andalso (not Newline orelse Output == "")) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#block_scalar_ctx{
      spaces  = "",
      newline = false,
      output  = [C | Spaces ++ Ctx#block_scalar_ctx.output],
      endline = ?CURSOR_LINE(Parser1),
      endcol  = ?CURSOR_COLUMN(Parser1)
    },
    do_parse_block_scalar(Parser1, Ctx1);

%% Folded style: a more-indented line.
do_parse_block_scalar(
  #yaml_parser{chars = [C | Rest]} = Parser,
  #block_scalar_ctx{style = folded, newline = true, spaces = Spaces,
    more_indent = More_Indented} = Ctx)
  when ?IS_SPACE(C) ->
    Parser1 = next_col(Parser, 1, Rest),
    Spaces1 = case Spaces of
        [$\s | S]            -> [$\n | S];
        _ when More_Indented -> Spaces;
        _                    -> [$\n | Spaces]
    end,
    Ctx1 = Ctx#block_scalar_ctx{
      spaces      = "",
      newline     = false,
      more_indent = true,
      output      = [C | Spaces1 ++ Ctx#block_scalar_ctx.output],
      endline     = ?CURSOR_LINE(Parser1),
      endcol      = ?CURSOR_COLUMN(Parser1)
    },
    do_parse_block_scalar(Parser1, Ctx1);

do_parse_block_scalar(
  #yaml_parser{chars = [], raw_eos = true} = Parser, Ctx) ->
    %% End-of-stream reached.
    queue_block_scalar_token(Parser, Ctx);
do_parse_block_scalar(
  #yaml_parser{chars = []} = Parser, Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_block_scalar).

queue_block_scalar_token(Parser,
  #block_scalar_ctx{style = Style, output = Output, spaces = Spaces,
  chomp = Chomp, newline = Newline, line = Line, col = Col,
  endline = Endline, endcol = Endcol}) ->
    {Text, Endline1, Endcol1} = case Chomp of
        strip                   -> {Output, Endline, Endcol};
        clip when Output == ""  -> {Output, Endline, Endcol};
        clip when Spaces == ""  -> {Output, Endline, Endcol};
        clip                    -> {[$\n | Output], Endline + 1, 1};
        keep                    -> {Spaces ++ Output, ?CURSOR_LINE(Parser), 1}
    end,
    Token = #yaml_scalar{
      style    = block,
      substyle = Style,
      text     = lists:reverse(Text),
      line     = Line,
      column   = Col
    },
    Token1  = set_default_tag(Token),
    Parser1 = queue_token(Parser, Token1),
    Parser2 = Parser1#yaml_parser{
      endpos_set_by_token = true,
      last_token_endline  = Endline1,
      last_token_endcol   = Endcol1,
      missed_nl           = Newline
    },
    next_state(Parser2, fun find_next_token/1).

%% -------------------------------------------------------------------
%% Flow scalars.
%% -------------------------------------------------------------------

parse_flow_scalar(
  #yaml_parser{line = Line, col = Col, chars = [C | Rest]} = Parser,
  Style) ->
    %% We start a flow scalar parsing: initialize the context.
    Ctx = #flow_scalar_ctx{
      style = Style,
      line  = Line,
      col   = Col
    },
    Parser1 = save_impl_key_pos(Parser),
    Parser2 = allow_impl_key(Parser1, false),
    Parser3 = case C of
        $' -> next_col(Parser2, 1, Rest);
        $" -> next_col(Parser2, 1, Rest);
        _  -> Parser2
    end,
    do_parse_flow_scalar(Parser3, Ctx).

do_parse_flow_scalar(
  #yaml_parser{chars = [$\r], raw_eos = false} = Parser, Ctx) ->
    %% Can't be sure it's a newline. It may be followed by a LF.
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_flow_scalar);

%%
%% Leading white spaces (plain scalar).
%%

do_parse_flow_scalar(#yaml_parser{chars = [C | Rest]} = Parser,
  #flow_scalar_ctx{style = plain, output = "", next_escaped = false,
  surrogate = undefined} = Ctx) when ?IS_SPACE(C) ->
    %% Skip leading white spaces in a plain scalar. We must update the
    %% position of beginning of the scalar and despite the implicit key.
    Parser1 = next_col(Parser, 1, Rest),
    Parser2 = save_impl_key_pos(Parser1),
    Ctx1    = Ctx#flow_scalar_ctx{
      line = ?CURSOR_LINE(Parser1),
      col  = ?CURSOR_COLUMN(Parser1)
    },
    do_parse_flow_scalar(Parser2, Ctx1);

%%
%% Escaped characters [62].
%% Only supported by double-quoted strings.
%%

%% The next character is escaped.
do_parse_flow_scalar(#yaml_parser{chars = [$\\ | Rest]} = Parser,
  #flow_scalar_ctx{style = double_quoted, next_escaped = false,
  spaces = Spaces} = Ctx) ->
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#flow_scalar_ctx{
      next_escaped = true,
      spaces       = "",
      newline      = false,
      output       = Spaces ++ Ctx#flow_scalar_ctx.output
    },
    do_parse_flow_scalar(Parser1, Ctx1);

%% Escaped 16-bit Unicode character, \uFFFF [60].
do_parse_flow_scalar(
  #yaml_parser{chars = [$u | _], chars_len = Len, raw_eos = true} = Parser,
  #flow_scalar_ctx{next_escaped = true, style = Style, line = Line, col = Col,
    output = Output})
  when Len < 5 ->
    %% Unexpected enf-of-stream.
    Token = #yaml_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = unexpected_eos,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing flow scalar", []),
    return(Parser1);
do_parse_flow_scalar(
  #yaml_parser{chars = [$u | _], chars_len = Len} = Parser,
  #flow_scalar_ctx{next_escaped = true} = Ctx)
  when Len < 5 ->
    %% Can't be sure it's an escaped Unicode character.
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_flow_scalar);
do_parse_flow_scalar(
  #yaml_parser{chars = [$u, O1, O2, O3, O4 | Rest]} = Parser,
  #flow_scalar_ctx{next_escaped = true, surrogate = High} = Ctx) when
  ?IS_HEXADECIMAL(O1) andalso ?IS_HEXADECIMAL(O2) andalso
  ?IS_HEXADECIMAL(O3) andalso ?IS_HEXADECIMAL(O4) ->
    Parser1 = next_col(Parser, 5, Rest),
    C       = hex_to_dec([O1, O2, O3, O4], 0),
    Ctx1    = case High of
        undefined ->
            if
                ?IS_HIGH_SURROGATE(C) ->
                    %% This is the high part of a UTF-16 surrogate pair.
                    Ctx#flow_scalar_ctx{
                      surrogate = C
                    };
                true ->
                    %% Normal character.
                    Ctx#flow_scalar_ctx{
                      output = [C | Ctx#flow_scalar_ctx.output]
                    }
            end;
        _ ->
            if
                ?IS_LOW_SURROGATE(C) ->
                    %% This is the low part of a UTF-16 surrogate pair.
                    C1 = 16#10000 + (High - 16#d800) * 16#400 + (C - 16#dc00),
                    Ctx#flow_scalar_ctx{
                      output    = [C1 | Ctx#flow_scalar_ctx.output],
                      surrogate = undefined
                    };
                true ->
                    %% Error: high surrogate without a low surrogate.
                    %% The error is generated by the next clause.
                    Ctx
            end
    end,
    Ctx2 = Ctx1#flow_scalar_ctx{
      next_escaped = false
    },
    do_parse_flow_scalar(Parser1, Ctx2);
do_parse_flow_scalar(
  #yaml_parser{chars = [$u | _]} = Parser,
  #flow_scalar_ctx{next_escaped = true, line = Line, col = Col,
  style = Style, output = Output} = Ctx) ->
    %% Invalid escaped character.
    Token = #yaml_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = invalid_escaped_character,
      type   = warning,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser) - 1
    },
    Parser1 = add_error(Parser, Error,
      "Invalid escaped character", []),
    Ctx1 = Ctx#flow_scalar_ctx{
      next_escaped = false
    },
    do_parse_flow_scalar(Parser1, Ctx1);

%% Escaped 32-bit Unicode character, \UFFFFFFFF [61].
do_parse_flow_scalar(
  #yaml_parser{chars = [$U | _], chars_len = Len, raw_eos = true} = Parser,
  #flow_scalar_ctx{next_escaped = true, style = Style, line = Line, col = Col,
    output = Output})
  when Len < 9 ->
    %% Unexpected enf-of-stream.
    Token = #yaml_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = unexpected_eos,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing flow scalar", []),
    return(Parser1);
do_parse_flow_scalar(
  #yaml_parser{chars = [$U | _], chars_len = Len} = Parser,
  #flow_scalar_ctx{next_escaped = true} = Ctx)
  when Len < 9 ->
    %% Can't be sure it's an escaped Unicode character.
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_flow_scalar);
do_parse_flow_scalar(
  #yaml_parser{chars = [$U, O1, O2, O3, O4, O5, O6, O7, O8 | Rest]} = Parser,
  #flow_scalar_ctx{next_escaped = true, surrogate = High} = Ctx) when
  ?IS_HEXADECIMAL(O1) andalso ?IS_HEXADECIMAL(O2) andalso
  ?IS_HEXADECIMAL(O3) andalso ?IS_HEXADECIMAL(O4) andalso
  ?IS_HEXADECIMAL(O5) andalso ?IS_HEXADECIMAL(O6) andalso
  ?IS_HEXADECIMAL(O7) andalso ?IS_HEXADECIMAL(O8) ->
    Parser1 = next_col(Parser, 9, Rest),
    C       = hex_to_dec([O1, O2, O3, O4, O5, O6, O7, O8], 0),
    Ctx1    = case High of
        undefined ->
            if
                ?IS_HIGH_SURROGATE(C) ->
                    %% This is the high part of a UTF-16 surrogate pair.
                    Ctx#flow_scalar_ctx{
                      surrogate = C
                    };
                true ->
                    %% Normal character.
                    Ctx#flow_scalar_ctx{
                      output = [C | Ctx#flow_scalar_ctx.output]
                    }
            end;
        _ ->
            if
                ?IS_LOW_SURROGATE(C) ->
                    %% This is the low part of a UTF-16 surrogate pair.
                    C1 = 16#10000 + (High - 16#d800) * 16#400 + (C - 16#dc00),
                    Ctx#flow_scalar_ctx{
                      output    = [C1 | Ctx#flow_scalar_ctx.output],
                      surrogate = undefined
                    };
                true ->
                    %% Error: high surrogate without a low surrogate.
                    %% The error is generated by the next clause.
                    Ctx
            end
    end,
    Ctx2 = Ctx1#flow_scalar_ctx{
      next_escaped = false
    },
    do_parse_flow_scalar(Parser1, Ctx2);
do_parse_flow_scalar(
  #yaml_parser{chars = [$U | _]} = Parser,
  #flow_scalar_ctx{next_escaped = true, line = Line, col = Col,
  style = Style, output = Output} = Ctx) ->
    %% Invalid escaped character.
    Token = #yaml_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = invalid_escaped_character,
      type   = warning,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser) - 1
    },
    Parser1 = add_error(Parser, Error,
      "Invalid escaped character", []),
    Ctx1 = Ctx#flow_scalar_ctx{
      next_escaped = false
    },
    do_parse_flow_scalar(Parser1, Ctx1);

%% Invalid surrogate pair.
do_parse_flow_scalar(#yaml_parser{chars = [_ | _]} = Parser,
  #flow_scalar_ctx{surrogate = High, style = Style,
    line = Line, col = Col, output = Output} = Ctx)
  when High /= undefined ->
    %% The next character isn't the expected low surrogate.
    Token = #yaml_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = invalid_surrogate_pair,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Invalid UTF-16 surrogate pair", []),
    Ctx1 = Ctx#flow_scalar_ctx{
      surrogate = undefined
    },
    do_parse_flow_scalar(Parser1, Ctx1);

%% Escaped 8-bit Unicode character, \xFF [59].
do_parse_flow_scalar(
  #yaml_parser{chars = [$x | _], chars_len = Len, raw_eos = true} = Parser,
  #flow_scalar_ctx{next_escaped = true, style = Style, line = Line, col = Col,
    output = Output})
  when Len < 3 ->
    %% Unexpected enf-of-stream.
    Token = #yaml_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = unexpected_eos,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing flow scalar", []),
    return(Parser1);
do_parse_flow_scalar(
  #yaml_parser{chars = [$x | _], chars_len = Len} = Parser,
  #flow_scalar_ctx{next_escaped = true} = Ctx)
  when Len < 3 ->
    %% Can't be sure it's an escaped Unicode character.
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_flow_scalar);
do_parse_flow_scalar(
  #yaml_parser{chars = [$x, O1, O2 | Rest]} = Parser,
  #flow_scalar_ctx{next_escaped = true} = Ctx) when
  ?IS_HEXADECIMAL(O1) andalso ?IS_HEXADECIMAL(O2) ->
    Parser1 = next_col(Parser, 3, Rest),
    C       = hex_to_dec([O1, O2], 0),
    Ctx1    = Ctx#flow_scalar_ctx{
      next_escaped = false,
      output       = [C | Ctx#flow_scalar_ctx.output]
    },
    do_parse_flow_scalar(Parser1, Ctx1);
do_parse_flow_scalar(
  #yaml_parser{chars = [$x | _]} = Parser,
  #flow_scalar_ctx{next_escaped = true, line = Line, col = Col,
  style = Style, output = Output} = Ctx) ->
    %% Invalid escaped character.
    Token = #yaml_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = invalid_escaped_character,
      type   = warning,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser) - 1
    },
    Parser1 = add_error(Parser, Error,
      "Invalid escaped character", []),
    Ctx1 = Ctx#flow_scalar_ctx{
      next_escaped = false
    },
    do_parse_flow_scalar(Parser1, Ctx1);

%% Escaped newline.
%% All trailing whitespaces are kept as content before an escaped
%% newline: this is handled in the $\\ clause above.
do_parse_flow_scalar(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser,
  #flow_scalar_ctx{style = double_quoted, next_escaped = true} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    Ctx1    = Ctx#flow_scalar_ctx{
      next_escaped = false,
      newline      = true
    },
    do_parse_flow_scalar(Parser1, Ctx1);

%% Other escaped characters.
do_parse_flow_scalar(#yaml_parser{chars = [C | Rest]} = Parser,
  #flow_scalar_ctx{next_escaped = true, line = Line, col = Col,
  style = Style, output = Output} = Ctx) ->
    Parser1 = warn_if_non_ascii_line_break(Parser),
    case unescape_char(C) of
        undefined ->
            %% Invalid escaped character.
            Token = #yaml_scalar{
              style    = flow,
              substyle = Style,
              text     = lists:reverse(Output),
              line     = Line,
              column   = Col
            },
            Token1 = set_default_tag(Token),
            Error  = #yaml_parser_error{
              name   = invalid_escaped_character,
              type   = warning,
              token  = Token1,
              line   = ?CURSOR_LINE(Parser),
              column = ?CURSOR_COLUMN(Parser) - 1
            },
            Parser2 = add_error(Parser1, Error,
              "Invalid escaped character", []),
            Ctx1 = Ctx#flow_scalar_ctx{
              next_escaped = false
            },
            do_parse_flow_scalar(Parser2, Ctx1);
        C1 ->
            Parser2 = next_col(Parser1, 1, Rest),
            Ctx1    = Ctx#flow_scalar_ctx{
              next_escaped = false,
              output       = [C1 | Ctx#flow_scalar_ctx.output]
            },
            do_parse_flow_scalar(Parser2, Ctx1)
    end;

%% In a single-quoted string, a single quote is escaped by doubling
%% it.
do_parse_flow_scalar(
  #yaml_parser{chars = [$'], raw_eos = false} = Parser,
  #flow_scalar_ctx{style = single_quoted} = Ctx) ->
    %% Can't be sure it's an escaped single quote.
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_flow_scalar);
do_parse_flow_scalar(#yaml_parser{chars = [$', $' | Rest]} = Parser,
  #flow_scalar_ctx{style = single_quoted, spaces = Spaces} = Ctx) ->
    Parser1 = next_col(Parser, 2, Rest),
    Ctx1    = Ctx#flow_scalar_ctx{
      spaces  = "",
      newline = false,
      output  = [$' | Spaces ++ Ctx#flow_scalar_ctx.output]
    },
    do_parse_flow_scalar(Parser1, Ctx1);

%%
%% Line folding.
%% Leading and trailing white spaces are dropped, except when the
%% newline character is escaped, a previous white spaces is escaped or
%% at the beginning of the first line.
%%

do_parse_flow_scalar(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser,
  #flow_scalar_ctx{newline = false} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    Ctx1    = Ctx#flow_scalar_ctx{
      spaces  = " ",
      newline = true
    },
    do_parse_flow_scalar(Parser1, Ctx1);

do_parse_flow_scalar(
  #yaml_parser{doc_version = Version, chars = [C | _] = Chars} = Parser,
  #flow_scalar_ctx{newline = true, spaces = Spaces} = Ctx)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    Parser1 = next_line(Parser, Chars),
    Spaces1 = case Spaces of
        [$\s | S] -> S;
        _         -> Spaces
    end,
    Ctx1 = Ctx#flow_scalar_ctx{
      spaces = [$\n | Spaces1]
    },
    do_parse_flow_scalar(Parser1, Ctx1);

do_parse_flow_scalar(#yaml_parser{chars = [C | Rest]} = Parser,
  #flow_scalar_ctx{spaces = Spaces, newline = false} = Ctx)
  when ?IS_SPACE(C) ->
    %% Keep white spaces in a separate buffer. If we find content later,
    %% this buffer will be merged with the result buffer. Otherwise, the
    %% white spaces buffer may be trimmed or dropped.
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#flow_scalar_ctx{
      spaces = [C | Spaces]
    },
    do_parse_flow_scalar(Parser1, Ctx1);

do_parse_flow_scalar(#yaml_parser{chars = [C | Rest]} = Parser,
  #flow_scalar_ctx{newline = true} = Ctx)
  when ?IS_SPACE(C) ->
    %% Drop leading white spaces when not on the first line.
    Parser1 = next_col(Parser, 1, Rest),
    do_parse_flow_scalar(Parser1, Ctx);

%%
%% Flow scalar end character.
%%

do_parse_flow_scalar(#yaml_parser{chars = [C | Rest]} = Parser,
  #flow_scalar_ctx{style = Style} = Ctx) when
  (Style == double_quoted andalso C == $") orelse
  (Style == single_quoted andalso C == $') ->
    %% Found the end of this flow scalar. Next step: find the next
    %% token.
    Parser1 = next_col(Parser, 1, Rest),
    Ctx1    = Ctx#flow_scalar_ctx{
      endline = ?CURSOR_LINE(Parser1),
      endcol  = ?CURSOR_COLUMN(Parser1)
    },
    queue_flow_scalar_token(Parser1, Ctx1);

do_parse_flow_scalar(#yaml_parser{chars = [$# | _]} = Parser,
  #flow_scalar_ctx{style = plain, spaces = Spaces} = Ctx) when Spaces /= [] ->
    %% A '#' character preceeded by white spaces is a comment. The plain
    %% scalar terminates with the first white spaces because trailing
    %% white spaces are ignored. [130]
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar(#yaml_parser{chars = [$:], raw_eos = true} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% We consider the end-of-stream as a "white space" and use the ':'
    %% character as the end character for this plain scalar. [130]
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar(#yaml_parser{chars = [$:], raw_eos = false} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% We don't have enough data to determine if it's the end of the
    %% plain scalar.
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_flow_scalar);
do_parse_flow_scalar(
  #yaml_parser{doc_version = Version, chars = [$:, C | _]} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% A ':' character followed by white spaces is not allowed in a
    %% plain scalar: end it. Only one character is available but it's
    %% enough to take a decision. The next state will handle the newline
    %% properly. [130]
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar(#yaml_parser{chars = [$:, C | _]} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IN_FLOW_CTX(Parser) andalso ?IS_FLOW_INDICATOR(C) ->
    %% A ':' character followed by an flow indicator character in flow
    %% context ends the plain scalar.
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar(#yaml_parser{chars = [C | _]} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IN_FLOW_CTX(Parser) andalso ?IS_FLOW_INDICATOR(C) ->
    %% The characters '[', ']', '{', '}' and ',' are forbidden in plain
    %% scalar because they are used as flow collection separation
    %% characters. [129]
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar(
  #yaml_parser{chars = [C | _], chars_len = Len,
    raw_eos = false, col = 1} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when (C == $- orelse C == $.) andalso Len < 4 ->
    %% We don't have enough data to determine if it's the end of the
    %% plain scalar.
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_flow_scalar);
do_parse_flow_scalar(
  #yaml_parser{doc_version = Version,
    chars = [$-, $-, $-, C | _], col = 1} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% A directives end indicator puts an end to the plain scalar.
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar(
  #yaml_parser{doc_version = Version,
    chars = [$., $., $., C | _], col = 1} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx)
  when ?IS_SPACE(C) orelse ?IS_NEWLINE(C) orelse
  (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% A document end indicator puts an end to the plain scalar.
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar(
  #yaml_parser{chars = [$-, $-, $-], raw_eos = true, col = 1} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% A directives end indicator puts an end to the plain scalar.
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});
do_parse_flow_scalar(
  #yaml_parser{chars = [$., $., $.], raw_eos = true, col = 1} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% A document end indicator puts an end to the plain scalar.
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar(#yaml_parser{chars = [_ | _],
    cur_coll = #bcoll{indent = Indent}, col = Col} = Parser,
  #flow_scalar_ctx{style = plain, newline = true} = Ctx)
  when ?IN_BLOCK_CTX(Parser) andalso Col =< Indent ->
    %% The continuation line is as or less indented than the current
    %% block collection. Therefore, it's not a continuation line and we
    %% end the flow scalar.
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});

do_parse_flow_scalar(#yaml_parser{chars = [], raw_eos = true} = Parser,
  #flow_scalar_ctx{style = plain} = Ctx) ->
    %% End of stream = end of plain scalar.
    queue_flow_scalar_token(Parser, Ctx#flow_scalar_ctx{spaces = ""});

%%
%% JSON acceptable characters range [2].
%%

do_parse_flow_scalar(#yaml_parser{chars = [C | Rest]} = Parser,
  #flow_scalar_ctx{spaces = Spaces} = Ctx)
  when C == 16#9 orelse (C >= 16#20 andalso C =< 16#10FFFF) ->
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Parser2 = next_col(Parser1, 1, Rest),
    Ctx1    = Ctx#flow_scalar_ctx{
      spaces  = "",
      newline = false,
      output  = [C | Spaces ++ Ctx#flow_scalar_ctx.output],
      endline = ?CURSOR_LINE(Parser2),
      endcol  = ?CURSOR_COLUMN(Parser2)
    },
    do_parse_flow_scalar(Parser2, Ctx1);

do_parse_flow_scalar(#yaml_parser{chars = [], raw_eos = true} = Parser,
  #flow_scalar_ctx{style = Style, line = Line, col = Col, output = Output}) ->
    Token = #yaml_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Output),
      line     = Line,
      column   = Col
    },
    Token1 = set_default_tag(Token),
    Error  = #yaml_parser_error{
      name   = unexpected_eos,
      token  = Token1,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected end-of-stream while parsing flow scalar", []),
    return(Parser1);
do_parse_flow_scalar(#yaml_parser{chars = []} = Parser, Ctx) ->
    ?SUSPEND_SUBPARSING(Parser, Ctx, do_parse_flow_scalar).

queue_flow_scalar_token(Parser,
  #flow_scalar_ctx{style = Style, output = Output, spaces = Spaces,
  newline = Newline, line = Line, col = Col,
  endline = Endline, endcol = Endcol}) ->
    Token = #yaml_scalar{
      style    = flow,
      substyle = Style,
      text     = lists:reverse(Spaces ++ Output),
      line     = Line,
      column   = Col
    },
    Token1  = set_default_tag(Token),
    Parser1 = queue_token(Parser, Token1),
    Parser2 = Parser1#yaml_parser{
      endpos_set_by_token = true,
      last_token_endline  = Endline,
      last_token_endcol   = Endcol,
      missed_nl           = (Style == plain andalso Newline)
    },
    next_state(Parser2, fun find_next_token/1).

unescape_char($0)  -> 16#0;    %% \0 = NUL                        [42]
unescape_char($a)  -> 16#7;    %% \7 = BELL                       [43]
unescape_char($b)  -> $\b;     %% \b = BS                         [44]
unescape_char($t)  -> $\t;     %% \t = TAB                        [45]
unescape_char($n)  -> $\n;     %% \n = LF                         [46]
unescape_char($v)  -> $\v;     %% \v = VT                         [47]
unescape_char($f)  -> $\f;     %% \f = FF                         [48]
unescape_char($r)  -> $\r;     %% \r = CR                         [49]
unescape_char($e)  -> $\e;     %% \e = ESC                        [50]
unescape_char($N)  -> 16#85;   %% \N = Unicode next line          [55]
unescape_char($_)  -> 16#A0;   %% \_ = Unicode non-breaking space [56]
unescape_char($L)  -> 16#2028; %% \L = Unicode line sep.          [57]
unescape_char($P)  -> 16#2029; %% \P = Unicode paragraph sep.     [58]
unescape_char($\s) -> $\s;     %% \  = SPC                        [51]
unescape_char($")  -> $";      %% \" = "                          [52]
unescape_char($/)  -> $/;      %% \/ = /                          [53]
unescape_char($\\) -> $\\;     %% \\ = \                          [54]
unescape_char(_)   -> undefined.

hex_to_dec([C | Rest], Number) ->
    C_Dec = if
        C >= $0, C =< $9 -> C - $0;
        C >= $a, C =< $f -> C - $a + 10;
        C >= $A, C =< $F -> C - $A + 10
    end,
    hex_to_dec(Rest, Number * 16 + C_Dec);
hex_to_dec([], Number) ->
    Number.

%% -------------------------------------------------------------------
%% Comments.
%% -------------------------------------------------------------------

parse_comment(#yaml_parser{doc_version = Version, chars = [C | _]} = Parser)
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    %% A comment ends at the end of the line.
    %% This clause also takes care of DOS newline (even if the buffer
    %% contains only \r and not \n yet). It doesn't matter because we
    %% let the next state handle the newline properly; the cursor is not
    %% moved forward.
    next_state(Parser, fun find_next_token/1);
parse_comment(#yaml_parser{chars = [_ | Rest]} = Parser) ->
    Parser1 = warn_if_non_ascii_line_break(Parser),
    Parser2 = next_col(Parser1, 1, Rest),
    parse_comment(Parser2);
parse_comment(#yaml_parser{chars = [], raw_eos = false} = Parser) ->
    return(Parser);
parse_comment(#yaml_parser{chars = [], raw_eos = true} = Parser) ->
    next_state(Parser, fun find_next_token/1).

%% -------------------------------------------------------------------
%% Implicit key handling.
%% -------------------------------------------------------------------

push_fake_impl_key(#yaml_parser{ik_stack = Stack} = Parser) ->
    Parser#yaml_parser{ik_stack = [?FAKE_IMPL_KEY | Stack]}.

pop_impl_key(#yaml_parser{ik_stack = [_ | Rest]} = Parser) ->
    Parser#yaml_parser{ik_stack = Rest}.

allow_impl_key(Parser, Flag) ->
    Parser#yaml_parser{ik_allowed = Flag}.

save_impl_key_pos(
  #yaml_parser{chars_idx = Chars_Index, line = Line, col = Col,
  tks_first_idx = First, tks_queued = Queued,
  cur_coll = Cur_Coll, ik_stack = [_ | Rest]} = Parser) ->
    Required = ?IN_BLOCK_CTX(Parser) andalso Cur_Coll#bcoll.indent == Col,
    if
        Parser#yaml_parser.ik_allowed ->
            Impl_Key    = #impl_key{
              possible  = true,
              required  = Required,
              line      = Line,
              col       = Col,
              chars_idx = Chars_Index,
              token_idx = First + Queued
            },
            Parser#yaml_parser{ik_stack = [Impl_Key | Rest]};
        Required ->
            Error = #yaml_parser_error{
              name   = required_implicit_key_not_allowed,
              line   = Line,
              column = Col
            },
            Parser1 = add_error(Parser, Error,
              "Required implicit key not allowed here", []),
            return(Parser1);
        true ->
            Parser
    end.

queue_impl_key(#yaml_parser{last_token_endline = Line,
    ik_stack = [#impl_key{line = Impl_Line} = Impl_Key | _]} = Parser)
  when Line > Impl_Line andalso ?IN_BLOCK_CTX(Parser) ->
    %% An implicit key must not span several lines.
    Error = #yaml_parser_error{
      name   = invalid_implicit_key,
      type   = warning,
      line   = Impl_Key#impl_key.line,
      column = Impl_Key#impl_key.col
    },
    Parser1 = add_error(Parser, Error,
      "An implicit key must not span several lines", []),
    queue_impl_key2(Parser1);
queue_impl_key(#yaml_parser{last_token_endline = Line,
    ik_stack = [#impl_key{line = Impl_Line} = Impl_Key | _]} = Parser)
  when Line > Impl_Line andalso ?IN_FLOW_CTX(Parser) ->
    %% An implicit key must not span several lines.
    Error = #yaml_parser_error{
      name   = invalid_implicit_key,
      type   = warning,
      line   = Impl_Key#impl_key.line,
      column = Impl_Key#impl_key.col
    },
    Parser1 = add_error(Parser, Error,
      "An implicit key must not span several lines", []),
    queue_impl_key2(Parser1);
queue_impl_key(#yaml_parser{chars_idx = Index,
    ik_stack = [#impl_key{chars_idx = Impl_Index} = Impl_Key | _]} = Parser)
  when Index > Impl_Index + 1024 ->
    %% An implicit key must not take more than 1024 characters.
    Error = #yaml_parser_error{
      name   = invalid_implicit_key,
      type   = warning,
      line   = Impl_Key#impl_key.line,
      column = Impl_Key#impl_key.col
    },
    Parser1 = add_error(Parser, Error,
      "An implicit key must not take more than 1024 characters", []),
    queue_impl_key2(Parser1);
queue_impl_key(Parser) ->
    queue_impl_key2(Parser).

queue_impl_key2(
  #yaml_parser{ik_stack = [Impl_Key | Rest]} = Parser) ->
    Token = #yaml_mapping_key{
      line   = Impl_Key#impl_key.line,
      column = Impl_Key#impl_key.col
    },
    Parser1 = queue_token(Parser, Token, Impl_Key#impl_key.token_idx),
    Parser1#yaml_parser{
      ik_stack   = [?FAKE_IMPL_KEY | Rest],
      ik_allowed = false
    }.

remove_impl_key_pos(
  #yaml_parser{ik_stack = [
  #impl_key{required = true, line = Line, col = Col} | _]} = Parser) ->
    %% This error is raised with the following examples:
    %%
    %% - entry
    %% unexpected-scalar
    %%
    %% ? key
    %% unexpected-scalar
    Error = #yaml_parser_error{
      name   = expected_sequence_entry_or_mapping_key_not_found,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Expected sequence entry or mapping implicit key not found", []),
    return(Parser1);
remove_impl_key_pos(
  #yaml_parser{ik_stack = [_ | Rest]} = Parser) ->
    Parser#yaml_parser{ik_stack = [?FAKE_IMPL_KEY | Rest]}.

%% -------------------------------------------------------------------
%% Tokens queueing.
%% -------------------------------------------------------------------

queue_token(Parser, Token) ->
    queue_token_check_doc(Parser, Token, tail).

queue_token(Parser, Token, Insert_At) ->
    queue_token_check_doc(Parser, Token, Insert_At).

%%
%% Handle document start/end.
%%

queue_token_check_doc(
  #yaml_parser{doc_started = false} = Parser, Token, Insert_At)
  when is_record(Token, yaml_stream_start) orelse
  is_record(Token, yaml_stream_end) orelse
  is_record(Token, yaml_doc_end) orelse
  is_record(Token, yaml_yaml_directive) orelse
  is_record(Token, yaml_tag_directive) orelse
  is_record(Token, yaml_reserved_directive) ->
    %% Directives token outside a document are perfectly allowed.
    queue_token_pending_entry(Parser, Token, Insert_At);
queue_token_check_doc(
  #yaml_parser{doc_started = false, last_tag = Tag} = Parser,
  Token, Insert_At) ->
    %% Other tokens starts the document automatically.
    {Line, Col} = case Tag of
        #yaml_tag{line = L, column = C} ->
            %% A tag is pending: use its position instead of the
            %% position of the token about to be queued.
            {L, C};
        _ ->
            {?TOKEN_LINE(Token), ?TOKEN_COLUMN(Token)}
    end,
    %% We may need to shift the implicit key's index.
    Parser1 = update_impl_key_index(Parser, Line, Col),
    Parser2 = start_doc(Parser1, Line, Col, Insert_At),
    queue_token_pending_entry(Parser2, Token, next_insert_at(Insert_At, 1));
queue_token_check_doc(
  #yaml_parser{doc_started = true} = Parser, Token, Insert_At)
  when is_record(Token, yaml_stream_end) ->
    %% A document is automatically ended when we reach the end of the stream.
    Parser1 = end_doc(Parser, ?TOKEN_LINE(Token), ?TOKEN_COLUMN(Token),
      Insert_At),
    queue_token_check_doc(Parser1, Token, Insert_At);
queue_token_check_doc(
  #yaml_parser{doc_started = true} = Parser, Token, Insert_At) ->
    %% Document already started.
    queue_token_pending_entry(Parser, Token, Insert_At).

%%
%% Pending entries.
%%

queue_token_pending_entry(
  #yaml_parser{pending_entry = false} = Parser,
  Token, Insert_At) ->
    queue_token_check_collection_start(Parser, Token, Insert_At);
queue_token_pending_entry(
  #yaml_parser{pending_entry = true, last_tag = Tag} = Parser,
  Token, Insert_At) ->
    %% There's a pending entry: queue it now.
    Parser1 = Parser#yaml_parser{
      pending_entry = false
    },
    {Line, Col} = case Tag of
        #yaml_tag{line = L, column = C} ->
            %% A tag is pending: use its position instead of the
            %% position of the token about to be queued.
            {L, C};
        _ ->
            {?TOKEN_LINE(Token), ?TOKEN_COLUMN(Token)}
    end,
    Entry = #yaml_sequence_entry{
      line   = Line,
      column = Col
    },
    %% We may need to shift the implicit key's index.
    Parser2 = update_impl_key_index(Parser1,
      ?TOKEN_LINE(Token), ?TOKEN_COLUMN(Token)),
    Parser3 = queue_token(Parser2, Entry, Insert_At),
    queue_token_check_collection_start(Parser3, Token,
      next_insert_at(Insert_At, 1)).

%%
%% Handle collection start.
%%

queue_token_check_collection_start(
  #yaml_parser{
    cur_coll = #bcoll{kind = Kind, indent = Indent} = Cur_Coll,
    parent_colls = Colls} = Parser,
  #yaml_sequence_entry{line = Line, column = Col} = Token, Insert_At)
  when ?IN_BLOCK_CTX(Parser) andalso
  (Col > Indent orelse (Kind == mapping andalso Col == Indent)) ->
    %% This is the first entry of a block sequence collection. Queue a
    %% collection-start token.
    Collection_Start = #yaml_collection_start{
      style  = block,
      kind   = sequence,
      line   = Line,
      column = Col
    },
    Collection_Start1 = set_default_tag(Collection_Start),
    Parser1 = queue_token(Parser, Collection_Start1, Insert_At),
    %% Record the new block indent.
    New_Coll = #bcoll{kind = sequence, indent = Col},
    Parser2  = Parser1#yaml_parser{
      cur_coll     = New_Coll,
      parent_colls = [Cur_Coll | Colls]
    },
    queue_token_keep_last_pos(Parser2, Token,
      next_insert_at(Insert_At, 1));
queue_token_check_collection_start(
  #yaml_parser{
    cur_coll = #bcoll{indent = Indent} = Cur_Coll,
    parent_colls = Colls} = Parser,
  #yaml_mapping_key{line = Line, column = Col} = Token, Insert_At)
  when ?IN_BLOCK_CTX(Parser) andalso Col > Indent ->
    %% This is the first key: value pair of a block mapping collection. Queue
    %% a collection-start token.
    Collection_Start = #yaml_collection_start{
      style  = block,
      kind   = mapping,
      line   = Line,
      column = Col
    },
    Collection_Start1 = set_default_tag(Collection_Start),
    Parser1 = queue_token(Parser, Collection_Start1, Insert_At),
    %% Record the new block indent.
    New_Coll = #bcoll{kind = mapping, indent = Col},
    Parser2  = Parser1#yaml_parser{
      cur_coll     = New_Coll,
      parent_colls = [Cur_Coll | Colls]
    },
    queue_token_keep_last_pos(Parser2, Token,
      next_insert_at(Insert_At, 1));
queue_token_check_collection_start(
  #yaml_parser{
    cur_coll = #fcoll{kind = sequence} = Cur_Coll,
    parent_colls = Colls} = Parser,
  #yaml_mapping_key{line = Line, column = Col} = Token, Insert_At)
  when ?IN_FLOW_CTX(Parser) ->
    %% This is a single key: value pair inside a flow sequence. Queue
    %% a collection-start token.
    Collection_Start = #yaml_collection_start{
      style  = flow,
      kind   = mapping,
      line   = Line,
      column = Col
    },
    Collection_Start1 = set_default_tag(Collection_Start),
    Parser1 = queue_token(Parser, Collection_Start1, Insert_At),
    %% Flag this mapping as single pair inside flow sequence.
    New_Coll = #fcoll{kind = single_mapping},
    Parser2  = Parser1#yaml_parser{
      cur_coll     = New_Coll,
      parent_colls = [Cur_Coll | Colls]
    },
    queue_token_keep_last_pos(Parser2, Token,
      next_insert_at(Insert_At, 1));
queue_token_check_collection_start(Parser, Token, Insert_At) ->
    queue_token_keep_last_pos(Parser, Token, Insert_At).

%%
%% Remember last sequence entry, mapping key and mapping value
%% positions.
%%

queue_token_keep_last_pos(
  #yaml_parser{cur_coll = Coll,
    tks_first_idx = First, tks_queued = Queued} = Parser,
  #yaml_sequence_entry{line = Line, column = Col} = Token, Insert_At) ->
    Index = case Insert_At of
        tail -> First + Queued;
        _    -> Insert_At + 1
    end,
    Coll1 = if
        ?IN_BLOCK_CTX(Parser) ->
            Coll#bcoll{kidx = Index, kline = Line, kcol = Col};
        ?IN_FLOW_CTX(Parser) ->
            Coll#fcoll{kidx = Index, kline = Line, kcol = Col}
    end,
    Parser1 = Parser#yaml_parser{
      cur_coll = Coll1
    },
    queue_token_json_like(Parser1, Token, Insert_At);
queue_token_keep_last_pos(
  #yaml_parser{cur_coll = Coll,
    tks_first_idx = First, tks_queued = Queued} = Parser,
  #yaml_mapping_key{line = Line, column = Col} = Token, Insert_At) ->
    %% While we're handling a mapping key, tell that we're not waiting
    %% for a key: value pair anymore.
    Index = case Insert_At of
        tail -> First + Queued;
        _    -> Insert_At + 1
    end,
    Coll1 = if
        ?IN_BLOCK_CTX(Parser) ->
            Coll#bcoll{kidx = Index, kline = Line, kcol = Col};
        ?IN_FLOW_CTX(Parser) ->
            Coll#fcoll{kidx = Index, kline = Line, kcol = Col}
    end,
    Parser1 = Parser#yaml_parser{
      cur_coll           = Coll1,
      waiting_for_kvpair = false
    },
    queue_token_json_like(Parser1, Token, Insert_At);
queue_token_keep_last_pos(
  #yaml_parser{cur_coll = Coll,
    tks_first_idx = First, tks_queued = Queued} = Parser,
  #yaml_mapping_value{line = Line, column = Col} = Token, Insert_At) ->
    Index = case Insert_At of
        tail -> First + Queued;
        _    -> Insert_At + 1
    end,
    Coll1 = if
        ?IN_BLOCK_CTX(Parser) ->
            Coll#bcoll{vidx = Index, vline = Line, vcol = Col};
        ?IN_FLOW_CTX(Parser) ->
            Coll#fcoll{vidx = Index, vline = Line, vcol = Col}
    end,
    Parser1 =Parser#yaml_parser{
      cur_coll = Coll1
    },
    queue_token_json_like(Parser1, Token, Insert_At);
queue_token_keep_last_pos(Parser, Token, Insert_At) ->
    queue_token_json_like(Parser, Token, Insert_At).

%%
%% JSON-like tokens.
%%

queue_token_json_like(Parser, Token, tail)
  when ?IS_JSON_LIKE(Token) ->
    Parser1 = Parser#yaml_parser{
      last_is_json_like = true
    },
    do_queue_token(Parser1, Token, tail);
queue_token_json_like(Parser, Token, tail) ->
    Parser1 = Parser#yaml_parser{
      last_is_json_like = false
    },
    do_queue_token(Parser1, Token, tail);
queue_token_json_like(Parser, Token, Insert_At) ->
    do_queue_token(Parser, Token, Insert_At).

%%
%% Insert the token at the end of the queue or a given index.
%%

do_queue_token(#yaml_parser{tokens = Tokens, tks_queued = Queued} = Parser,
  Token, tail) ->
    Tokens1 = [Token | Tokens],
    Parser1 = Parser#yaml_parser{
      tokens     = Tokens1,
      tks_queued = Queued + 1
    },
    emit_tokens(Parser1);
do_queue_token(#yaml_parser{tokens = Tokens, tks_queued = Queued,
    tks_first_idx = First} = Parser,
  Token, Insert_At) ->
    Split        = Queued - (Insert_At - First),
    {Head, Tail} = lists:split(Split, Tokens),
    Parser1 = Parser#yaml_parser{
      tokens     = Head ++ [Token] ++ Tail,
      tks_queued = Queued + 1
    },
    emit_tokens(Parser1).

%%
%% Emit tokens which are ready using the callback function.
%% At this point, all tokens coming in are in the final order. We may
%% only do some checks, add empty nodes and set nodes' tag property.
%%

emit_tokens(
  #yaml_parser{tokens = Tokens, tks_first_idx = First} = Parser) ->
    Max = max_token_idx_ready(Parser),
    emit_tokens2(Parser, lists:reverse(Tokens), First, Max).

emit_tokens2(
  #yaml_parser{last_tag = undefined,
    tks_queued = Queued, tks_first_idx = First} = Parser,
  [#yaml_tag{} = Tag | Rest], Idx, Max)
  when Idx =< Max ->
    %% Keep the tag outside of the token queue. It'll be attached to a
    %% following node.
    Parser1 = Parser#yaml_parser{
      tks_queued    = Queued - 1,
      tks_first_idx = First + 1,
      last_tag      = Tag
    },
    emit_tokens2(Parser1, Rest, Idx + 1, Max);
emit_tokens2(#yaml_parser{tks_queued = Queued, tks_first_idx = First} = Parser,
  [#yaml_tag{line = Line, column = Col} = Tag | Rest], Idx, Max)
  when Idx =< Max ->
    %% Error: several tags for the same node.
    Error = #yaml_parser_error{
      name   = multiple_tag_properties,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Multiple tag properties attached to one node: "
      "the last one will be used", []),
    Parser2 = Parser1#yaml_parser{
      tks_queued    = Queued - 1,
      tks_first_idx = First + 1,
      last_tag      = Tag
    },
    emit_tokens2(Parser2, Rest, Idx + 1, Max);

emit_tokens2(#yaml_parser{last_anchor = undefined,
    tks_queued = Queued, tks_first_idx = First} = Parser,
  [#yaml_anchor{} = Anchor | Rest], Idx, Max)
  when Idx =< Max ->
    %% Keep the anchor outside of the token queue. It'll be emitted with
    %% its attached node. We also use this to check if multiple anchors
    %% are attached to the same node.
    Parser1 = Parser#yaml_parser{
      tks_queued    = Queued - 1,
      tks_first_idx = First + 1,
      last_anchor   = Anchor
    },
    emit_tokens2(Parser1, Rest, Idx + 1, Max);
emit_tokens2(#yaml_parser{tks_queued = Queued, tks_first_idx = First} = Parser,
  [#yaml_anchor{line = Line, column = Col} = Anchor | Rest], Idx, Max)
  when Idx =< Max ->
    %% Error: several tags for the same node.
    Error = #yaml_parser_error{
      name   = multiple_anchor_properties,
      line   = Line,
      column = Col
    },
    Parser1 = add_error(Parser, Error,
      "Multiple anchor properties attached to one node: "
      "the last one will be used", []),
    Parser2 = Parser1#yaml_parser{
      tks_queued    = Queued - 1,
      tks_first_idx = First + 1,
      last_anchor   = Anchor
    },
    emit_tokens2(Parser2, Rest, Idx + 1, Max);

emit_tokens2(#yaml_parser{last_token = Last} = Parser,
  [Token | Rest], Idx , Max)
  when Idx =< Max ->
    %% Run some checks:
    %%   o  Can "Last" and "Token" be in a raw?
    %%   o  Do we need to insert an empty scalar?
    Parser1 = check_tokens_in_a_row(Parser, Last, Token),
    %% Handle properties and execute the specified callback function (or
    %% queue the token).
    Parser2 = handle_anchor_property(Parser1, Token),
    emit_tokens2(Parser2, Rest, Idx + 1, Max);

emit_tokens2(Parser, Tokens, _, _) ->
    Tokens1 = lists:reverse(Tokens),
    Parser#yaml_parser{
      tokens = Tokens1
    }.

%% Check if a token can follow another and insert empty node if
%% necessary.
check_tokens_in_a_row(
  #yaml_parser{last_tag = Tag, last_anchor = Anchor,
    tks_queued = Queued} = Parser,
  Token1, Token2) when
  %% Empty sequence entry.
  (is_record(Token1, yaml_sequence_entry) andalso
   (?IN_BLOCK_CTX(Parser) orelse
    Tag /= undefined orelse
    Anchor /= undefined) andalso
   (is_record(Token2, yaml_sequence_entry) orelse
    is_record(Token2, yaml_collection_end))) orelse
  %% Empty mapping key.
  (is_record(Token1, yaml_mapping_key) andalso
   (is_record(Token2, yaml_mapping_value) orelse
    is_record(Token2, yaml_collection_end))) orelse
  %% Empty mapping value.
  (is_record(Token1, yaml_mapping_value) andalso
   (is_record(Token2, yaml_mapping_key) orelse
    is_record(Token2, yaml_collection_end))) orelse
  %% Empty mapping value.
  (is_record(Token1, yaml_mapping_value) andalso
   (is_record(Token2, yaml_mapping_key) orelse
    is_record(Token2, yaml_collection_end))) orelse
  %% Empty document.
  (is_record(Token1, yaml_doc_start) andalso
   is_record(Token2, yaml_doc_end)) orelse
  %% Anchor alone.
  (is_record(Token1, yaml_anchor) andalso
   (is_record(Token2, yaml_mapping_value) orelse
    is_record(Token2, yaml_collection_end) orelse
    is_record(Token2, yaml_doc_end))) ->
    %% Token1 is followed by an empty scalar.
    {Line, Col} = case Tag of
        #yaml_tag{line = L, column = C} ->
            %% A tag is pending: use its position instead of the
            %% position of the token about to be queued.
            {L, C};
        undefined ->
            case Token1 of
                #yaml_doc_start{} ->
                    {?TOKEN_LINE(Token2), ?TOKEN_COLUMN(Token2)};
                _ ->
                    {?TOKEN_LINE(Token1), ?TOKEN_COLUMN(Token1)}
            end
    end,
    Empty = empty_scalar(Line, Col),
    Parser1 = Parser#yaml_parser{
      tks_queued = Queued + 1
    },
    handle_anchor_property(Parser1, Empty);
check_tokens_in_a_row(Parser, Token1, Token2) when
  (is_record(Token1, yaml_scalar) orelse
   is_record(Token1, yaml_collection_end)) andalso
  (is_record(Token2, yaml_scalar) orelse
   is_record(Token2, yaml_collection_start)) ->
    %% Token2 can't follow Token1.
    Error = #yaml_parser_error{
      name   = unpected_token,
      token  = Token2,
      line   = ?TOKEN_LINE(Token2),
      column = ?TOKEN_COLUMN(Token2)
    },
    Parser1 = add_error(Parser, Error,
      "Unexpected \"~s\" token following a \"~s\" token",
      [?TOKEN_NAME(Token2), ?TOKEN_NAME(Token1)]),
    return(Parser1);
check_tokens_in_a_row(Parser, _, _) ->
    Parser.

handle_anchor_property(
  #yaml_parser{last_anchor = undefined} = Parser, Token) ->
    handle_tag_property(Parser, Token);
handle_anchor_property(
  #yaml_parser{last_anchor = Anchor} = Parser, Token) when
  (is_record(Token, yaml_collection_start) andalso
   Anchor#yaml_anchor.line < Token#yaml_collection_start.line) orelse
  (is_record(Token, yaml_scalar) andalso
   (Anchor#yaml_anchor.line < Token#yaml_scalar.line orelse
    (Anchor#yaml_anchor.line == Token#yaml_scalar.line andalso
     Anchor#yaml_anchor.column =< Token#yaml_scalar.column))) ->
    Parser1 = do_emit_token(Parser, Anchor),
    Parser2 = Parser1#yaml_parser{
      last_anchor = undefined
    },
    handle_tag_property(Parser2, Token);
handle_anchor_property(Parser, Token) ->
    handle_tag_property(Parser, Token).

handle_tag_property(
  #yaml_parser{last_tag = undefined} = Parser, Token) ->
    do_emit_token(Parser, Token);
handle_tag_property(
  #yaml_parser{last_tag = Tag} = Parser, Token) when
  (is_record(Token, yaml_collection_start) andalso
   Tag#yaml_tag.line < Token#yaml_collection_start.line) orelse
  (is_record(Token, yaml_scalar) andalso
   (Tag#yaml_tag.line < Token#yaml_scalar.line orelse
    (Tag#yaml_tag.line == Token#yaml_scalar.line andalso
     Tag#yaml_tag.column =< Token#yaml_scalar.column))) ->
    %% The tag property is attached to this token.
    Token1 = case Token of
        #yaml_scalar{} ->
            Token#yaml_scalar{
              tag = Tag
            };
        #yaml_collection_start{} ->
            Token#yaml_collection_start{
              tag = Tag
            }
    end,
    %% Clear the pending tag property.
    Parser1 = Parser#yaml_parser{
      last_tag = undefined
    },
    do_emit_token(Parser1, Token1);
handle_tag_property(Parser, Token) ->
    do_emit_token(Parser, Token).

do_emit_token(
  #yaml_parser{token_fun = undefined,
    tks_queued = Queued, tks_first_idx = First,
    tks_emitted = Emitted, tks_ready = Ready} = Parser,
  Token) ->
    %% The anchor was already counted when first removed from the queue.
    {Queued1, First1} = case ?TOKEN_NAME(Token) of
        yaml_anchor -> {Queued,     First};
        _           -> {Queued - 1, First + 1}
    end,
    Parser#yaml_parser{
      tks_queued    = Queued1,
      tks_first_idx = First1,
      tks_emitted   = Emitted + 1,
      last_token    = Token,
      tks_ready     = [Token | Ready]
    };
do_emit_token(
  #yaml_parser{token_fun = Fun,
    tks_queued = Queued, tks_first_idx = First, tks_emitted = Emitted} = Parser,
  Token) ->
    %% The anchor was already counted when first removed from the queue.
    {Queued1, First1} = case ?TOKEN_NAME(Token) of
        yaml_anchor -> {Queued,     First};
        _           -> {Queued - 1, First + 1}
    end,
    try
        Fun1 = case Fun(Token) of
            ok       -> Fun;
            {ok, F1} -> F1
        end,
        Parser#yaml_parser{
          token_fun     = Fun1,
          tks_queued    = Queued1,
          tks_first_idx = First1,
          tks_emitted   = Emitted + 1,
          last_token    = Token
        }
    catch
        throw:Error when is_record(Error, yaml_parser_error) ->
            Parser1 = add_error(Parser, Error),
            Parser2 = Parser1#yaml_parser{
              tks_queued    = Queued1,
              tks_first_idx = First1,
              tks_emitted   = Emitted + 1,
              last_token    = Token
            },
            if
                Error#yaml_parser_error.type == error -> return(Parser2);
                true                                  -> Parser2
            end;
        throw:{Fun2, Error} when is_record(Error, yaml_parser_error) ->
            Parser1 = add_error(Parser, Error),
            Parser2 = Parser1#yaml_parser{
              token_fun     = Fun2,
              tks_queued    = Queued1,
              tks_first_idx = First1,
              tks_emitted   = Emitted + 1,
              last_token    = Token
            },
            if
                Error#yaml_parser_error.type == error -> return(Parser2);
                true                                  -> Parser2
            end
    end.

next_insert_at(tail, _)      -> tail;
next_insert_at(Insert_At, N) -> Insert_At + N.

max_token_idx_ready(
  #yaml_parser{ik_stack = Stack, tks_first_idx = First, tks_queued = Queued}) ->
    max_token_idx_ready2(First + Queued - 1, lists:reverse(Stack)).

max_token_idx_ready2(_, [#impl_key{possible = true, token_idx = Idx} | _]) ->
    Idx - 1;
max_token_idx_ready2(All, [_ | Rest]) ->
    max_token_idx_ready2(All, Rest);
max_token_idx_ready2(All, []) ->
    All.

%% A token was inserted before the potential implicit key: move the
%% key's index.
update_impl_key_index(#yaml_parser{ik_stack = Stack} = Parser, Line, Col) ->
    update_impl_key_index2(Parser, Stack, Line, Col, []).

update_impl_key_index2(Parser,
  [#impl_key{token_idx = Index, line = Key_L, col = Key_C} = Impl_Key | Rest],
  Line, Col, Result)
  when is_integer(Key_L) andalso is_integer(Key_C) andalso
  (Line < Key_L orelse (Line == Key_L andalso Col =< Key_C)) ->
    Impl_Key1 = Impl_Key#impl_key{
      token_idx = Index + 1
    },
    Result1 = [Impl_Key1 | Result],
    update_impl_key_index2(Parser, Rest, Line, Col, Result1);
update_impl_key_index2(Parser,
  [#impl_key{line = Key_L, col = Key_C} | _] = Rest,
  Line, Col, Result)
  when is_integer(Key_L) andalso is_integer(Key_C) andalso
  (Line > Key_L orelse (Line == Key_L andalso Col > Key_C)) ->
    Parser#yaml_parser{
      ik_stack = lists:reverse(Result) ++ Rest
    };
update_impl_key_index2(Parser,
  [Impl_Key | Rest], Line, Col, Result) ->
    Result1 = [Impl_Key | Result],
    update_impl_key_index2(Parser, Rest, Line, Col, Result1);
update_impl_key_index2(Parser, [], _, _, Result) ->
    Parser#yaml_parser{
      ik_stack = lists:reverse(Result)
    }.

%% -------------------------------------------------------------------
%% Tag resolution.
%% -------------------------------------------------------------------

setup_default_tags(#yaml_parser{options = Options} = Parser) ->
    Tags  = dict:new(),
    %% By default, "!" is resolved as "!" and the tag is considered
    %% local.
    Tags1 = dict:store({default, "!"},  "!", Tags),
    %% By default, "!!" is resolved as "tag:yaml.org,2002:" and is used
    %% by the YAML tags repository.
    Tags2 = dict:store({default, "!!"}, "tag:yaml.org,2002:", Tags1),
    %% Non-specific tags are associated to nodes which don't have an
    %% explicit tag or to those with the "!" explicit non-specific tag.
    %% The non-specific tags are resolved using a schema.
    Tags3 = dict:store({default, {non_specific, "!"}}, "tag:yaml.org,2002:",
      Tags2),
    Tags4 = dict:store({default, {non_specific, "?"}}, "tag:yaml.org,2002:",
      Tags3),
    Tags5 = case proplists:get_value(default_tags, Options) of
        undefined ->
            Tags4;
        List ->
            Fun = fun({Prefix, Value}, T) ->
                dict:store({default, Prefix}, Value, T)
            end,
            lists:foldl(Fun, Tags4, List)
    end,
    Parser#yaml_parser{
      tags = Tags5
    }.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

option_names() ->
    [
      default_tags,
      doc_version,
      io_blocksize,
      token_fun
    ].

check_options([Option | Rest]) ->
    case is_option_valid(Option) of
        true  -> check_options(Rest);
        false -> invalid_option(Option)
    end;
check_options([]) ->
    ok.

is_option_valid({default_tags, List}) when is_list(List) ->
    %% This fun() returns true for any invalid entries, to keep only
    %% those.
    Fun = fun
        ({{non_specific, A}, B}) ->
            not (io_lib:char_list(A) andalso io_lib:char_list(B));
        ({A, B}) ->
            not (io_lib:char_list(A) andalso io_lib:char_list(B));
        (_) ->
            true
    end,
    case lists:filter(Fun, List) of
        [] -> true;
        _  -> false
    end;
is_option_valid({doc_version, {Major, Minor}}) when
  is_integer(Major) andalso Major >= 0 andalso
  is_integer(Minor) andalso Minor >= 0 ->
    true;
is_option_valid({io_blocksize, BS})
  when is_integer(BS) andalso BS >= 1 ->
    true;
is_option_valid({token_fun, Fun})
  when is_function(Fun, 1) ->
    true;
is_option_valid(_) ->
    false.

invalid_option(Option) ->
    Error = #yaml_parser_error{
      name  = invalid_parser_option,
      extra = [{option, Option}]
    },
    Error1 = case Option of
        {default_tags, _} ->
            Error#yaml_parser_error{
              text = "Invalid value for option \"default_tags\": "
              "it must be a list of {Prefix, Prefix_Value}"
            };
        {doc_version, _} ->
            Error#yaml_parser_error{
              text = "Invalid value for option \"doc_version\": "
              "it must be a tuple of the form {Major, Minor} "
              "where Major and Minor are positive integers"
            };
        {io_blocksize, _} ->
            Error#yaml_parser_error{
              text = "Invalid value for option \"io_blocksize\": "
              "it must be a positive interger, expressed in bytes"
            };
        {token_fun, _} ->
            Error#yaml_parser_error{
              text = "Invalid value for option \"token_fun\": "
              "it must be a function taking the parser state "
              "as its sole argument"
            };
        _ ->
            Error#yaml_parser_error{
              text = lists:flatten(io_lib:format("Unknown option \"~w\"",
                  [Option]))
            }
    end,
    throw_error(Error1).

next_state(Parser, State) ->
    State(Parser#yaml_parser{stream_state = State}).

next_col(
  #yaml_parser{chars_len = Len, chars_idx = Index, col = Col} = Parser,
  Count, Rest) ->
    Parser#yaml_parser{
      chars     = Rest,
      chars_len = Len - Count,
      chars_idx = Index + Count,
      col       = Col + Count
    }.

next_line(Parser, [$\r, $\n | Rest]) ->
    next_line(Parser, 2, Rest);
next_line(#yaml_parser{doc_version = Version} = Parser, [C | Rest])
  when ?IS_NEWLINE(C) orelse (Version == {1,1} andalso ?IS_NEWLINE_11(C)) ->
    next_line(Parser, 1, Rest).

next_line(
  #yaml_parser{chars_len = Len, chars_idx = Index, line = Line} = Parser,
  Count, Rest) ->
    Parser#yaml_parser{
      chars     = Rest,
      chars_len = Len - Count,
      chars_idx = Index + Count,
      col       = 1,
      line      = Line + 1
    }.

empty_scalar(Line, Col) ->
    Empty = #yaml_scalar{
      style    = flow,
      substyle = plain,
      text     = "",
      line     = Line,
      column   = Col
    },
    set_default_tag(Empty).

set_default_tag(
  #yaml_scalar{style = block, line = Line, column = Col} = Token) ->
    Token#yaml_scalar{
      tag = ?DEFAULT_TAG({non_specific, "!"}, Line, Col)
    };
set_default_tag(
  #yaml_scalar{substyle = plain, line = Line, column = Col} = Token) ->
    Token#yaml_scalar{
      tag = ?DEFAULT_TAG({non_specific, "?"}, Line, Col)
    };
set_default_tag(
  #yaml_scalar{line = Line, column = Col} = Token) ->
    Token#yaml_scalar{
      tag = ?DEFAULT_TAG({non_specific, "!"}, Line, Col)
    };
set_default_tag(
  #yaml_collection_start{line = Line, column = Col} = Token) ->
    Token#yaml_collection_start{
      tag = ?DEFAULT_TAG({non_specific, "?"}, Line, Col)
    }.

check_for_closed_block_collections(#yaml_parser{col = Col} = Parser) ->
    check_for_closed_block_collections(Parser, Col).

check_for_closed_block_collections(#yaml_parser{
  cur_coll = #bcoll{kind = sequence, indent = Col},
  parent_colls = [#bcoll{kind = mapping,  indent = Col} = Parent_Coll | Colls],
  chars = [C | _]} = Parser, Col) when C /= $- ->
    %% The sequence has the same indentation level than its parent
    %% mapping. The next token has this same indentation but is not a
    %% sequence entry (denoted by the '-' character). Let's close it but
    %% not the parent mapping.
    Token    = #yaml_collection_end{
      style  = block,
      kind   = sequence,
      line   = Parser#yaml_parser.last_token_endline,
      column = Parser#yaml_parser.last_token_endcol
    },
    Parser1 = queue_token(Parser, Token),
    %% Remove its indentation from the stack.
    Parser2 = Parser1#yaml_parser{
      cur_coll     = Parent_Coll,
      parent_colls = Colls
    },
    check_for_closed_block_collections(Parser2, Col);
check_for_closed_block_collections(#yaml_parser{
  cur_coll = #bcoll{kind = Kind, indent = Indent},
  parent_colls = [Parent_Coll | Colls]} = Parser, Col)
  when Col < Indent ->
    Parser1 = finish_incomplete_block_entries(Parser),
    %% Emit a token to signal the end of the block collection.
    Token    = #yaml_collection_end{
      style  = block,
      kind   = Kind,
      line   = Parser1#yaml_parser.last_token_endline,
      column = Parser1#yaml_parser.last_token_endcol
    },
    Parser2 = queue_token(Parser1, Token),
    %% Remove its indentation from the stack.
    Parser3 = Parser2#yaml_parser{
      cur_coll     = Parent_Coll,
      parent_colls = Colls
    },
    check_for_closed_block_collections(Parser3, Col);
check_for_closed_block_collections(Parser, _) ->
    Parser.

is_uri_valid(Parser, #yaml_tag{uri = {non_specific, _}}) ->
    Parser;
is_uri_valid(Parser, #yaml_tag{uri = [$! | _]}) ->
    Parser;
is_uri_valid(Parser, #yaml_tag{uri = URI} = Tag) ->
    is_uri_scheme_valid1(Parser, Tag, URI);
is_uri_valid(Parser, #yaml_tag_directive{prefix = [$! | _]}) ->
    Parser;
is_uri_valid(Parser, #yaml_tag_directive{prefix = URI} = Directive) ->
    is_uri_scheme_valid1(Parser, Directive, URI).

is_uri_scheme_valid1(Parser, Token, [C | Rest]) when
  (C >= $a andalso C =< $z) orelse
  (C >= $A andalso C =< $Z) ->
    is_uri_scheme_valid2(Parser, Token, Rest);
is_uri_scheme_valid1(Parser, Token, [_ | _]) ->
    Error = #yaml_parser_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Invalid character in URI scheme", []);
is_uri_scheme_valid1(Parser, Token, []) ->
    Error = #yaml_parser_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Unexpected end of URI", []).

is_uri_scheme_valid2(Parser, Token, [C | Rest]) when
  (C >= $a andalso C =< $z) orelse
  (C >= $A andalso C =< $Z) orelse
  (C >= $0 andalso C =< $9) orelse
  C == $+ orelse C == $. orelse C == $- ->
    is_uri_scheme_valid2(Parser, Token, Rest);
is_uri_scheme_valid2(Parser, Token, [$: | Rest]) ->
    is_uri_hier_part_valid(Parser, Token, Rest);
is_uri_scheme_valid2(Parser, Token, [_ | _]) ->
    Error = #yaml_parser_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Invalid character in URI scheme", []);
is_uri_scheme_valid2(Parser, Token, []) ->
    Error = #yaml_parser_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Unexpected end of URI", []).

is_uri_hier_part_valid(Parser, Token, [C | Rest]) when ?IS_URI_CHAR(C) ->
    is_uri_hier_part_valid(Parser, Token, Rest);
is_uri_hier_part_valid(Parser, _, []) ->
    Parser;
is_uri_hier_part_valid(Parser, Token, [_ | _]) ->
    Error = #yaml_parser_error{
      name   = invalid_uri,
      type   = warning,
      token  = Token,
      line   = ?TOKEN_LINE(Token),
      column = ?TOKEN_COLUMN(Token)
    },
    add_error(Parser, Error, "Invalid character in URI scheme", []).

warn_if_non_ascii_line_break(#yaml_parser{chars = [C | _]} = Parser)
  when ?IS_NEWLINE_11(C) ->
    %% Non-ASCII line break in a YAML 1.2 document.
    Error = #yaml_parser_error{
      type   = warning,
      name   = non_ascii_line_break,
      line   = ?CURSOR_LINE(Parser),
      column = ?CURSOR_COLUMN(Parser)
    },
    add_error(Parser, Error,
      "Use of non-ASCII line break is not supported anymore starting "
      "with YAML 1.2; treated as non-break character", []);
warn_if_non_ascii_line_break(Parser) ->
    Parser.

add_error(Parser, Error, Format, Args) ->
    %% Format error message.
    Text = lists:flatten(io_lib:format(Format, Args)),
    Error1 = Error#yaml_parser_error{
      text = Text
    },
    add_error(Parser, Error1).

add_error(
  #yaml_parser{has_errors = Has_Errors, errors = Errors} = Parser,
  #yaml_parser_error{type = Type} = Error) ->
    %% Update has_errors flag.
    Has_Errors1 = if
        Has_Errors -> Has_Errors;
        true       -> Type == error
    end,
    Parser#yaml_parser{
      has_errors = Has_Errors1,
      errors     = [Error | Errors]
    }.

return(#yaml_parser{has_errors = true} = Parser) ->
    throw_error(Parser);
return(#yaml_parser{raw_eos = true, chars_len = 0} = Parser) ->
    Parser;
return(Parser) ->
    {continue, Parser}.

-spec throw_error(#yaml_parser{} | #yaml_parser_error{}) -> no_return().
throw_error(Parser_Or_Error) ->
    throw({yaml_parser, Parser_Or_Error}).
