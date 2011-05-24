-ifndef(yaml_tokens_hrl).
-define(yaml_tokens_hrl, true).

-include("yaml_types.hrl").

%% CAUTION:
%% Records defined in this file have default values for all members.
%% Those default values are often bad values but this is needed so that
%% Erlang won't add "undefined" in our back to the allowed values in the
%% type specifications.

%% -------------------------------------------------------------------
%% Supported versions range.
%% -------------------------------------------------------------------

-define(MIN_YAML_MAJOR_VERSION_SUPPORTED, 1).
-define(MIN_YAML_MINOR_VERSION_SUPPORTED, 1).
-define(MAX_YAML_MAJOR_VERSION_SUPPORTED, 1).
-define(MAX_YAML_MINOR_VERSION_SUPPORTED, 2).
-define(IMPLICIT_YAML_VERSION, {
    ?MAX_YAML_MAJOR_VERSION_SUPPORTED,
    ?MAX_YAML_MINOR_VERSION_SUPPORTED
  }).

%% -------------------------------------------------------------------
%% Stream tokens.
%% -------------------------------------------------------------------

%% Stream start: emitted at the beginning of a stream. It's the very
%% first emitted token.
-record(yaml_stream_start, {
    line     = 1    :: position(),
    column   = 1    :: position(),
    encoding = utf8 :: encoding()
  }).
-type yaml_stream_start() :: #yaml_stream_start{}.

%% Stream end: emitted at the end of a stream. It's the last emitted
%% token.
-record(yaml_stream_end, {
    line   = 1      :: position(),
    column = 1      :: position()
  }).
-type yaml_stream_end() :: #yaml_stream_end{}.

%% -------------------------------------------------------------------
%% Document tokens.
%% -------------------------------------------------------------------

%% Document start: emitted at the beginning of a document: either when
%% an explicit "directives-end" marker is found or before the first
%% token that is not directive or a comment token.
-record(yaml_doc_start, {
    line    = 1                      :: position(),
    column  = 1                      :: position(),
    version = ?IMPLICIT_YAML_VERSION :: document_version(),
    tags    = dict:new()             :: tags_table()
  }).
-type yaml_doc_start() :: #yaml_doc_start{}.

%% Document end: emitted at the end of a document: either when an
%% explicit "document-end" or "directives-end" marker is found or before
%% a stream end.
-record(yaml_doc_end, {
    line   = 1                       :: position(),
    column = 1                       :: position()
  }).
-type yaml_doc_end() :: #yaml_doc_end{}.

%% YAML directive: emitted when a YAML directive is parsed.
-record(yaml_yaml_directive, {
    line    = 1                      :: position(),
    column  = 1                      :: position(),
    version = ?IMPLICIT_YAML_VERSION :: document_version() | undefined
  }).
-type yaml_yaml_directive() :: #yaml_yaml_directive{
  version :: document_version()
}.

%% TAG directive: emitted when a TAG directive is parsed.
-record(yaml_tag_directive, {
    line    = 1                      :: position(),
    column  = 1                      :: position(),
    handle  = "!"                    :: tag_handle() | [] | undefined,
    prefix  = "!"                    :: tag_prefix() | [] | undefined
  }).
-type yaml_tag_directive() :: #yaml_tag_directive{
  handle :: tag_handle(),
  prefix :: tag_prefix()
}.

%% Reserved directive: emitted when an unknown directive is found.
-record(yaml_reserved_directive, {
    line       = 1                   :: position(),
    column     = 1                   :: position(),
    name       = "RESERVED"          :: nonempty_string(),
    args       = []                  :: [nonempty_string()],
    args_count = 0                   :: non_neg_integer()
  }).
-type yaml_reserved_directive() :: #yaml_reserved_directive{}.

%% -------------------------------------------------------------------
%% Node properties.
%% -------------------------------------------------------------------

-record(yaml_anchor, {
    line   = 1        :: position(),
    column = 1        :: position(),
    name   = "anchor" :: nonempty_string()
  }).
-type yaml_anchor() :: #yaml_anchor{}.

-record(yaml_alias, {
    line   = 1        :: position(),
    column = 1        :: position(),
    name   = "alias"  :: nonempty_string()
  }).
-type yaml_alias() :: #yaml_alias{}.

-record(yaml_tag, {
    line   = 1        :: position(),
    column = 1        :: position(),
    uri    = "!"      :: tag_uri() | []
  }).
-type yaml_tag() :: #yaml_tag{uri :: tag_uri()}.

%% -------------------------------------------------------------------
%% Node tokens.
%% -------------------------------------------------------------------

%% Scalar: emitted when a scalar, no matter its style, is found.
-record(yaml_scalar, {
    line     = 1           :: position(),
    column   = 1           :: position(),
    tag      = #yaml_tag{} :: yaml_tag(),
    style    = flow        :: style(),
    substyle = plain       :: scalar_substyle(),
    text     = ""          :: string()
  }).
-type yaml_scalar() :: #yaml_scalar{
  tag :: yaml_tag()
}.

%% Collection start: emitted when a sequence or a mapping is opened.
-record(yaml_collection_start, {
    line   = 1             :: position(),
    column = 1             :: position(),
    tag    = #yaml_tag{}   :: yaml_tag(),
    style  = block         :: style(),
    kind   = sequence      :: collection_kind()
  }).
-type yaml_collection_start() :: #yaml_collection_start{}.

%% Collection end: emitted when a sequence or a mapping is closed.
-record(yaml_collection_end, {
    line   = 1             :: position(),
    column = 1             :: position(),
    style  = block         :: style(),
    kind   = sequence      :: collection_kind()
  }).
-type yaml_collection_end() :: #yaml_collection_end{}.

%% -------------------------------------------------------------------
%% Collection entries.
%% -------------------------------------------------------------------

%% Sequence entry: emitted before each sequence entry.
-record(yaml_sequence_entry, {
    line   = 1 :: position(),
    column = 1 :: position()
  }).
-type yaml_sequence_entry() :: #yaml_sequence_entry{}.

%% Mapping key: emitted before each mapping key.
-record(yaml_mapping_key, {
    line   = 1 :: position(),
    column = 1 :: position()
  }).
-type yaml_mapping_key() :: #yaml_mapping_key{}.

%% Mapping value: emitted before each mapping value.
-record(yaml_mapping_value, {
    line   = 1 :: position(),
    column = 1 :: position()
  }).
-type yaml_mapping_value() :: #yaml_mapping_value{}.

%% -------------------------------------------------------------------
%% Final data type specifications.
%% -------------------------------------------------------------------

-type yaml_token() ::
        yaml_stream_start()
      | yaml_stream_end()
      | yaml_doc_start()
      | yaml_doc_end()
      | yaml_yaml_directive()
      | yaml_tag_directive()
      | yaml_reserved_directive()
      | yaml_scalar()
      | yaml_collection_start()
      | yaml_collection_end()
      | yaml_sequence_entry()
      | yaml_mapping_key()
      | yaml_mapping_value()
      | yaml_tag()
      | yaml_anchor()
      | yaml_alias().

%% A partial token appears in a #yaml_parser_error{}.
-type yaml_partial_token() ::
        #yaml_stream_start{}
      | #yaml_stream_end{}
      | #yaml_doc_start{}
      | #yaml_doc_end{}
      | #yaml_yaml_directive{}
      | #yaml_tag_directive{}
      | #yaml_reserved_directive{}
      | #yaml_scalar{}
      | #yaml_collection_start{}
      | #yaml_collection_end{}
      | #yaml_sequence_entry{}
      | #yaml_mapping_key{}
      | #yaml_mapping_value{}
      | #yaml_tag{}
      | #yaml_anchor{}
      | #yaml_alias{}.

%% -------------------------------------------------------------------
%% Macros to access common members of the token records.
%% -------------------------------------------------------------------

-define(TOKEN_NAME(T),   element(1, T)).
-define(TOKEN_LINE(T),   element(#yaml_scalar.line, T)).
-define(TOKEN_COLUMN(T), element(#yaml_scalar.column, T)).

-endif.
