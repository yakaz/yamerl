-ifndef(yamerl_types_hrl).
-define(yamerl_types_hrl, true).

%% -------------------------------------------------------------------
%% Data types specifications.
%% -------------------------------------------------------------------

%% YAML version which a document conforms to.
%% The tuple has the form {Major, Minor}
-type document_version() :: {non_neg_integer(), non_neg_integer()}.

%% Stream encoding.
%% Only Unicode encodings are accepted.
-type encoding()         :: utf8
                          | {utf16, little | big}
                          | {utf32, little | big}.
-type unicode_string()   :: [unicode:unicode_char()].
%% unicode:external_unicode_binary() (UTF16/32) not exported.
-type unicode_binary()   :: unicode:unicode_binary().
-type unicode_data()     :: unicode_string()
                          | unicode_binary().

%% Tag declaration and usage.
%% A tag handle is used in TAG directives and in front of a node. A tag
%% prefix is used in a TAG directive. The final tag is used on nodes
%% after tag resolution. The tags table stores all tags declaration.
-type tag_handle()       :: nonempty_string().
-type tag_prefix()       :: nonempty_string().
-type tag_uri()          :: nonempty_string()
                          | {non_specific, [33 | 63]}. %% "!" | "?"
-type tags_table()       :: dict().

%% Node styles, substyles and kinds.
-type style()            :: block
                          | flow.
-type scalar_substyle()  :: literal
                          | folded
                          | double_quoted
                          | single_quoted
                          | plain.
-type collection_kind()  :: sequence
                          | mapping.

%% Position of a token (line and column).
-type position()         :: pos_integer().

-endif.
