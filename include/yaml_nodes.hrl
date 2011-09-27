-ifndef(yaml_nodes_hrl).
-define(yaml_nodes_hrl, true).

-include("yaml_types.hrl").

%% CAUTION:
%% Records defined in this file have default values for all members.
%% Those default values are often bad values but this is needed so that
%% Erlang won't add "undefined" in our back to the allowed values in the
%% type specifications.

%% -------------------------------------------------------------------
%% Nodes specifications.
%% -------------------------------------------------------------------

%% String (Failsafe Schema).
-record(yaml_str, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    text   = ""                :: string()
  }).
-type yaml_str()               :: #yaml_str{}.
-type yaml_simple_str()        :: string().

%% Null (Core Schema).
-record(yaml_null, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list()
  }).
-type yaml_null()              :: #yaml_null{}.
-type yaml_simple_null()       :: null.

%% Boolean (Core Schema).
-record(yaml_bool, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    value  = true              :: boolean()
  }).
-type yaml_bool()              :: #yaml_bool{}.
-type yaml_simple_bool()       :: boolean().

%% Integer (Core Schema).
-record(yaml_int, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    value  = 0                 :: integer()
  }).
-type yaml_int()               :: #yaml_int{}.
-type yaml_simple_int()        :: integer().

%% Erlang atom.
-record(yaml_erlang_atom, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    name                       :: atom()
  }).
-type yaml_erlang_atom()        :: #yaml_erlang_atom{}.
-type yaml_simple_erlang_atom() :: atom().

%% Erlang anonymous function.
-record(yaml_erlang_fun, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    function                   :: function(),
    text                       :: string()
  }).
-type yaml_erlang_fun()        :: #yaml_erlang_fun{}.
-type yaml_simple_erlang_fun() :: function().

%% Timestamp.
-record(yaml_timestamp, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    year                       :: calendar:year() | undefined,
    month                      :: calendar:month() | undefined,
    day                        :: calendar:day() | undefined,
    hour   = 0                 :: calendar:hour(),
    minute = 0                 :: calendar:minute(),
    second = 0                 :: calendar:second(),
    frac   = 0                 :: non_neg_integer(),
    tz     = 0                 :: integer()
  }).
-type yaml_timestamp()         :: #yaml_timestamp{}.
-type yaml_simple_timestamp()  :: calendar:t_datetime()
                                | {undefined, calendar:t_time()}.

%% Sequence (Failsafe Schema).
-record(yaml_seq, {
    module  = undefined        :: atom(),
    tag     = "!"              :: tag_uri(),
    pres    = []               :: list(),
    entries = []               :: [yaml_node()],
    count   = 0                :: non_neg_integer()
  }).
-type yaml_seq()               :: #yaml_seq{}.
-type yaml_simple_seq()        :: [yaml_simple_node()].
-type yaml_partial_seq()       :: {
  atom(),
  {seq, non_neg_integer()},
  [yaml_node() | yaml_simple_node() | '$insert_here']
}.

%% Mapping (Failsafe Schema).
-record(yaml_map, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    pairs  = []                :: [{yaml_node(), yaml_node()}]
  }).
-type yaml_map()               :: #yaml_map{}.
-type yaml_simple_map()        :: [{yaml_simple_node(), yaml_simple_node()}].
-type yaml_partial_map()       :: {
  atom(),
  {map, yaml_node() | yaml_simple_node() | undefined},
  [{
      yaml_node() | yaml_simple_node() | '$insert_here',
      yaml_node() | yaml_simple_node() | '$insert_here' | undefined
    }]
}.

%% Document.
-record(yaml_doc, {
    root = undefined           :: yaml_node() | yaml_simple_node() | undefined
  }).
-type yaml_doc()               :: #yaml_doc{root :: yaml_node()}.
-type yaml_simple_doc()        :: #yaml_doc{root :: yaml_simple_node()}.
-type yaml_partial_doc()       :: #yaml_doc{}.

%% -------------------------------------------------------------------
%% Final data type specifications.
%% -------------------------------------------------------------------

-type yaml_user_node()        :: tuple().
-type yaml_user_simple_node() :: term().

-type yaml_node()         :: yaml_seq()
                           | yaml_map()
                           | yaml_str()
                           | yaml_null()
                           | yaml_bool()
                           | yaml_int()
                           | yaml_timestamp()
                           | yaml_erlang_atom()
                           | yaml_erlang_fun()
                           | yaml_user_node().

-type yaml_simple_node()  :: yaml_simple_seq()
                           | yaml_simple_map()
                           | yaml_simple_str()
                           | yaml_simple_null()
                           | yaml_simple_bool()
                           | yaml_simple_int()
                           | yaml_simple_timestamp()
                           | yaml_simple_erlang_atom()
                           | yaml_simple_erlang_fun()
                           | yaml_user_simple_node().

-type yaml_partial_node() :: yaml_partial_seq()
                           | yaml_partial_map()
                           | yaml_str()
                           | yaml_null()
                           | yaml_bool()
                           | yaml_int()
                           | yaml_timestamp()
                           | yaml_erlang_atom()
                           | yaml_erlang_fun()
                           | yaml_user_node()
                           | yaml_simple_str()
                           | yaml_simple_null()
                           | yaml_simple_bool()
                           | yaml_simple_int()
                           | yaml_simple_timestamp()
                           | yaml_simple_erlang_atom()
                           | yaml_simple_erlang_fun()
                           | yaml_user_simple_node().

%% -------------------------------------------------------------------
%% Macros to access common members of the node records.
%% -------------------------------------------------------------------

-define(NODE_MOD(N),  element(#yaml_str.module, N)).
-define(NODE_TAG(N),  element(#yaml_str.tag, N)).
-define(NODE_PRES(N), element(#yaml_str.pres, N)).

%% -------------------------------------------------------------------
%% List of modules implementing the Core Schema nodes.
%% -------------------------------------------------------------------

-define(FAILSAFE_SCHEMA_MODS, [
    yaml_node_str,
    yaml_node_seq,
    yaml_node_map
  ]).

-define(JSON_SCHEMA_MODS, [
    yaml_node_json_null,
    yaml_node_json_bool,
    yaml_node_json_int,
    yaml_node_json_float,
    yaml_node_str,
    yaml_node_seq,
    yaml_node_map
  ]).

-define(CORE_SCHEMA_MODS, [
    yaml_node_null,
    yaml_node_bool,
    yaml_node_int,
    yaml_node_float,
    yaml_node_str,
    yaml_node_seq,
    yaml_node_map
  ]).

-endif.
