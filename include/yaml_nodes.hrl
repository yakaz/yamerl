%% -------------------------------------------------------------------
%% Nodes specifications.
%% -------------------------------------------------------------------

%% String.
-record(yaml_string, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    text   = ""                :: string()
  }).
-type yaml_string()            :: #yaml_string{}.
-type yaml_simple_string()     :: string().

-record(yaml_erlang_fun, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    function                   :: function(),
    text                       :: string()
  }).
-type yaml_erlang_fun()        :: #yaml_erlang_fun{}.
-type yaml_simple_erlang_fun() :: function().

-record(yaml_sequence, {
    module  = undefined        :: atom(),
    tag     = "!"              :: tag_uri(),
    pres    = []               :: list(),
    entries = []               :: [yaml_node()],
    count   = 0                :: non_neg_integer()
  }).
-type yaml_sequence()          :: #yaml_sequence{}.
-type yaml_simple_sequence()   :: [yaml_simple_node()].
-type yaml_partial_sequence()  :: {
  atom(),
  {seq, non_neg_integer()},
  [yaml_node() | yaml_simple_node() | '$insert_here']
}.

-record(yaml_mapping, {
    module = undefined         :: atom(),
    tag    = "!"               :: tag_uri(),
    pres   = []                :: list(),
    pairs  = []                :: [{yaml_node(), yaml_node()}]
  }).
-type yaml_mapping()           :: #yaml_mapping{}.
-type yaml_simple_mapping()    :: [{yaml_simple_node(), yaml_simple_node()}].
-type yaml_partial_mapping()   :: {
  atom(),
  {map, yaml_node() | yaml_simple_node() | undefined},
  [{
      yaml_node() | yaml_simple_node() | '$insert_here',
      yaml_node() | yaml_simple_node() | '$insert_here' | undefined
    }]
}.

-record(yaml_document, {
    root = undefined           :: yaml_node() | yaml_simple_node() | undefined
  }).
-type yaml_document()          :: #yaml_document{root :: yaml_node()}.
-type yaml_simple_document()   :: #yaml_document{root :: yaml_simple_node()}.
-type yaml_partial_document()  :: #yaml_document{}.

%% -------------------------------------------------------------------
%% Final data type specifications.
%% -------------------------------------------------------------------

-type yaml_node()         :: yaml_sequence()
                           | yaml_mapping()
                           | yaml_string()
                           | yaml_erlang_fun().

-type yaml_simple_node()  :: yaml_simple_sequence()
                           | yaml_simple_mapping()
                           | yaml_simple_string()
                           | yaml_simple_erlang_fun().

-type yaml_partial_node() :: yaml_partial_sequence()
                           | yaml_partial_mapping()
                           | yaml_string()
                           | yaml_erlang_fun()
                           | yaml_simple_string()
                           | yaml_simple_erlang_fun().
