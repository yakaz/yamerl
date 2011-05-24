-ifndef(yaml_nodes_yakaz_hrl).
-define(yaml_nodes_yakaz_hrl, true).

-include("yaml_types.hrl").

%% CAUTION:
%% Records defined in this file have default values for all members.
%% Those default values are often bad values but this is needed so that
%% Erlang won't add "undefined" in our back to the allowed values in the
%% type specifications.

%% -------------------------------------------------------------------
%% Nodes specifications.
%% -------------------------------------------------------------------

%% IP address/range/netmask.
-record(yaml_ip_addr, {
    module  = undefined        :: atom(),
    tag     = "!"              :: tag_uri(),
    pres    = []               :: list(),
    address = {0, 0, 0, 0}     :: inet:ip_address()
  }).
-type yaml_ip_addr()           :: #yaml_ip_addr{}.
-type yaml_simple_ip_addr()    :: inet:ip_address().

-record(yaml_ip_netmask, {
    module  = undefined        :: atom(),
    tag     = "!"              :: tag_uri(),
    pres    = []               :: list(),
    address = {0, 0, 0, 0}     :: inet:ip_address(),
    mask    = 1                :: pos_integer()
  }).
-type yaml_ip_netmask()        :: #yaml_ip_netmask{}.
-type yaml_simple_ip_netmask() :: {inet:ip_address(), pos_integer()}.

-record(yaml_ip_range, {
    module  = undefined        :: atom(),
    tag     = "!"              :: tag_uri(),
    pres    = []               :: list(),
    from    = {0, 0, 0, 0}     :: inet:ip_address(),
    to      = {0, 0, 0, 0}     :: inet:ip_address()
  }).
-type yaml_ip_range()          :: #yaml_ip_range{}.
-type yaml_simple_ip_range()   :: {inet:ip_address(), inet:ip_address()}.

-endif.
