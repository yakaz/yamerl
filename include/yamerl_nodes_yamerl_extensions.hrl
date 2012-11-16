-ifndef(yamerl_nodes_yamerl_extensions_hrl).
-define(yamerl_nodes_yamerl_extensions_hrl, true).

-include("yamerl_types.hrl").

%% CAUTION:
%% Records defined in this file have default values for all members.
%% Those default values are often bad values but this is needed so that
%% Erlang won't add "undefined" in our back to the allowed values in the
%% type specifications.

%% -------------------------------------------------------------------
%% Nodes specifications.
%% -------------------------------------------------------------------

%% IP address/range/netmask.
-record(yamerl_ip_addr, {
    module  = undefined          :: atom(),
    tag     = "!"                :: tag_uri(),
    pres    = []                 :: list(),
    address = {0, 0, 0, 0}       :: inet:ip_address()
  }).
-type yamerl_ip_addr()           :: #yamerl_ip_addr{}.
-type yamerl_simple_ip_addr()    :: inet:ip_address().

-record(yamerl_ip_netmask, {
    module  = undefined          :: atom(),
    tag     = "!"                :: tag_uri(),
    pres    = []                 :: list(),
    address = {0, 0, 0, 0}       :: inet:ip_address(),
    mask    = 1                  :: pos_integer()
  }).
-type yamerl_ip_netmask()        :: #yamerl_ip_netmask{}.
-type yamerl_simple_ip_netmask() :: {inet:ip_address(), pos_integer()}.

-record(yamerl_ip_range, {
    module  = undefined          :: atom(),
    tag     = "!"                :: tag_uri(),
    pres    = []                 :: list(),
    from    = {0, 0, 0, 0}       :: inet:ip_address(),
    to      = {0, 0, 0, 0}       :: inet:ip_address()
  }).
-type yamerl_ip_range()          :: #yamerl_ip_range{}.
-type yamerl_simple_ip_range()   :: {inet:ip_address(), inet:ip_address()}.

-endif.
