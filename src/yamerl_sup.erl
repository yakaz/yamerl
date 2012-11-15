-module(yamerl_sup).

-behaviour(supervisor).

%% Initialization.
-export([
    start_link/0
  ]).

%% supervisor(3erl) callbacks.
-export([
    init/1
  ]).

%% -------------------------------------------------------------------
%% Initialization.
%% -------------------------------------------------------------------

-spec start_link() ->
    {ok, pid()} |
    ignore      |
    {error, {already_started, pid()} | shutdown | term()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% -------------------------------------------------------------------
%% supervisor(3erl) callbacks.
%% -------------------------------------------------------------------

init(_) ->
    {ok, {
        {one_for_one, 10, 1}, [
        ]
      }}.
