-module(iotlb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ChildSpec = [
        ?CHILD(iotlb_stats_col,worker),
        ?CHILD(iotlb_conn_sup_sup,supervisor)
    ],
    {ok, { {rest_for_one, 5, 10}, ChildSpec} }.

