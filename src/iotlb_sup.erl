-module(iotlb_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CONN_SUP_SUP(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(STATS_COLS(BrokerSel, I, Type), {I, {I, start_link, [BrokerSel]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(BrokerSel) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [BrokerSel]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([BrokerSel]) ->
    ChildSpec = [
        ?STATS_COLS(BrokerSel,iotlb_stats_col,worker),
        ?CONN_SUP_SUP(iotlb_conn_sup_sup,supervisor)
    ],
    {ok, { {rest_for_one, 5, 10}, ChildSpec} }.

