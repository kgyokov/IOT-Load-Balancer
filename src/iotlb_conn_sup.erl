%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2017 11:25 PM
%%%-------------------------------------------------------------------
-module(iotlb_conn_sup).
-author("Kalin").

-behaviour(supervisor).

%% API
-export([start_link/0, start_bidirectional_sender/3, start_connection/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(SENDER_SPEC(TSO,BrokerSpec),
  {
    sender,
    {iotlb_bsender, start_link, [TSO,BrokerSpec]},
    permanent,          % cannot recover from a lost connection
    2000,               % should be more than sufficient
    worker,             % as opposed to supervisor
    [iotlb_bsender]
  }
).

-define(CONN_SPEC(ReceiverPid,SupPid,TSO),
  {
    connection,                               %% Id
    {iotlb_conn, start_link, [ReceiverPid,SupPid,TSO]},
    permanent,                                %% must never stop
    5000,                                     %% should be more than sufficient for the process to clean up
    worker,                                   %% as opposed to supervisor
    [iotlb_conn]
  }
).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link(?MODULE, []).

start_connection(SupPid,ReceiverPid,TSO) ->
  supervisor:start_child(SupPid,?CONN_SPEC(ReceiverPid,SupPid,TSO)).

start_bidirectional_sender(SupPid,TSO,BrokerSpec) ->
  supervisor:start_child(SupPid,?SENDER_SPEC(TSO,BrokerSpec)).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  {ok, {{one_for_all, 0, 1}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
