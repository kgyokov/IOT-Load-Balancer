%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2017 8:42 PM
%%%-------------------------------------------------------------------
-module(iotlb_stats_col).
-author("Kalin").

-behaviour(gen_server).

%% API
-export([start_link/0, get_stats/0, get_all_stats/0, connected/3]).

-export_type([stats/0,broker_stats/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(TABLE, ?MODULE).

%%@todo: replace with broker() type from other branch
-type broker() :: {inet:hostname()|inet:ip_address(),inet:port_number()}.
-type bstats() :: non_neg_integer().
-type broker_stats()::{any(),bstats()}.
-type node_stats()::{node(),[broker_stats()]}.
-type lbstats() :: [node_stats()].

-record(state, {
  stats
}).

%%%===================================================================
%%% API
%%%===================================================================

connected(Pid,ClientId,Broker) ->
  gen_server:cast(?SERVER,{connected,Pid,ClientId,Broker}).

-spec get_stats() -> broker_stats().
get_stats() ->
  gen_server:call(?SERVER,get_stats).

-spec get_all_stats() -> [node_stats()].
get_all_stats() -> get_all_stats(5000).

-spec get_all_stats(Timeout::non_neg_integer()) -> [node_stats()].
get_all_stats(Timeout) ->
  gen_server:multi_call(nodes(),?SERVER,get_stats,Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(?TABLE,[set,named_table]),
  {ok, #state{stats = iotlb_stats:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(get_stats, _From, S = #state{stats = Stats}) ->
  Response = iotlb_stats:get_stats(Stats),
  {reply, Response, S};

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({connected,Pid,ClientId,Broker}, S = #state{stats = Stats}) ->
  Ref = monitor(process,Pid),
  ets:insert_new(?TABLE,{Ref,ClientId,Broker}),
  Stats1 = iotlb_stats:connected(ClientId,Broker,Stats),
  {noreply, S#state{stats = Stats1}};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info({'DOWN', Ref, _, _, _}, S = #state{stats = Stats}) ->
  %% This check might not event be necessary if we are sure we will only get 'DOWN' messages from connection processes
  S1 =
    case ets:lookup(?TABLE,Ref) of
      [] -> ok;
      [{Ref,ClientId,Broker}] ->
        ets:delete(?TABLE,Ref),
        S#state{stats = iotlb_stats:disconnected(ClientId,Broker,Stats)}
    end,
  {noreply, S1};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ets:delete(?TABLE).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
