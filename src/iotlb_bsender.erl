%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2017 11:35 PM
%%%-------------------------------------------------------------------
-module(iotlb_bsender).
-author("Kalin").

-behaviour(gen_server).

%% API
-export([start_link/2, forward_to_server/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {transport,client_socket,broker_socket,opts,broker_spec}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link({Transport::any(),Socket::any(),Opts::[any()]},BrokerSpec::any()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link({Transport,Socket,Opts},BrokerSpec) ->
  gen_server:start_link(?MODULE, [Transport,Socket,Opts,BrokerSpec], []).

forward_to_server(Pid,Packet) ->
  gen_server:call(Pid,{to_broker,Packet}).

%%%%@todo: Bi-directional packet inspection
%%forward_to_client(Pid,Binary) ->
%%  gen_server:call(Pid,{to_client,Binary}).

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
init([Transport,Socket,Opts,BrokerSpec]) ->
  process_flag(trap_exit,true),
  self() ! async_init,
  {ok, #state{transport = Transport,
              client_socket = Socket,
              opts = Opts,
              broker_spec = BrokerSpec}}.

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

handle_call({to_broker,Packet},_From, S = #state{broker_socket = BrokerSocket}) ->
  Binary = mqttl_builder:build_packet(Packet),
  error_logger:info_msg("Sending to broker Packet ~p with Binary ~p~n",[Packet,Binary]),
  case gen_tcp:send(BrokerSocket,Binary)of
    ok ->
      {reply,ok,S};
    {error, _Reason} ->
      {stop, unable_to_send, {error,_Reason}, S}
  end;

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

handle_info(async_init, S = #state{broker_spec = {Address,Port}}) ->
  {ok,Socket} = gen_tcp:connect(Address,Port,[binary, {active,true}],10000),
  {noreply, S#state{broker_socket = Socket}};

handle_info({tcp,_,Binary},S = #state{client_socket = ClientSocket,
                                      transport = Transport}) ->
  error_logger:info_msg("Sending back binary ~p~n",[Binary]),
  Transport:send(ClientSocket,Binary),
  {noreply,S};

handle_info({tcp_closed, _Socket},S) ->
  {stop,tcp_closed,S};

handle_info({tcp_error, _Socket, _Reason},S) ->
  {stop,tcp_error,S};

handle_info(_Info, State) ->
  error_logger:info_msg("Unexpected message ~p~n",[_Info]),
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
terminate(_Reason, #state{broker_socket = Socket}) ->
  error_logger:info_msg("bidirectional sender terminatig with reason ~p~n",[_Reason]),
  case Socket of
    undefined -> ok;
    _ -> gen_tcp:close(Socket)
  end.

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
