%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2017 11:01 PM
%%%-------------------------------------------------------------------
-module(iotlb_conn).
-author("Kalin").

-include_lib("../deps/mqttl/include/mqttl_packets.hrl").
-behaviour(gen_server).

%% API
-export([start_link/3, new/3, unexpected_disconnect/2, bad_packet/2, handle_packet/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  transport_info ::any(),
  connected::boolean(),
  sup_pid :: pid(),
  sender_pid :: pid(),                 %% The process sending to the actual device
  receiver_pid ::pid()               %% The process receiving from the actual device TODO: Do we even need to know this???
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ReceiverPid::pid(),SupPid::pid(),TSO::any()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ReceiverPid,SupPid,TSO) ->
  gen_server:start_link(?MODULE, [ReceiverPid,SupPid,TSO], []).


new(Transport,Socket,Opts) ->
  TSO = {Transport,Socket,Opts},
  iotlb_conn_sup_sup:start_connection(self(),TSO).

%%todo: Maybe just have the receiver quit let the mqttl_conn process be killed?
unexpected_disconnect(_Pid,_Details) -> ok. %%gen_server:cast(Pid,{disconnect,Details}).

%%todo: Maybe just have the receive quit and let the mqttl_conn process be killed?
bad_packet(_Pid,_Reason) -> ok. %%gen_server:cast(Pid,{bad_packet,Reason}).

handle_packet(Pid,Packet) ->
  gen_server:cast(Pid,{packet,Packet}).

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
init([ReceiverPid,SupPid,TSO]) ->
  link(ReceiverPid),
  {ok, #state{receiver_pid = ReceiverPid,
              sup_pid = SupPid,
              connected = false,
              transport_info = TSO}
  }.

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
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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

handle_cast({packet,Packet = #'CONNECT'{}},S = #state{connected = false,
                                                      sup_pid = SupPid,
                                                      transport_info = TSO}) ->

  %%@todo: Packet validation to avoid bad packets being sent to the wrong server???
  BrokerSpec = iotlb_broker_selection:select_broker(Packet),
  {ok,Sender} = iotlb_conn_sup:start_bidirectional_sender(SupPid,TSO,BrokerSpec),
  iotlb_bsender:forward_to_server(Sender,Packet),
  {noreply, S#state{sender_pid = Sender,connected = true}};

handle_cast({packet,#'CONNECT'{}},S = #state{connected = true}) ->
  {stop, duplicate_connect, S}; %%Maybe specify a different reason

handle_cast({packet,_Packet},S = #state{connected = false}) ->
  {stop, premature_packet, S}; %%Maybe specify a different reason

handle_cast({packet,Packet},S = #state{connected = true,sender_pid = Sender}) ->
  iotlb_bsender:forward_to_server(Sender,Packet),
  {noreply, S};

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
  ok.

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
