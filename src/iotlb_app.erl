-module(iotlb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

-define(APPLICATION,iotlb).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> application:ensure_all_started(?APPLICATION).

start(_StartType, _StartArgs) ->
    start_listeners(),
    start_gui(),
    BrokerSel = get_broker_sel(),
    iotlb_sup:start_link(BrokerSel).


start_listeners() ->
    ProtOpts = [{shutdown,5000}],
    ConnOpts = #{},
    TransportOpts =[
        {tcp,[{port,1883}]},
%%        {ssl,[
%%            {port,5556},
%%            {certfile, ""},
%%            {cacertfile, ""},
%%            {verify, verify_peer}
%%        ]},
        {http,[{port, 80}]}
%%        {https,[{port, 443}]}
    ],
    [{ok,_} = mqttl_sup:start_listener(T,Opts,iotlb_conn,ConnOpts,ProtOpts) || {T,Opts} <- TransportOpts].


start_gui() ->
    ProcessOpts  = [],
    Dispatch = cowboy_router:compile([
        %% {HostMatch, list({PathMatch, Handler, Opts})}
        {'_',
            [
                {"/ws/stats", iotlb_gui_ws, ProcessOpts},
                {"/stats", iotlb_gui, ProcessOpts},
                {"/", cowboy_static, {priv_file, iotlb, "static/index.html"}},
                {"/[...]", cowboy_static, {priv_dir, iotlb, "static/[...]"}}
            ]
        }
    ]),
    ProtOpts =
        [{env, [{dispatch, Dispatch}]},
         {shutdown,5000}],
    TransOpts =[{port,12000}],
    {ok,_} = cowboy:start_http(gui_http,10,TransOpts,ProtOpts).


get_broker_sel() ->
    BrokerSel = application:get_env(?APPLICATION,broker_selector,iotlb_broker_selection),
    BrokersStr = [io_lib:format("~p~n",[Broker]) || Broker <- BrokerSel:get_brokers()],
    error_logger:info_msg("Starting with available brokers: " ++ lists:flatten(BrokersStr)),
    BrokerSel.

stop(_State) ->
    ok.
