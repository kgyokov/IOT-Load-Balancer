-module(iotlb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> application:ensure_all_started(iotlb).

start(_StartType, _StartArgs) ->
    ProtOpts = [
        {shutdown,5000}
    ],
    ConnOpts = #{},
    TransportOpts =[
        {tcp,[
            {port,1883}
        ]},
%%        {ssl,[
%%            {port,5556},
%%            {certfile, ""},
%%            {cacertfile, ""},
%%            {verify, verify_peer}
%%        ]},
        {http,[
            {port, 80}
        ]}
    ],
    [{ok,_} = mqttl_sup:start_listener(T,Opts,iotlb_conn,ConnOpts,ProtOpts) || {T,Opts} <- TransportOpts],
    iotlb_sup:start_link().

stop(_State) ->
    ok.
