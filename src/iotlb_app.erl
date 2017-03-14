-module(iotlb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> application:ensure_all_started(iotlb).

start(_StartType, _StartArgs) ->
    TransOpts = [
        {port,1883}
    ],
    _SslTransOpts = [
        {port,5556},
        {certfile, ""},
        {cacertfile, ""},
        {verify, verify_peer}
    ],
    ProtOpts = [
        {shutdown,5000},
        %%{connection_type, supervisor} %% mqtt_ranch_sup is an adaptor for the mqtt_connection_sup
        {connection_type, worker} %% mqtt_ranch_sup is an adaptor for the mqtt_connection_sup
    ],

    {ok,_} = mqttl_sup:start_listener(tcp,TransOpts,iotlb_conn,ProtOpts),
    iotlb_sup:start_link().

stop(_State) ->
    ok.
