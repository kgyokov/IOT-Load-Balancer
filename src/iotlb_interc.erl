%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2017 11:37 PM
%%%-------------------------------------------------------------------
-module(iotlb_interc).
-author("Kalin").

-include_lib("mqttl/include/mqttl_packets.hrl").

-callback connect(Packet::#'CONNECT'{},Ctx::any()) ->
  {
    {stop,Code::non_neg_integer()}
    | {forward, NewPacket::#'CONNECT'{}},
    NewCtx::any()
  }.     %% Ctx can contain things like Auth claims, etc. %% error, e.g. invalid password

-callback client_to_server(Packet::mqttl_packet(),Ctx::any()) ->
  {
    noreply
    | {reply|mqttl_packet()},
    stop
    | {forward, NewPacket::mqttl_packet()},
    NewCtx::any()
  }.     %% Ctx can contain things like Auth claims, etc. %% error, e.g. invalid password

-callback server_to_client(Packet::mqttl_packet(),Ctx::any()) ->
  {
    noreply
    | {reply|mqttl_packet()},
    stop
    | {forward, NewPacket::mqttl_packet()},
    NewCtx::any()
  }.     %% Ctx can contain things like Auth claims, etc. %% error, e.g. invalid password