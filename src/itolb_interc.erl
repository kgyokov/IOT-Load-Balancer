%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2017 11:37 PM
%%%-------------------------------------------------------------------
-module(itolb_interc).
-author("Kalin").

-include_lib("mqttl/include/mqttl_packets.hrl").

-callback connect(Packet::#'CONNECT'{},Ctx) ->
  {ok, NewPacket::#'CONNECT'{}, NewCtx::any()}     %% Ctx can contain things like Auth claims, etc.
  |{error,Reason::any()}.                          %% error, e.g. invalid password

-callback client_to_broker(Packet::mqttl_packet(),Ctx) ->
  {ok, NewPacket::mqttl_packet(), NewCtx::any()}     %% Ctx can contain things like Auth claims, etc.
  |{error,Reason::any()}.                          %% error, e.g. invalid password

-callback broker_to_client(Packet::mqttl_packet(),Ctx) ->
  {ok, NewPacket::mqttl_packet(), NewCtx::any()}     %% Ctx can contain things like Auth claims, etc.
  |{error,Reason::any()}.
