%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2017 1:16 AM
%%%-------------------------------------------------------------------
-module(iotlb_interc_default).
-author("Kalin").

-behavior(iotlb_interc).

%% API
-export([connect/2, client_to_server/2, server_to_client/2]).


connect(Packet, Ctx) -> {{forward,Packet},Ctx}.
client_to_server(Packet, Ctx) -> default(Packet,Ctx).
server_to_client(Packet, Ctx) -> default(Packet,Ctx).

default(Packet,Ctx) -> {noreply,{forward,Packet},Ctx}.