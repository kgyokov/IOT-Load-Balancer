%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2017 12:49 AM
%%%-------------------------------------------------------------------
-module(iotlb_broker_hash_sel).
-author("Kalin").

-include_lib("../deps/mqttl/include/mqttl_packets.hrl").

-behavior(iotl_broker_sel).

%% API
-export([select/1]).

select(#'CONNECT'{client_id = ClientId}) ->
  %%@todo: inject these values in a better way
  %%@todo: Better way to select a broker
  {ok,Brokers} = application:get_env(brokers),
  <<BrokerNum:32,_Rest/binary>> = erlang:md5(ClientId),
  Index = (BrokerNum rem length(Brokers))+1,
  lists:nth(Index,Brokers).

