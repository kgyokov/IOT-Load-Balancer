%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2017 12:49 AM
%%%-------------------------------------------------------------------
-module(iotlb_broker_selection).
-author("Kalin").

-include_lib("../deps/mqttl/include/mqttl_packets.hrl").

%% API
-export([select_broker/1, get_brokers/0]).

get_brokers() ->
  {ok,Brokers} = application:get_env(brokers),
  Brokers.

select_broker(#'CONNECT'{client_id = ClientId}) ->
  %%@todo: inject these values in a better way
  %%@todo: Better way to select a broker
  {ok,Brokers} = application:get_env(brokers),
  <<BrokerNum:32,_Rest/binary>> = erlang:md5(ClientId),
  Index = (BrokerNum rem length(Brokers))+1,
  lists:nth(Index,Brokers).



