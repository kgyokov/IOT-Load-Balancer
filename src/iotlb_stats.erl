%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2017 9:08 PM
%%%-------------------------------------------------------------------
-module(iotlb_stats).
-author("Kalin").

%% API
-export([new/0, connected/3, disconnected/3, get_stats/1]).

-record(state,{
  brokers :: orddict:orddict()
}).

new() -> #state{brokers = orddict:new()}.

connected(_ClientId,Broker,S = #state{brokers = Brokers}) ->
  S#state{brokers = orddict:update(Broker,fun(Count) -> Count+1 end,1,Brokers)}.

disconnected(_ClientId,Broker,S = #state{brokers = Brokers}) ->
  S#state{brokers = orddict:update(Broker,fun(Count) -> Count-1 end,Brokers)}.

get_stats(#state{brokers = Brokers}) ->
  orddict:to_list(Brokers).


