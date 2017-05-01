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
-export([new/0, connected/3, disconnected/3, get_stats/1, new/1]).

-record(state,{
  brokers :: orddict:orddict()
}).

new() -> new([]).

new(Brokers) ->
  Dict = lists:foldl(fun(B,Acc) -> orddict:store(B,0,Acc) end, orddict:new(),Brokers),
  #state{brokers = Dict}.

connected(_ClientId,Broker,S = #state{brokers = Brokers}) ->
  S#state{brokers = orddict:update(Broker,fun(Count) -> Count+1 end,1,Brokers)}.

disconnected(_ClientId,Broker,S = #state{brokers = Brokers}) ->
  S#state{brokers = orddict:update(Broker,fun(Count) -> Count-1 end,Brokers)}.

get_stats(#state{brokers = Brokers}) ->
  orddict:to_list(Brokers).


