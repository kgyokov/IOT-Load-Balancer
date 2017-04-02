%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2017 8:13 PM
%%%-------------------------------------------------------------------
-module(iotlb_stats_json).
-author("Kalin").

%% API
-export([encode_stats/1, stats/1]).

encode_stats(LBStats) ->
  Map = stats(LBStats),
  jsone:encode(Map).

stats(LBStats) ->
  lists:map(fun node_stats/1,LBStats).

node_stats({Node,Brokers}) ->
  #{node => Node, brokers => lists:map(fun broker_stats/1, Brokers)}.

broker_stats({Broker,BStats}) ->
  #{broker => broker(Broker), stats => BStats}.

broker({Host,Port}) ->
  #{host => host(Host), port => Port}.

host(Name) when is_atom(Name); is_binary(Name); is_list(Name) -> Name;
host(IP = {_B1,_B2,_B3,_B4}) -> inet:ntoa(IP);
host(IP = {_B1,_B2,_B3,_B4,_B5,_B6,_B7,_B8}) -> inet:ntoa(IP).

