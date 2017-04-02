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
-export([encode_stats/1]).

encode_stats(LBStats) ->
  Json = lists:map(fun node_stats/1,LBStats),
  jsone:encode(Json).

node_stats({Node,Brokers}) ->
  #{node => Node, brokers => lists:map(fun broker_stats/1, Brokers)}.

broker_stats({Broker,BStats}) ->
  #{broker => broker(Broker), stats => BStats}.

broker({Host,Port}) ->
  #{host => host(Host), port => Port}.

host(<<Name>>) -> Name;
host({B1,B2,B3,B4}) -> ip([B1,B2,B3,B4]);
host({B1,B2,B3,B4,B5,B6}) -> ip([B1,B2,B3,B4,B5,B6]).

ip(L) ->
  L1 = lists:map(fun integer_to_list/1, L),
  L2 = string:join(L1,"."),
  iolist_to_binary(L2).

%%  [H|T] = lists:map(fun integer_to_list/1, L),
%%  L2 = lists:foldl(fun(Str,Acc) -> [Str,"."|Acc] end, H, T),
%%  iolist_to_binary(lists:reverse(L2)).

