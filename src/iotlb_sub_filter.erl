%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2017 1:47 AM
%%%-------------------------------------------------------------------
-module(iotlb_sub_filter).
-author("Kalin").

-include_lib("mqttl/include/mqttl_packets.hrl").
-define(REJECTED,?SUBSCRIPTION_FAILURE).


%% API
-export([intercept_req/4, merge_resp/2]).


intercept_req(P = #'SUBSCRIBE'{packet_id = PacketId, subscriptions = Subs},Auth,AuthCtx,Dict) ->
  NewQoS = [Auth:subscribe(Sub,AuthCtx) || Sub <- Subs],
  UpdatedSubs = lists:zipwith(fun({Topic,_},QoS) -> {Topic,QoS} end, Subs, NewQoS),
  AllowedSubs = [S || S = {_,QoS} <- UpdatedSubs, QoS =/= ?REJECTED],
  Dict1 =
    case AllowedSubs of
      [] -> {ignore,Dict};
      _ ->
        P1 = P#'SUBSCRIBE'{subscriptions = AllowedSubs},
        Dict1 = store_qos(PacketId,NewQoS,Dict),
        {{forward,P1},Dict1}
    end.


store_qos(PacketId,NewSubs,Dict) ->
  dict:store(PacketId,NewSubs,Dict).

merge_resp(P = #'SUBACK'{packet_id = PacketId,return_codes = BrokerResp},Dict) ->
  {ok,StoredQoS} = dict:find(PacketId,Dict),
  AllowedSubs = [QoS || QoS <- StoredQoS, QoS =/= ?REJECTED],
  MergedResp = lists:zipwith(fun min_qos/2, AllowedSubs, BrokerResp),
  CompleteResp = lists:reverse(replace_allowed(StoredQoS,MergedResp)),
  {P#'SUBACK'{return_codes = CompleteResp},dict:erase(PacketId,Dict)}.


min_qos(?REJECTED,_) -> ?REJECTED;
min_qos(_,?REJECTED) -> ?REJECTED;
min_qos(QoSA,QoSB) -> min(QoSA,QoSB).

replace_allowed(L1,L2)                 ->  replace_allowed(L1,L2,[]).
replace_allowed([?REJECTED|T1],L2,Acc) ->  replace_allowed(T1,L2,[?REJECTED|Acc]);
replace_allowed([_|T1],[H2|T2],Acc)    ->  replace_allowed(T1,T2,[H2|Acc]);
replace_allowed([],[],Acc)             ->  Acc.


