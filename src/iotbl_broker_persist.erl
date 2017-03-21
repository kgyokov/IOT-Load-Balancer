%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2017 9:53 PM
%%%-------------------------------------------------------------------
-module(iotbl_broker_persist).
-author("Kalin").

%% API
-export([get_or_select/2]).

-record(broker_sel, {
  client_id   :: binary(),
  broker      :: iotl_broker_sel:broker()
}).

-define(TABLE,#broker_sel{}).


get_or_create(BrokerFun,ClientId) ->
  fun() ->
    %% take write lock
    case mnesia:read(?TABLE,ClientId,write) of
      [] ->
        %error_logger:info_msg("empty result"),
        Broker = BrokerFun(),
        mnesia:write(?TABLE,#broker_sel{broker = Broker,client_id = ClientId},write),
        %error_logger:info_msg(P),
        {ok,Broker};
      [#broker_sel{broker = Broker}] ->
        {ok,Broker}
    end
  end.

get_or_select(BrokerFun,ClientId) ->
  Fun = get_or_create(BrokerFun,ClientId),
  mnesia:transaction(Fun).
