%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2017 10:17 PM
%%%-------------------------------------------------------------------
-module(iotlb_gui_ws).
-author("Kalin").

%% API
-export([init/3, websocket_init/3, websocket_info/3, websocket_handle/3, websocket_terminate/3]).

%%@todo: More push-based stats
-record(state,{
  timeout ::non_neg_integer(),
  period  ::non_neg_integer()
}).

%%% ======================================================================
%%% API
%%% ======================================================================

%%% ======================================================================
%%% Callbacks
%%% ======================================================================

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, Opts) ->
  Resp =
    case validate_sec_protocol(Req) of
      error -> {shutdown, Req};
      {ok,Req1} ->
        Period = proplists:get_value(period,Opts,250),
        TimeOut = proplists:get_value(timeout,Opts,3000),
        S = #state{timeout = TimeOut,period = Period},
        schedule_stats(Period),
        {ok,Req1,S}
    end,
  Resp.

validate_sec_protocol(Req) ->
  case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
    {ok, undefined, _} ->
      {ok, Req};
    {ok, SubProts, Req2} ->
      case lists:keymember(<<"mqtt">>, 1, SubProts) of
        true ->
          Req3 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,<<"mqtt">>,Req2),
          {ok, Req3};
        false ->
          error
      end
  end.

websocket_handle(_Frame, Req, S = #state{}) ->
  {ok, Req, S}.

%%websocket_handle(_Frame, Req, #state{}) ->
%%  {shutdown,Req}.

websocket_info(get_stats, Req, S = #state{period = Period,timeout = Timeout}) ->
  Stats = iotlb_stats_col:get_all_stats(Timeout),
  schedule_stats(Period),
  {reply, {binary, Stats}, Req, S};

websocket_info(_Msg,Req,S) ->
  {ok,Req,S}.

websocket_terminate(_Reason,_Req,_S) ->
  ok.

schedule_stats(Period) -> erlang:send_after(Period,self(),get_stats).