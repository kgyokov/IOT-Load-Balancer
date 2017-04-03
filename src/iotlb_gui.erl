%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2017 8:12 PM
%%%-------------------------------------------------------------------
-module(iotlb_gui).
-author("Kalin").

%% API
-export([init/3, terminate/3, handle/2]).

-record(state,{timeout :: non_neg_integer()}).

init(_Type, Req, Opts) ->
  TimeOut = proplists:get_value(timeout,Opts,3000),
  {ok, Req, #state{timeout = TimeOut}}.

handle(Req, S = #state{timeout = TimeOut}) ->
  Stats = iotlb_stats_col:get_all_stats(TimeOut),
  Enc = iotlb_stats_json:encode_stats(Stats),
  {ok, Req2} = cowboy_req:reply(200, [
    {<<"Content-Type">>, <<"application/json">>}
  ], Enc, Req),
  {ok, Req2, S}.


terminate(_Reason, _Req, _S) ->
  ok.