%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2017 9:53 PM
%%%-------------------------------------------------------------------
-module(iotlb_stats_json_tests).
-author("Kalin").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  Stats = [
    {<<"node1">>,
      [
        {
          {{127,0,0,1},1234},
          1234
        },
        {
          {<<"broker1">>,1234},
          1234
        }
%%        {
%%          {"broker2",1234},
%%          1234
%%        },
%%        {
%%          {broker3,1234}
%%        }
      ]
    }
  ],
  ?assertNotException(_,_,iotlb_stats_json:encode_stats(Stats)).
