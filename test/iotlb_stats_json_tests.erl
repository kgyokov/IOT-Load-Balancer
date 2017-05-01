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
    {ok,<<"node1">>,
      [
        {
          {{127,0,0,1},1234},
          1234
        },
        {
          {{0,1,2,3,4,5,6,65535},1234},
          1234
        },
        {
          {<<"broker1">>,1234},
          1234
        },
        {
          {"broker2",1234},
          1234
        },
        {
          {broker3,1234},
          1234
        }
      ]
    },
    {ok,"node2",
      [
        {
          {{127,0,0,1},1234},
          1234
        }
      ]
    },
    {ok,node3,
      [
        {
          {{127,0,0,1},1234},
          1234
        }
      ]
    },
    {bad_node,<<"node4">>}
  ],
  ?assertNotException(_,_,iotlb_stats_json:encode_stats(Stats)).
