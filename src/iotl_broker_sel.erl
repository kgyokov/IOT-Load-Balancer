%%%-------------------------------------------------------------------
%%% @author Kalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2017 9:39 PM
%%%-------------------------------------------------------------------
-module(iotl_broker_sel).
-author("Kalin").

-include_lib("../deps/mqttl/include/mqttl_packets.hrl").


-export_type([broker/0]).

-type broker() :: {Host::inet:hostname()|inet:ip_address(),
                   Port::inet:port_number()}.

-callback select(#'CONNECT'{}) -> broker().
