-module( port_sup ).
-behaviour( supervisor ).

-export( [ start_link/0, start_child/2 ] ).
-export( [ init/1 ] ).

%% Module API

start_link() ->
    supervisor:start_link( ?MODULE, [] ).

start_child( Pid, { DevicePid, Id, Type } ) ->
    supervisor:start_child( Pid, { 
    	Id, 
    	{ port, start_link, [ DevicePid, Id, Type ] }, 
    	permanent, 
    	1000, 
    	worker, 
    	[ port ] 
    } ).

%% supervisor callbacks

init( [] ) ->
	{ ok, { { one_for_one, 60, 3600 }, [] } }.