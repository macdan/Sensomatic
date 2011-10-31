-module( zone_sup ).

-export( [ 
	start_link/0, 
	start_zone/1,
	stop_zone/1
] ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

%% Module API

start_link() ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

start_zone( Id ) ->
	ChildSpec = { Id, 
		{ zone, start_link, [] }, 
		temporary, 1000, worker, [ zone ] 
	},
	supervisor:start_child( ?MODULE, ChildSpec ).

stop_zone( Id ) ->
	supervisor:terminate_child( ?MODULE, Id ).

%% supervisor callbacks

init( [] ) ->
	{ ok, { { one_for_one, 60, 3600 }, [] } }.