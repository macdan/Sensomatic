-module( sensomatic_sup ).

-export( [ 
	start_link/0
] ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% start_link/0
%%==============================================================================
start_link() ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% init/1
%%==============================================================================
init( [] ) ->
	{ ok, { { one_for_one, 60, 3600 }, [
		
		{ device_sup, { device_sup, start_link, [] }, temporary, 1000, supervisor, [ device_sup ] },
		
		{ zone_sup, { zone_sup, start_link, [] }, temporary, 1000, supervisor, [ zone_sup ] },
		
		{ listener, { listener, start_link, [ 8181, [
			{ active, true },
			{ packet, line },
			{ reuseaddr, true }
		] ] }, temporary, 1000, worker, [ listener ] },
		
		{ sensomatic_web_sup, { sensomatic_web_sup, start_link, [] }, temporary, 1000, supervisor, [ sensomatic_web_sup ] }

	] } }.