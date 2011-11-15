-module( arduino_sup ).

-export( [ 
	start_link/0,
	start_worker/1
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

%%==============================================================================
%% start_worker/1
%%==============================================================================
start_worker( Mfa = { Mod, _, _ } ) ->
	start_worker( { Mod, Mfa } );
start_worker( { Id, Mfa = { Mod, _, _ } } ) ->
	ChildSpec = { Id, Mfa, temporary, 1000, supervisor, [ Mod ] },
	supervisor:start_child( ?MODULE, ChildSpec ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% init/1
%%==============================================================================
init( [] ) ->
	{ ok, { { one_for_one, 60, 3600 }, [
		
		{ arduino_device_sup, { arduino_device_sup, start_link, [] }, temporary, 1000, supervisor, [ arduino_device_sup ] },
		
		{ arduino_listener, { arduino_listener, start_link, [ 8181, [
			{ active, true },
			{ packet, line },
			{ reuseaddr, true }
		] ] }, temporary, 1000, worker, [ arduino_listener ] }

	] } }.