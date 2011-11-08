-module( sensomatic_sup ).

-export( [ 
	start_link/0,
	start_child/1,
	restart_child/1,
	terminate_child/1,
	delete_child/1,
	which_children/0,
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
%% start_child/1
%%==============================================================================
start_child( ChildSpec ) ->
	supervisor:start_child( ?MODULE, ChildSpec ).

%%==============================================================================
%% restart_child/1
%%==============================================================================
restart_child( Id ) ->
	supervisor:restart_child( ?MODULE, Id ).

%%==============================================================================
%% terminate_child/1
%%==============================================================================
terminate_child( Id ) ->
	supervisor:terminate_child( ?MODULE, Id ).

%%==============================================================================
%% delete_child/1
%%==============================================================================
delete_child( Id ) ->
	supervisor:delete_child( ?MODULE, Id ).

%%==============================================================================
%% which_children/0
%%==============================================================================
which_children() ->
	supervisor:which_children( ?MODULE ).

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
		
		{ device_sup, { device_sup, start_link, [] }, temporary, 1000, supervisor, [ device_sup ] },
		
		{ zone_sup, { zone_sup, start_link, [] }, temporary, 1000, supervisor, [ zone_sup ] },
		
		{ listener, { listener, start_link, [ 8181, [
			{ active, true },
			{ packet, line },
			{ reuseaddr, true }
		] ] }, temporary, 1000, worker, [ listener ] },
		
		{ sensomatic_web_sup, { sensomatic_web_sup, start_link, [] }, temporary, 1000, supervisor, [ sensomatic_web_sup ] }

	] } }.