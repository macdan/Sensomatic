-module( sensomatic ).

-export( [ 
	start/0,
	zones/0, 
	add_zone/1,
	remove_zone/1,
	devices/0,
	ports/0
] ).

-behaviour( application ).
-export( [ start/2, stop/1 ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% start/0
%%
%% @doc Start sensomatic.
%%==============================================================================
start() -> 
	application:start( crypto ),
	application:start( ?MODULE ).

%%==============================================================================
%% zones/0
%%
%% @doc Get a list of running zones
%%==============================================================================
zones() -> 
	lists:map( fun( { Id, Pid, _, _ } ) -> 
		{ Id, Pid } 
	end, supervisor:which_children( zone_sup ) ).

%%==============================================================================
%% add_zone/1
%%
%% @doc Start a new zone with the given Id
%%==============================================================================
add_zone( Id ) ->
	zone_sup:start_zone( Id ).
	
%%==============================================================================
%% remove_zone/1
%% 
%% @doc Kill the specified zone
%%==============================================================================
remove_zone( Id ) ->
	zone_sup:stop_zone( Id ).

%%==============================================================================
%% devices/0
%%
%% @doc Get a list of registered devices
%%==============================================================================
devices() -> 
	lists:map( fun( { Id, Pid, _, _ } ) ->
		{ Id, Pid }
	end, supervisor:which_children( arduino_device_sup ) ).

%%==============================================================================
%% ports/0
%%
%% @doc Get a list of all ports from all devices
%%==============================================================================
ports() ->
	lists:flatten( lists:map( fun( { DeviceId, DevicePid } ) ->
		lists:map( fun( { PortId, PortPid, _ } ) ->
			{ { DeviceId, DevicePid }, { PortId, PortPid } }
		end, arduino_device:get_ports( DevicePid ) )
	end, devices() ) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% start/2
%%==============================================================================
start( Type, Args ) -> 
	util:shout( "Starting ~s Type: ~p Args: ~p", [ ?MODULE, Type, Args ] ),
	register( ?MODULE, self() ),
	
	sensomatic_web_deps:ensure(),
	sensomatic_sup:start_link().

%%==============================================================================
%% stop/1
%%==============================================================================
stop( _ ) -> ok.