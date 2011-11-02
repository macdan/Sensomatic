-module( device_sup ).
-behaviour( supervisor ).

-export( [ start_link/0, start_device/1, start_child/2 ] ).
-export( [ init/1 ] ).

%% Module API

start_link() ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

start_device( Id ) ->
	supervisor:start_child( ?MODULE, {
		Id,
		{ device, start_link, [] },
		permanent,
		1000,
		worker,
		[ device ]
	} ).

start_child( Id, DeviceModule ) ->
    supervisor:start_child( ?MODULE, { 
    	Id, 
    	{ DeviceModule, start_link, [] }, 
    	permanent, 
    	1000, 
    	worker, 
    	[ DeviceModule ] 
    } ).

%% supervisor callbacks

init( _ ) ->
    { ok, { { one_for_one, 60, 3600 }, [] } }.