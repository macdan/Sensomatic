-module( client_sup ).
-behaviour( supervisor ).

-export( [ start_link/1, start_child/0 ] ).
-export( [ init/1 ] ).

%% Module API

start_link( ListenSocket ) ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [ 
    	ListenSocket
    ] ).

start_child() ->
    supervisor:start_child( ?MODULE, [] ).

%% supervisor callbacks

init( [ ListenSocket ] ) ->
    spawn( fun() -> start_child() end ),
    
    { ok, { { simple_one_for_one, 60, 3600 }, [
		{ client, 
			{ client, start_link, [ ListenSocket ] },
			temporary, 1000, worker, [ client ]
		}
	] } }.