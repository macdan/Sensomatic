-module( arduino_listener ).

-export( [ start_link/2 ] ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2 ] ).

-record( state, { 
	socket,
	supervisor
} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% start_link/1
%%==============================================================================
start_link( Port, Options ) ->
    gen_server:start_link( { local, ?MODULE }, ?MODULE, [ Port, Options ], [] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% init/1
%%==============================================================================
init( [ Port, Options ] ) ->
	util:shout( "Starting listener..." ),
	
	case gen_tcp:listen( Port, Options ) of
		
		{ error, Reason } -> 
			util:shout( "Failed to listen on port ~p: ~s", [ Port, Reason ] ),
			{ stop, Reason };
		
		{ ok, ListenSocket } ->
			util:shout( "Listening on port ~p with socket ~p", 
				[ Port, ListenSocket ] ),
				
			{ ok, Supervisor } = arduino_client_sup:start_link( ListenSocket ),
			{ ok, #state{ 
				socket     = ListenSocket,
				supervisor = Supervisor
			} }
	end.

%%==============================================================================
%% terminate/2
%%==============================================================================
terminate( Reason, State ) ->
	util:shout( "Closing listen socket ~p", [ State#state.socket ] ),
	gen_tcp:close( State#state.socket ),
    util:shout( "terminate reason: ~p~n", [ Reason ] ),
    ok.

%%==============================================================================
%% handle_call/3
%%==============================================================================
handle_call( Call, From, State ) -> 
	util:shout( "Unknown call: ~p from ~p", [ Call, From ] ),
	{ noreply, State }.

%%==============================================================================
%% handle_cast/2
%%==============================================================================
handle_cast( Cast, State ) -> 
	util:shout( "Unknown cast: ~p", [ Cast ] ),
	{ noreply, State }.

%%==============================================================================
%% handle_info/2
%%==============================================================================
handle_info( Msg, State ) -> 
	util:shout( "Unknown Msg: ~p", [ Msg ] ),
	{ noreply, State }.

%%==============================================================================
%% code_change/3
%%==============================================================================
code_change( _, State, _ ) -> 
	{ ok, State }.