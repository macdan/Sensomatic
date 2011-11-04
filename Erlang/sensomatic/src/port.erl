-module( port ).

-export( [ 
	start_link/1,
	start_link/3, 
	set_value/2, 
	get_value/1,
	on/1,
	off/1,
	validate/2
] ).

-behaviour( gen_server ).
-export( [ 
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2, 
	code_change/3, 
	terminate/2 
] ).

-record( state, { device, event, id, rw, ad, type, value } ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link( DevicePid, Id, { Rw, _ } ) -> 
	gen_server:start_link( ?MODULE, [ { DevicePid, Id, Rw, { analog, { 0, 2041 } } } ], [] ).

start_link( PortSpec ) ->
	gen_server:start_link( ?MODULE, [ PortSpec ], [] ).

set_value( Pid, Value ) -> 
	gen_server:call( Pid, { set_value, Value } ).
	
get_value( Pid ) -> 
	gen_server:call( Pid, get_value ).

%%==============================================================================
%% on/1
%%==============================================================================
on( Pid ) when is_pid( Pid ) ->
	set_value( Pid, 1 );
on( Pids ) when is_list( Pids ) ->
	lists:foreach( fun( Pid ) ->
		on( Pid )
	end, Pids ).

%%==============================================================================
%% off/1
%%==============================================================================
off( Pid ) when is_pid( Pid ) ->
	set_value( Pid, 0 );
off( Pids ) when is_list( Pids ) ->
	lists:foreach( fun( Pid ) ->
		off( Pid )
	end, Pids ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% init/1
%%==============================================================================
%init( [ DevicePid, Id, Type = { Rw, Ad } ] ) ->
init( [ { DevicePid, Id, Rw, Type } ] ) ->
	util:shout( "Starting ~p port ~p on device ~p...", [ Id, Type, DevicePid ] ),
	{ ok, EventPid } = gen_event:start_link(),
	{ ok, #state{ 
		device = DevicePid,
		id = Id,
		rw = Rw,
		type = Type,
		event = EventPid
	} }.

%%==============================================================================
%% handle_cast/2
%%==============================================================================
%% Commit
%%------------------------------------------------------------------------------
handle_cast( commit, State ) ->
	gen_server:cast( State#state.device, commit ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% Catch All
%%------------------------------------------------------------------------------
handle_cast( Cast, State ) ->
	util:shout( "unexpected cast: ~p", [ Cast ] ),
    { noreply, State }.

%%==============================================================================
%% handle_info/2
%%==============================================================================
handle_info( Msg, State ) ->
    util:shout( "unexpected: ~p", [ Msg ] ),
    { noreply, State }.

%%==============================================================================
%% handle_call/2
%%==============================================================================
%% Get Value
%%------------------------------------------------------------------------------
handle_call( get_value, _From, State ) ->
	{ reply, State#state.value, State };
%%------------------------------------------------------------------------------
%% Set Value (Read-Write)
%%------------------------------------------------------------------------------
handle_call( { set_value, Value }, _From, State ) ->
	if
		Value =/= State#state.value ->
			gen_event:notify( State#state.event, { value_changed, self(), Value } );
		
		Value =:= State#state.value ->
			ok
	
	end,
	NewState = State#state{ value = Value },
	{ reply, ok, NewState };
%%------------------------------------------------------------------------------
%% Catch All
%%------------------------------------------------------------------------------
handle_call( Call, _From, State ) ->
	util:shout( "unexpected call: ~p", [ Call ] ),
	{ noreply, State }.

%%==============================================================================
%% code_change/3
%%==============================================================================
code_change( _OldVsn, State, _Extra ) ->
	{ ok, State }.

%%==============================================================================
%% terminate/2
%%==============================================================================
terminate( normal, _State ) ->
	ok;
terminate( Reason, _State ) ->
	util:shout( "terminate reason: ~p~n", [ Reason ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @spec validate( port_type(), Value ) -> ok | error
validate( { scale, Scale, _ }, { scale, Scale, Value } ) ->
	Value;
validate( { scale, { Min, Max }, _ }, Value ) when 
	is_integer( Value ), Value >= Min, Value =< Max;
	is_float( Value ), Value >= Min, Value =< Max -> 
	Value;
validate( { scale, { _, Max }, _ }, Value ) when Value == true ->
	Max;
validate( { scale, { Min, _ }, _ }, Value ) when Value == false ->
	Min;
validate( { digital, _ }, Value ) when Value == true; Value == false ->
	Value;
validate( { digital, _ }, Value ) when is_integer( Value ); is_float( Value ) ->
	if
		Value =< 0 -> false;
		Value > 0  -> true
	end;
	
validate( _, _ ) ->
	error.