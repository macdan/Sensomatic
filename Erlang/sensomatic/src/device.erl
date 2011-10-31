-module( device ).

-export( [ 
	start_link/1,
	get_id/1,
	get_ports/1,
	commit/1
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

-record( state, { 
	socket, 
	port_sup,
	id,
	ports = []
} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% start_link/1
%%==============================================================================
start_link( Socket ) ->
    gen_server:start_link( ?MODULE, [ Socket ], [] ).

get_id( Pid ) when is_pid( Pid ) ->
	gen_server:call( Pid, id ).

%%==============================================================================
%% get_ports/1
%%==============================================================================
get_ports( Pid ) when is_pid( Pid ) ->
	gen_server:call( Pid, ports ).

%%==============================================================================
%% commit/1
%%==============================================================================
commit( Pid ) when is_pid( Pid ) ->
	gen_server:cast( Pid, commit ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% init
%%==============================================================================
init( [ Socket ] ) ->
	util:shout( "Starting ~p gen_server...", [ ?MODULE ] ),
    gen_server:cast( self(), accept ),
    { ok, #state{ 
    	socket = Socket
    } }.

%%==============================================================================
%% handle_call
%%==============================================================================
%% Ports
%%------------------------------------------------------------------------------
handle_call( ports, _From, State ) ->
	Ports = lists:map( fun( { Id, Port } ) -> 
		{ Id, Port, port:get_value( Port ) }
	end, State#state.ports ),
	{ reply, Ports, State };
%%------------------------------------------------------------------------------
%% Id
%%------------------------------------------------------------------------------
handle_call( id, _, State ) ->
	{ reply, State#state.id, State };
%%------------------------------------------------------------------------------
%% Socket
%%------------------------------------------------------------------------------
handle_call( socket, _, State ) ->
	{ reply, State#state.socket, State };
%%------------------------------------------------------------------------------
%% Catch All
%%------------------------------------------------------------------------------
handle_call( _E, _From, State ) ->
	{ noreply, State }.

%%==============================================================================
%% handle_cast
%%==============================================================================
%% Accept
%%------------------------------------------------------------------------------
handle_cast( accept, S = #state{ socket=ListenSocket } ) ->
	{ ok, AcceptSocket } = gen_tcp:accept( ListenSocket ),
	util:shout( "Accepted socket ~p", [ AcceptSocket ] ),
	device_sup:start_child(),
	{ ok, PortSupPid } = port_sup:start_link(),
	{ ok, _EventPid } = gen_event:start_link(),
	{ noreply, S#state{ 
		socket = AcceptSocket,
		port_sup = PortSupPid
	} };
%%------------------------------------------------------------------------------
%% Invert
%%------------------------------------------------------------------------------
handle_cast( invert, State ) ->
	lists:foreach( fun( { _, Pid } ) ->
		case port:get_value( Pid ) of
			off -> port:on( Pid );
			on -> port:off( Pid )
		end
	end, State#state.ports ),
	commit( self() ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% Commit
%%------------------------------------------------------------------------------
handle_cast( commit, State ) ->
	Values = lists:map( fun( { _, Pid } ) ->
		case port:get_value( Pid ) of
			on  -> "1";
			off -> "0"
		end
	end, State#state.ports ),
	Line = "VALUES: " ++ string:join( Values, "," ) ++ "\r\n",
	gen_tcp:send( State#state.socket, Line ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% Catch All
%%------------------------------------------------------------------------------
handle_cast( _, State ) ->
	{ noreply, State }.

%%==============================================================================
%% handle_info
%%==============================================================================
%% TCP: Got DEVICE: line
%%------------------------------------------------------------------------------
handle_info( { tcp, _Port, "DEVICE:" ++ Tail }, State ) ->
	[ Id ] = string:tokens( Tail, " \r\n" ),
	{ noreply, State#state{ id = Id } };
%%------------------------------------------------------------------------------
%% TCP: Got PORTS: line
%%------------------------------------------------------------------------------
handle_info( { tcp, _Port, "PORTS:" ++ Tail }, State ) ->
	Ports = string:tokens( Tail, " ,\r\n" ),
	PortPids = lists:map( fun( Port ) ->
		{ Id2, PortSpec } = case string:tokens( Port, ":" ) of

			[ Id, "I", "A" | _ ] -> { Id, { self(), Id, { input , analog  } } };
			[ Id, "O", "A" | _ ] -> { Id, { self(), Id, { output, analog  } } };
			[ Id, "I", "D" | _ ] -> { Id, { self(), Id, { input , digital } } };
			[ Id, "O", "D" | _ ] -> { Id, { self(), Id, { output, digital } } };
			
			Else -> 
				util:shout( "Unknown port type: ~p", [ Else ] ),
				throw( { bad_port_definition, Else } )
		end,
		{ ok, Pid } = port_sup:start_child( State#state.port_sup, PortSpec ),
		{ Id2, Pid }
	end, Ports ),
	{ noreply, State#state{ ports = PortPids } };
%%------------------------------------------------------------------------------
%% TCP: Got DONE line
%%------------------------------------------------------------------------------
handle_info( { tcp, _Port, "DONE" ++ _ }, State ) ->
	device_reg:add( { State#state.id, self() } ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% TCP: Got VALUES line
%%------------------------------------------------------------------------------
handle_info( { tcp, _Port, "VALUES:" ++ Csv }, State ) ->
	Data = string:tokens( Csv, " ,\r\n" ),
	Pairs = lists:zip( State#state.ports, Data ),
	lists:foreach( fun( { { _, Port }, ValueStr } ) ->
		{ Value, _ } = string:to_integer( ValueStr ),
		port:set_value( Port, Value )
	end, Pairs ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% TCP: Got unknown line
%%------------------------------------------------------------------------------
handle_info( { tcp, _Port, Line }, State ) ->
    util:shout( "~s", [ Line ] ),
    { noreply, State };
%%------------------------------------------------------------------------------
%% TCP: Socket closed
%%------------------------------------------------------------------------------
handle_info( { tcp_closed, Socket }, State ) ->
	util:shout( "Socket ~p closed. Ending.", [ Socket ] ),
	device_reg:remove( { State#state.id, self() } ),
    { stop, normal, State };
%%------------------------------------------------------------------------------
%% Catch all
%%------------------------------------------------------------------------------
handle_info( E, S ) ->
    util:shout( "unexpected: ~p~n", [ E ] ),
    { noreply, S }.

%%==============================================================================
%% code_change/3
%%==============================================================================
code_change( _OldVsn, State, _Extra ) ->
    { ok, State }.

%%==============================================================================
%% terminate/2
%%==============================================================================
terminate( Reason, State ) ->
	util:shout( "Closing socket ~p", [ State#state.socket ] ),
    gen_tcp:close( State#state.socket ),
	util:shout( "terminate reason: ~p~n", [ Reason ] ).

