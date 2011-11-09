-module( client ).

-export( [ 
	start_link/1,
	send/2,
	
	get_id/1,
	get_ports/1,
	commit/1,
	
	parse_ports/1,
	parse_port/1
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
	device,
	resumed = false,
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

%%==============================================================================
%% commit/2
%%==============================================================================
send( Pid, Data ) ->
	gen_server:call( Pid, { send, Data } ).

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
%%------------------------------------------------------------------------------
handle_call( { send, Data }, _, State ) ->
	{ reply,
		gen_tcp:send( State#state.socket, Data ),
	State };
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
	client_sup:start_child(),
	{ noreply, S#state{ 
		socket = AcceptSocket
	} };
%%------------------------------------------------------------------------------
%% Commit
%%------------------------------------------------------------------------------
handle_cast( commit, State ) ->
	Values = lists:map( fun( { _, Pid } ) ->
		case port:get_value( Pid ) of
			1 -> "1";
			0 -> "0"
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
	
	NewState = case device_sup:start_or_resume_device( Id ) of
		
		{ ok, DevicePid } -> 
			device:add_handler( DevicePid, client_handler_device, [ self() ] ),
			State#state{ 
				id = Id, 
				device = DevicePid 
			};
			
		{ resumed, DevicePid } -> 
			device:add_handler( DevicePid, client_handler_device, [ self() ] ),
			device:commit( DevicePid ),
			Ports = lists:map( fun( { PortId, PortPid, _ } ) ->
				{ PortId, PortPid }
			end, device:get_ports( DevicePid ) ),
			State#state{ 
				id = Id, 
				device = DevicePid, 
				resumed = true,
				ports = lists:reverse( Ports )
			}
	end,
	
	
	
	{ noreply, NewState };
%%------------------------------------------------------------------------------
%% TCP: Got PORTS: line
%%------------------------------------------------------------------------------
handle_info( { tcp, _, "PORTS:" ++ _ }, State = #state{ resumed = true } ) -> 
	% Safely ignore ports line after resuming
	{ noreply, State };
handle_info( { tcp, _, "PORTS:" ++ Tail }, State ) ->
	Ports = lists:map( fun( ParsedPort ) ->
		PortSpec = case ParsedPort of
			{ Id, input, digital } -> { State#state.device, Id, ro, { digital, false } };
			{ Id, output, digital } -> { State#state.device, Id, rw, { digital, false } };
			{ Id, input, analog } -> { State#state.device, Id, ro, { scale, { 0, 1024 }, 0 } };
			{ Id, output, analog } -> { State#state.device, Id, rw, { scale, { 0, 1024 }, 0 } }
		end,
		{ ok, Pid } = device:add_port( State#state.device, PortSpec ),
		{ Id, Pid }
	end, parse_ports( Tail ) ),
	{ noreply, State#state{ ports = Ports } };
%%------------------------------------------------------------------------------
%% TCP: Got DONE line
%%------------------------------------------------------------------------------
handle_info( { tcp, _Port, "DONE" ++ _ }, State ) ->
	{ noreply, State };
%%------------------------------------------------------------------------------
%% TCP: Got VALUES line
%%------------------------------------------------------------------------------
handle_info( { tcp, _Port, "VALUES:" ++ Csv }, State ) ->
	Data = lists:map( fun parse_value/1, string:tokens( Csv, " ,\r\n" ) ),
	lists:foreach( fun( { { _, Port }, Value } ) ->
		port:set_value( Port, Value )
	end, lists:zip( State#state.ports, Data ) ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% TCP: Got unknown line
%%------------------------------------------------------------------------------
handle_info( { tcp, _Port, Line }, State ) ->
    util:shout( "Unknown Line: ~p", [ Line ] ),
    { noreply, State };
%%------------------------------------------------------------------------------
%% TCP: Socket closed
%%------------------------------------------------------------------------------
handle_info( { tcp_closed, Socket }, State ) ->
	util:shout( "Socket ~p closed. Ending.", [ Socket ] ),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_ports( "PORTS:" ++ Tail ) ->
	parse_ports( Tail );
parse_ports( PortsString ) ->
	PortStrings = string:tokens( PortsString, " ,\r\n" ),
	lists:map( fun parse_port/1, PortStrings ).

parse_port( PortString ) ->
	case string:tokens( PortString, ":" ) of
			[ Id, "I", "A" | _ ] -> { Id, input, analog };
			[ Id, "O", "A" | _ ] -> { Id, output, analog };
			[ Id, "I", "D" | _ ] -> { Id, input, digital };
			[ Id, "O", "D" | _ ] -> { Id, output, digital }
	end.

parse_value( ValueString ) ->
	{ Value, _ } = string:to_integer( ValueString ),
	Value.

