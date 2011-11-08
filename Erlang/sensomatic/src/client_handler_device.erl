-module( client_handler_device ).
-behaviour( gen_event ).

-export( [ 
	init/1, 
	handle_event/2, 
	handle_call/2, 
	handle_info/2, 
	code_change/3,
	terminate/2
] ).

-record( state, { client } ).

init( [ Client ] ) ->
	{ ok, #state{ 
		client = Client 
	} }.
 
handle_event( { commit, Device }, State ) ->
	Values = lists:map( fun( { _, _, Value } ) ->
		case Value of
			0 -> "0";
			1 -> "1";
			{ digital, low } -> "0";
			{ digital, high } -> "1"
		end
	end, device:get_ports( Device ) ),
	Line = "VALUES: " ++ string:join( Values, "," ) ++ "\r\n",
	client:send( State#state.client, Line ),
	{ ok, State }.
 
handle_call( _, State ) ->
	{ ok, ok, State }.
 
handle_info( _, State ) ->
	{ ok, State }.
 
code_change( _OldVsn, State, _Extra ) ->
	{ ok, State }.
 
terminate( _Reason, _State ) ->
	ok.