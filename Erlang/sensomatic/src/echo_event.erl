-module( echo_event ).
-behaviour( gen_event ).

-export( [ 
	init/1, 
	handle_event/2, 
	handle_call/2, 
	handle_info/2, 
	code_change/3,
	terminate/2
] ).
 
init( [] ) ->
	{ ok, 0 }.
 
handle_event( { value_changed, Port, NewVal }, Val ) ->
	if
		NewVal > Val ->
			util:shout( "Port: ~p Increase: ~p (~p)", [ Port, NewVal - Val, NewVal ] );
		
		NewVal < Val ->
			util:shout( "Port: ~p Decrease: ~p (~p)", [ Port, Val - NewVal, NewVal ] )
	end,
	{ ok, NewVal }.
 
handle_call( _, State ) ->
	{ ok, ok, State }.
 
handle_info( _, State ) ->
	{ ok, State }.
 
code_change( _OldVsn, State, _Extra ) ->
	{ ok, State }.
 
terminate( _Reason, _State ) ->
	ok.