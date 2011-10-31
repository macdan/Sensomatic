-module( sensomatic_web_device ).

-export( [ list/2 ] ).

list( 'GET', Req ) ->
	Devices = lists:map( fun( { Id, _Pid } ) ->
		[ 
			{ "id", Id },
			{ "ports", [ one, two, three ] }
		]
	end, sensomatic:devices() ),
	
	util:shout( "Devices: ~p~n", [ Devices ] ),
	
	{ ok, Html } = device_list_dtl:render( [
		{ "devices", Devices }
	] ),
	
	Req:respond( { 200, [
		{ "Content-Type", "text/html" }
	], Html } ).