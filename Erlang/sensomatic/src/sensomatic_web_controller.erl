-module( sensomatic_web_controller ).

-export( [ zones/2, devices/2 ] ).

zones( 'GET', Req ) ->
	{ ok, Html } = zones_dtl:render( [
		{ zones, lists:map( fun( { Zone, _ } ) -> Zone end, sensomatic:zones() ) }
	] ),

	Req:respond( { 200, [
		{ "Content-Type", "text/html" }
	], Html } );
zones( 'POST', Req ) ->
	PostData = Req:parse_post(),
	case proplists:get_value( "submit", PostData ) of
		"Add Zone" ->
			ZoneId = proplists:get_value( "zone_id", PostData, undefined ),
			if
				ZoneId =:= undefined -> throw( "No zone_id" );
				ZoneId =/= undefined -> ok
			end,
			sensomatic:add_zone( ZoneId );
		"Remove Zone" ->
			ZoneId = proplists:get_value( "zone_id", PostData, undefined ),
			if
				ZoneId =:= undefined -> throw( "No zone_id" );
				ZoneId =/= undefined -> ok
			end,
			sensomatic:remove_zone( ZoneId )
	end,
	zones( 'GET', Req ).

devices( 'GET', Req ) ->
	Devices = lists:map( fun( { DeviceId, DevicePid } ) ->
		[
			{ pid, io_lib:format( "~p", [ DevicePid ] ) },
			{ id, DeviceId },
			{ ports, lists:map( fun( { PortId, PortPid, PortValue } ) ->
				[
					{ pid, io_lib:format( "~p", [ PortPid ] ) },
					{ id, PortId },
					{ value, PortValue }
				]
			end, device:get_ports( DevicePid ) ) }
		]
	end, sensomatic:devices() ),
	
	{ ok, Html } = device_list_dtl:render( [
		{ devices, Devices },
		{ port_select, helper:html_port_select( all ) }
	] ),

	Req:respond( { 200, [
		{ "Content-Type", "text/html" }
	], Html } ).
