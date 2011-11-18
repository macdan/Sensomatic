-module( sensomatic_web_controller ).

-export( [ 
	zones/3, 
	zone/3,
	devices/3 
] ).

zones( 'GET', Req, _ ) ->
	{ ok, Html } = zones_dtl:render( [
		{ zones, lists:map( fun( { Zone, _ } ) -> Zone end, sensomatic:zones() ) }
	] ),

	Req:respond( { 200, [
		{ "Content-Type", "text/html" }
	], Html } );
zones( 'POST', Req, Args ) ->
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
	zones( 'GET', Req, Args ).

zone( 'GET', Req, [ ZoneId ] ) ->
	{ ok, Html } = zone_dtl:render( [
		{ zone_id, ZoneId }
	] ),

	Req:respond( { 200, [
		{ "Content-Type", "text/html" }
	], Html } ).

devices( 'GET', Req, _ ) ->
	Devices = lists:map( fun( { DeviceId, DevicePid } ) ->
		[
			{ pid, io_lib:format( "~p", [ DevicePid ] ) },
			{ id, DeviceId },
			{ ports, lists:map( fun( { PortId, PortPid, PortValue } ) ->
				{ _, PortId, Rw, _Type } = port:get_spec( PortPid ),
				[
					{ pid, io_lib:format( "~p", [ PortPid ] ) },
					{ id, PortId },
					{ value, PortValue },
					{ rw, Rw }
				]
			end, device:get_ports( DevicePid ) ) }
		]
	end, sensomatic:devices() ),
	
	{ ok, Html } = devices_dtl:render( [
		{ devices, Devices },
		{ port_select, helper:html_port_select( all ) }
	] ),

	Req:respond( { 200, [
		{ "Content-Type", "text/html" }
	], Html } );

devices( 'POST', Req, _ ) ->
	PostData = Req:parse_post(),
	[ DeviceId, PortId ] = string:tokens( proplists:get_value( "port", PostData ), "-" ),
	
	DevicePid = arduino_device_sup:get_device( DeviceId ),
	PortPid = device:get_port( DevicePid, PortId ),
	
	case port:get_value( PortPid ) of
		0 -> port:set_value( PortPid, 1 );
		1 -> port:set_value( PortPid, 0 )
	end,
	
	device:commit( DevicePid ),
	
	devices( 'GET', Req, [] ).
