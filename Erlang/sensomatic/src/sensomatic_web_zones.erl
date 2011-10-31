-module( sensomatic_web_zones ).

-export( [ index/2 ] ).

index( 'GET', Req ) ->
	{ ok, Html } = zones_dtl:render( [
		{ zones, lists:map( fun( { Zone, _ } ) -> Zone end, sensomatic:zones() ) }
	] ),

	Req:respond( { 200, [
		{ "Content-Type", "text/html" }
	], Html } );
index( 'POST', Req ) ->
	PostData = Req:parse_post(),
	case proplists:get_value( "submit", PostData ) of
		"Add Zone" ->
			ZoneId = proplists:get_value( "zone_id", PostData, undefined ),
			if
				ZoneId =:= undefined ->
					throw( "No zone_id" );
				
				true -> ok
			end,
			sensomatic:add_zone( ZoneId );
		"Remove Zone" ->
			ZoneId = proplists:get_value( "zone_id", PostData, undefined ),
			if
				ZoneId =:= undefined ->
					throw( "No zone_id" );
				
				true -> ok
			end,
			sensomatic:remove_zone( ZoneId )
	end,
	index( 'GET', Req ).

