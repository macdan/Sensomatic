-module( sensomatic_web_dispatcher ).
-export( [ dispatch/2 ] ).

dispatch( _Request, [] ) ->
	none;
dispatch( Request, [ { RegExp, { Module, Function } } | Routes ] ) ->
	"/" ++ Path = Request:get( path ),
	Method = Request:get( method ),

	Match = re:run( Path, RegExp, [ 
		global, 
		{ capture, all_but_first, list } 
	] ),

	case Match of
		%% Found route
		{ match, [ _MatchList ] } -> 
			Module:Function( Method, Request );
		
		%% Didn't match, keep going
		_ -> 
			dispatch( Request, Routes )
	
	end.
