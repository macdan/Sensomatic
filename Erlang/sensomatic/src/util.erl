-module( util ).

-export( [ shout/1, shout/2 ] ).

shout( Format ) -> shout( Format, [] ).
shout( Format, Args ) ->
	io:format( "~p " ++ Format ++ "~n", [ self() | Args ] ).