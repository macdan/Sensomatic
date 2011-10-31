-module( helper ).
-export( [ 
	html_port_select/1,
	
	html_select/2,
	html_select_option/2,
	
	html_input_submit/1,
	html_input_submit/2
] ).

%% @spec html_port_select( all ) -> iolist()
%% @doc Create an HTML <select/> element of all ports on all devices.
html_port_select( all ) -> 
	Options = lists:map( fun( { Id, Pid } ) -> 
		PortOptions = lists:map( fun( { PortId, _, _ } ) ->
			html_select_option( PortId, PortId )
		end, device:get_ports( Pid ) ),
		"<optgroup label=\"" ++ Id ++ "\">" ++ PortOptions ++ "</optgroup>"
	end, sensomatic:devices() ),
	html_select( "port_id", Options );

%% @spec html_port_select( pid() ) -> iolist()
%% @doc Create an HTML <select/> element of all ports on the device specified.
html_port_select( Device ) when is_pid( Device ) -> 
	html_select( "port_id", lists:map( fun( { Id, _Pid, _Value } ) ->
		html_select_option( Id, Id )
	end, device:get_ports( Device ) ) ).

html_select( Name, Options ) -> 
	"<select name=\"" ++ Name ++ "\">" ++ Options ++ "</select>".

html_select_option( Value, Label ) -> 
	"<option value=\"" ++ Value ++ "\">" ++ Label ++ "</option>".

html_input_submit( Value ) -> 
	html_input_submit( Value, "submit" ).

html_input_submit( Value, Name ) -> 
	"<input type=\"submit\" name=\"" ++ Name ++ "\" value=\"" ++ Value ++ "\"/>".

