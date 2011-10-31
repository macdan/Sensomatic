%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for sensomatic_web.

-module(sensomatic_web_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
    	DispatchResult = sensomatic_web_dispatcher:dispatch( Req, [
			{ "^zones/?$", { sensomatic_web_controller, zones } },
			{ "^zones/(.+?)/?$", { sensomatic_web_controller, zone } },
			{ "^devices/?$", { sensomatic_web_controller, devices } }
		] ),
		case DispatchResult of
			none -> serve_file( Req, DocRoot );
			_ -> ok
		end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond( { 500, [ { "Content-Type", "text/plain" } ],
                         io_lib:format( "request failed, sorry~n~n~p", [ Report ] ) } )
    end.

%% Internal API

serve_file( Req, DocRoot ) ->
	"/" ++ Path = Req:get( path ),
	case Req:get( method ) of
	
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			Req:serve_file( Path, DocRoot );
		
		_ -> Req:respond( { 405, [ { "Content-Type", "text/plain" } ], "Method not allowed" } )
	end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
