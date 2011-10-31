%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc sensomatic_web.

-module(sensomatic_web).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the sensomatic_web server.
start() ->
    sensomatic_web_deps:ensure(),
    ensure_started(crypto),
    application:start(sensomatic_web).


%% @spec stop() -> ok
%% @doc Stop the sensomatic_web server.
stop() ->
    application:stop(sensomatic_web).
