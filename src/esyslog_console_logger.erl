-module(esyslog_console_logger).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

init(_Args) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({log, {Priority, Timestamp, Host, Tag, Body}}, State) ->
    io:format("Logging: ~p ~p ~p ~p ~p~n", [Priority, Timestamp, Host, Tag, Body]),
    {ok, State};
    
handle_event(Event, State) ->
    io:format("Catchall: ~p, ~p~n", [Event, State]),
    {ok, State}.

handle_call(Call, State) ->
    io:format("Catchall: ~p, ~p~n", [Call, State]),
    {ok, State}.

handle_info(Info, State) ->
    io:format("Catchall: ~p, ~p~n", [Info, State]),
    {ok, State}.

