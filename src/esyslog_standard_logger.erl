-module(esyslog_standard_logger).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

init(Config) ->
    {ok, Config}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({log, {Priority, Timestamp, Host, Tag, Body}}, Config) ->
    Targets = esyslog_config:get_targets(Priority, Config),
    io:format("~p~n", [Targets]),
    {ok, Config};
    
handle_event(Event, State) ->
    io:format("Catchall: ~p, ~p~n", [Event, State]),
    {ok, State}.

handle_call(Call, State) ->
    io:format("Catchall: ~p, ~p~n", [Call, State]),
    {ok, State}.

handle_info(Info, State) ->
    io:format("Catchall: ~p, ~p~n", [Info, State]),
    {ok, State}.

