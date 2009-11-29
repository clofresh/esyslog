-module(esyslog_disk_logger).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

init(_Args) ->
    file:open("/tmp/esyslog.log", [append]).

terminate(_Reason, IoDevice) ->
    file:close(IoDevice).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({log, Msg = {Priority, Timestamp, Host, Tag, Body}}, IoDevice) ->
    io:put_chars(IoDevice, string:concat(esyslog_message:format(Msg), "\n")), 
    {ok, IoDevice};
    
handle_event(Event, State) ->
    io:format("Catchall: ~p, ~p~n", [Event, State]),
    {ok, State}.

handle_call(Call, State) ->
    io:format("Catchall: ~p, ~p~n", [Call, State]),
    {ok, State}.

handle_info(Info, State) ->
    io:format("Catchall: ~p, ~p~n", [Info, State]),
    {ok, State}.

