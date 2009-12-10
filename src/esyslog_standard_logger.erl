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

handle_event({log, Msg = {Priority, Timestamp, Host, Tag, Body}}, Config) ->
    io:format("Message: ~p~n", [esyslog_message:format(human, Msg)]),
    Targets = esyslog_config:get_targets(Priority, Config),
    io:format("~p~n", [Targets]),
    lists:foreach(
        fun(Target) ->
            log(Msg, Target)
        end,
    Targets),
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

log(Msg, {local, Filename}) ->
    io:format("Writing locally to ~p~n", [Filename]),
    {ok, IoDevice} = file:open(Filename, [append]),
    try
        io:put_chars(IoDevice, string:concat(esyslog_message:format(human, Msg), "\n"))
    after
        file:close(IoDevice)
    end,
    ok;
    
log(Msg, {remote, Remote}) ->
    {Host, Port} = case Remote of
        {H, P} -> {H, P};
        H      -> {H, application:get_env(esyslog, port)}
    end,

    io:format("Writing remotely to ~p port ~p~n", [Host, Port]),

    case gen_udp:open(Port) of
        {ok, Socket} ->
            case gen_udp:send(Socket, Host, Port, esyslog_message:format(rfc3164, Msg)) of
                ok -> 
                    ok;
                {error, Reason} -> 
                    io:format("Failed to send to ~s:~B: ~p~n", [Host, Port, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to open udp socket to ~s:~B: ~p~n", [Host, Port, Reason]),
            {error, Reason}
    end.

    
    
    