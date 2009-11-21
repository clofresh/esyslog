% Implementation of syslog server protocol (RFC3164)

-module(esyslog).
-export([listen/1]).

-define(UDP_OPTIONS, [binary, {active, false}]).

listen(Port) -> 
    listen_loop(Port).

listen_loop(Port) -> 
    io:format("Opening socket at port ~p~n", [Port]),
    
    try gen_udp:open(Port, ?UDP_OPTIONS) of
        {ok, Socket} -> 
            io:format("Receiving data from socket ~p~n", [Socket]),
            case gen_udp:recv(Socket, 0) of
                {ok, {IP, _, Data}} ->
                    io:format("~p~n", [esyslog_message:parse(binary_to_list(Data))]),
                    listen_loop(Socket);

                Other ->
                    io:format("Nope: ~p~n", [Other])
            end,
    
            gen_udp:close(Socket),
        
            listen_loop(Port)
    catch
        error:Error ->
            io:format("~p~n", [Error]),
            fail
    end.

