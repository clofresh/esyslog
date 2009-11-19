% Implementation of syslog server protocol (RFC3164)

-module(esyslog).
-export([listen/1, parse_message/1, test/0]).

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
                    io:format("~p~n", [parse_message(binary_to_list(Data))]),
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

parse_message(Message, []) ->
    % Priority
    io:format("~p (~p)~n", [Message, []]),
    
    "<" ++ PriorityPlusOtherStuff = Message,
    Priority = lists:takewhile(
        fun($>) -> false; 
           (_) -> true
        end,
        PriorityPlusOtherStuff
    ),
    Tail = string:sub_string(PriorityPlusOtherStuff, length("<" ++ Priority ++ ">")),
    parse_message(Tail, [Priority]);
    
parse_message(Message, Parts) when length(Parts) == 1 ->
    % Timestamp
    io:format("~p (~p)~n", [Message, Parts]),
    
    Timestamp = string:sub_string(Message, 1, 15),
    TimestampTuple = list_to_tuple(string:tokens(Timestamp, " ")),
    parse_message(string:sub_string(Message, 16), [TimestampTuple] ++ Parts);
    
parse_message(Message, Parts) when length(Parts) == 2 ->
    % Host
    io:format("~p (~p)~n", [Message, Parts]),

    " " ++ HostPlusOtherStuff = Message,
    case string:chr(HostPlusOtherStuff, $ ) of
        0 -> 
            bad_message;
        I -> 
            Host = string:sub_string(HostPlusOtherStuff, 1, I - 1),
            parse_message(string:sub_string(HostPlusOtherStuff, I + 1), [Host] ++ Parts)
    end;
    
    
parse_message(Message, Parts) when length(Parts) == 3 ->
    % Tag
    io:format("~p (~p)~n", [Message, Parts]),
    
    case string:chr(Message, $:) of
        0 -> 
            bad_message;
        I -> 
            Tag = string:sub_string(Message, 1, I - 1),
            parse_message(string:sub_string(Message, I + 1), [Tag] ++ Parts)
    end;
    
parse_message(Message, Parts) when length(Parts) == 4 ->
    % Body
    io:format("~p (~p)~n", [Message, Parts]),
    
    " " ++ Body = Message,
    list_to_tuple(lists:reverse([Body] ++ Parts)). 
    
parse_message(Message) ->
    try 
        parse_message(Message, [])
    catch error:{badmatch, _} ->
        bad_message
    end.

test() ->
    {"147", 
     {"Nov", "18", "19:17:55"},
     "myhost",
     "mytag[909]",
     "yo what's really real"} = parse_message("<147>Nov 18 19:17:55 myhost mytag[909]: yo what's really real"),
    bad_message = parse_message("asdf"),
    true.
