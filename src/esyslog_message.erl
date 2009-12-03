-module(esyslog_message).
-include_lib("eunit/include/eunit.hrl").
-export([parse/1, decode_priority/1]).

parse(Message, []) ->
    % Priority
    %io:format("~p (~p)~n", [Message, []]),
    
    "<" ++ PriorityPlusOtherStuff = Message,
    Priority = lists:takewhile(
        fun($>) -> false; 
           (_) -> true
        end,
        PriorityPlusOtherStuff
    ),
    Tail = string:sub_string(PriorityPlusOtherStuff, length("<" ++ Priority ++ ">")),
    parse(Tail, [list_to_integer(Priority)]);
    
parse(Message, Parts) when length(Parts) == 1 ->
    % Timestamp
    %io:format("~p (~p)~n", [Message, Parts]),
    
    Timestamp = string:sub_string(Message, 1, 15),
    TimestampTuple = list_to_tuple(string:tokens(Timestamp, " ")),
    parse(string:sub_string(Message, 16), [TimestampTuple] ++ Parts);
    
parse(Message, Parts) when length(Parts) == 2 ->
    % Host
    %io:format("~p (~p)~n", [Message, Parts]),

    " " ++ HostPlusOtherStuff = Message,
    case string:chr(HostPlusOtherStuff, $ ) of
        0 -> 
            bad_message;
        I -> 
            Host = string:sub_string(HostPlusOtherStuff, 1, I - 1),
            parse(string:sub_string(HostPlusOtherStuff, I + 1), [Host] ++ Parts)
    end;
    
    
parse(Message, Parts) when length(Parts) == 3 ->
    % Tag
    %io:format("~p (~p)~n", [Message, Parts]),
    
    case string:chr(Message, $:) of
        0 -> 
            bad_message;
        I -> 
            Tag = string:sub_string(Message, 1, I - 1),
            parse(string:sub_string(Message, I + 1), [Tag] ++ Parts)
    end;
    
parse(Message, Parts) when length(Parts) == 4 ->
    % Body
    %io:format("~p (~p)~n", [Message, Parts]),
    
    " " ++ Body = Message,
    list_to_tuple(lists:reverse([string:strip(Body, right, $\n)] ++ Parts)). 
    
parse(Message) ->
    io:format("Parsing message: ~p~n", [Message]),
    try 
        parse(Message, [])
    catch error:{badmatch, _} ->
        bad_message
    end.

decode_priority(Priority) ->
    {decode_facility(Priority div 8), 
     decode_severity(Priority rem 8)}.

decode_facility(Facility) ->
    case Facility of
        0 -> kern;
        1 -> user;
        2 -> mail;
        3 -> system;
        4 -> auth;
        5 -> internal;
        6 -> lpr;
        7 -> nns;
        8 -> uucp;
        9 -> clock;
        10 -> authpriv;
        11 -> ftp;
        12 -> ntp;
        13 -> audit;
        14 -> alert;
        15 -> clock2; % ?
        16 -> local0;
        17 -> local1;
        18 -> local2;
        19 -> local3;
        20 -> local4;
        21 -> local5;
        22 -> local6;
        23 -> local7;
        _ -> undefined
    end.

decode_severity(Severity) ->
    case Severity of
        0 -> emerg;
        1 -> alert;
        2 -> crit;
        3 -> err;
        4 -> warn;
        5 -> notice;
        6 -> info;
        7 -> debug;
        _ -> undefined
    end.
    
parse_test() ->
    {147, 
     {"Nov", "18", "19:17:55"},
     "myhost",
     "mytag[909]",
     "yo what's really real"} = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo what's really real"),
    bad_message = parse("asdf"),
    true.
