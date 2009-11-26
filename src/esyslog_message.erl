-module(esyslog_message).
-include_lib("eunit/include/eunit.hrl").
-export([parse/1]).

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
    parse(Tail, [Priority]);
    
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

parse_test() ->
    {"147", 
     {"Nov", "18", "19:17:55"},
     "myhost",
     "mytag[909]",
     "yo what's really real"} = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo what's really real"),
    bad_message = parse("asdf"),
    true.
