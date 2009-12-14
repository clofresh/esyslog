-module(esyslog_message).
-include_lib("eunit/include/eunit.hrl").
-export([parse/1, decode_priority/1, format/1]).


%% @type datetime() = {{Year, Month, Day}, {Hour, Minute, Second}}
%%      Year   = integer(),
%%      Month  = integer(),
%%      Day    = integer(),
%%      Hour   = integer(),
%%      Minute = integer(),
%%      Second = integer().

%% @type priority() = integer().
%% An integer encoding the facility and priority of a syslog message.
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>

%% @type facility() = integer().
%% An integer between 0 and 23 inclusive that indicates the syslog facility. 
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>

%% @type severity() = integer().
%% An integer between 0 and 7 inclusive that indicates the syslog severity. 
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>

%% @type msg() = {Priority, DateTime, Host, Tag, Body}
%%      Priority = priority(),
%%      DateTime = datetime(),
%%      Host     = string(),
%%      Tag      = string(),
%%      Body     = string().

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
    {MonthStr, DayStr, TimeStr} = list_to_tuple(string:tokens(Timestamp, " ")),
    {{Year, _, _}, _} = calendar:now_to_local_time(now()),
    Month = case MonthStr of
                "Jan" -> 1;
                "Feb" -> 2;
		"Mar" -> 3;
		"Apr" -> 4;
		"May" -> 5;
		"Jun" -> 6;
		"Jul" -> 7;
		"Aug" -> 8;
		"Sep" -> 9;
		"Oct" -> 10;
		"Nov" -> 11;
		"Dec" -> 12
    end,
    Day = list_to_integer(DayStr),
    Hour = list_to_integer(string:sub_string(TimeStr, 1, 2)),
    Minute = list_to_integer(string:sub_string(TimeStr, 4, 5)),
    Second = list_to_integer(string:sub_string(TimeStr, 7, 8)),
    parse(string:sub_string(Message, 16), [{{Year, Month, Day}, {Hour, Minute, Second}}] ++ Parts);
    
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

%% @spec parse(Message::string()) -> msg() | bad_message
%% @doc Parses a string into a syslog message tuple
parse(Message) ->
    io:format("Parsing message: ~p~n", [Message]),
    try 
        parse(Message, [])
    catch error:{badmatch, _} ->
        bad_message
    end.

%% @spec decode_priority(Priority::priority()) -> {facility(), severity()}
%% @doc Decodes a priority value into facility and severity
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
    
%% @spec format(Msg::msg()) -> string()
%% @doc Pretty-print a syslog message tuple
format({Priority, Timestamp, Host, Tag, Body}) ->
    string:join([httpd_util:rfc1123_date(Timestamp), integer_to_list(Priority), Host, Tag, Body], " ").

parse_test() ->
    {{Year, _, _}, _} = calendar:now_to_local_time(now()),
    {147, 
     {{Year, 11, 18}, {19, 17, 55}},
     "myhost",
     "mytag[909]",
     "yo what's really real"} = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo what's really real"),
    bad_message = parse("asdf"),
    true.
