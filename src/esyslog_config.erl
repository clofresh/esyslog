-module(esyslog_config).
-export([get_targets/2, parse/1, test/0]).

get_targets(Priority, Config) ->
    {Facility, Severity} = esyslog_message:decode_priority(Priority),
    
    lists:foldl(
        fun(Elem, Result) -> 
            case Elem of
                {{Facility, Severity}, T} ->
                    lists:append(Result, [T]);
                {{Facility, wildcard}, T} ->
                    lists:append(Result, [T]);
                {{wildcard, Severity}, T} ->
                    lists:append(Result, [T]);
                _ ->
                    Result
            end
        end,
        [],
        Config
    ).
    
parse(Filename) ->
    {ok, Data} = file:read_file(Filename),
    case esyslog_config_lexer:string(erlang:binary_to_list(Data)) of
        {ok, Tokens, _} -> 
            case esyslog_config_parser:parse(Tokens) of
                {ok, ParseTree} -> 
                    {ok, clean(ParseTree)};
                ParseError ->
                    ParseError
            end;

        LexError -> 
            LexError
    end.

clean(ParseTree) ->
    lists:map(fun(Action) ->
            Items = lists:map(fun(Item) ->
                    case Item of
                        {facility, {word, _, I}}  -> 
                            {facility, I};
                        {facility, {wildcard, _}} -> 
                            {facility, wildcard};
                        {severity, {word, _, I}}  -> 
                            {severity, I};
                        {severity, {wildcard, _}} -> 
                            {severity, wildcard};
                        {target,   {local, {filepath, _, I}}} -> 
                            {target, {local, I}};
                        {target,   {remote, {host, _, I}}} -> 
                            {target, {remote, I}};
                        {target,   {remote, {{host, _, I}, {port, _, I2}}}} -> 
                            {target, {remote, {I, I2}}}
                    end
                end,
                Action
            ),
            {{proplists:get_value(facility, Items), 
              proplists:get_value(severity, Items)},
             proplists:get_value(target, Items)}
        end,
        ParseTree
    ).

test() ->
    test1(),
    test2(),
    test3(),
    test4(),
    test5().

test1() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("local0.info    /blah/asdf"),
    io:format("Tokens:~n~p~n", [Tokens]),
    {ok, ParseTree} = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

test2() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("local0.*    @localhost:7777"),
    io:format("Tokens:~n~p~n", [Tokens]),
    {ok, ParseTree} = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

test3() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("*.*    @127.0.0.1"),
    io:format("Tokens:~n~p~n", [Tokens]),
    {ok, ParseTree} = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

test4() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("local0.*    @somehost.com:514"),
    io:format("Tokens:~n~p~n", [Tokens]),
    {ok, ParseTree} = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

test5() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("*.*    @192.168.1.1:7777"),
    io:format("Tokens:~n~p~n", [Tokens]),
    {ok, ParseTree} = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

