-module(test_esyslog_config).
-export([test/0]).

test() ->
    leex:file(esyslog_config_lexer),
    yecc:file(esyslog_config_parser),
    compile:file(esyslog_config_lexer),
    compile:file(esyslog_config_parser),
    test1(),
    test2(),
    test3(),
    test4(),
    test5().

test1() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("local0.info    /blah/asdf"),
    io:format("Tokens:~n~p~n", [Tokens]),
    ParseTree = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

test2() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("local0.*    @localhost"),
    io:format("Tokens:~n~p~n", [Tokens]),
    ParseTree = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

test3() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("*.*    @127.0.0.1"),
    io:format("Tokens:~n~p~n", [Tokens]),
    ParseTree = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

test4() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("local0.*    @somehost.com:514"),
    io:format("Tokens:~n~p~n", [Tokens]),
    ParseTree = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

test5() ->
    {ok, Tokens, _} = esyslog_config_lexer:string("*.*    @192.168.1.1:7777"),
    io:format("Tokens:~n~p~n", [Tokens]),
    ParseTree = esyslog_config_parser:parse(Tokens),
    io:format("ParseTree:~n~p~n", [ParseTree]).

