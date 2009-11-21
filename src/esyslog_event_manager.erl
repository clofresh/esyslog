-module(esyslog_event_manager).
-export([start/0]).

start() ->
    io:format("Starting event manager~n"),
    {ok, Pid} = gen_event:start({local, esyslog_logger}),
    gen_event:add_handler(esyslog_logger, esyslog_console_logger, []),
    {ok, Pid}.

