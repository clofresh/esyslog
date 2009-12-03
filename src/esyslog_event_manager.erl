-module(esyslog_event_manager).
-export([start/0]).

start() ->
    {ok, Config} = esyslog_config:parse("etc/syslog.conf"),
    io:format("Starting event manager~n"),
    {ok, Pid} = gen_event:start({local, esyslog_logger}),
    gen_event:add_handler(esyslog_logger, esyslog_standard_logger, Config),
    {ok, Pid}.

