-module(esyslog_event_manager).
-export([start/0]).

start() ->
    {ok, Config} = esyslog_config:parse("etc/syslog.conf"),
    io:format("Starting event manager~n"),
    {ok, Pid} = gen_event:start({local, esyslog_logger}),
    
    couchbeam:start(),
    couchbeam_server:start_connection(),
    Db = couchbeam_db:open(default, "esyslog"),

    gen_event:add_handler(esyslog_logger, esyslog_standard_logger, Config),
    gen_event:add_handler(esyslog_logger, esyslog_couchdb_logger, Db),

    {ok, Pid}.

