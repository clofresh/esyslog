-module(esyslog_event_manager).
-export([start/0]).

-include("deps/couchbeam/include/couchbeam.hrl").

start() ->
    {ok, Config} = esyslog_config:parse("etc/syslog.conf"),
    io:format("Starting event manager~n"),
    {ok, Pid} = gen_event:start({local, esyslog_logger}),
    
    couchbeam:start(),
    
    {ok, {CouchDbHost, CouchDbPort}} = application:get_env(esyslog, couchdb),
    
    Prefix = "",
    Options = [],
    
    S = couchbeam:server_connection(CouchDbHost, CouchDbPort, Prefix, Options),
    {ok, Db} = couchbeam:open_db(S, "esyslog"),

    gen_event:add_handler(esyslog_logger, esyslog_standard_logger, Config),
    gen_event:add_handler(esyslog_logger, esyslog_couchdb_logger, Db),

    {ok, Pid}.

