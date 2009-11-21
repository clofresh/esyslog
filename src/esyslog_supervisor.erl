-module(esyslog_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    io:format("Starting supervisor~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    Port = 7777,
    
    Policy = {one_for_one, 1, 60},
    EventManager = {esyslog_event_manager, 
                        {esyslog_event_manager, start, []},
                        permanent, brutal_kill, worker, dynamic},
    Server = {esyslog_server, 
                {esyslog_server, start, [Port]},
                permanent, brutal_kill, worker, [esyslog_server]},

    {ok, {Policy, [EventManager, Server]}}.
