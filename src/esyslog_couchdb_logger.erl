-module(esyslog_couchdb_logger).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

init(Db) ->
    {ok, Db}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({log, Msg = {Priority, Timestamp, Host, Tag, Body}}, Db) ->
    couchbeam:save_doc(Db, {esyslog_message:couchdoc(Msg)}),
    {ok, Db}.
  
handle_call(Call, State) ->
    io:format("~p catchall: ~p, ~p~n", [?MODULE, Call, State]),
    {ok, State}.

handle_info(Info, State) ->
    io:format("~p catchall: ~p, ~p~n", [?MODULE, Info, State]),
    {ok, State}.


    
    
    
    