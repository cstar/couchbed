%%%-------------------------------------------------------------------
%% @doc couchbed public API
%% @end
%%%-------------------------------------------------------------------

-module(couchbed).
-export([run/0, client/0, server/0, start_client/0, start/0, get/0, generate_and_store_docs/2, create_view/1, query_view/2]).

-include_lib("couchbeam/include/couchbeam.hrl").

-define(DB_NAME, "test").
% -define(COUCH_URL, "http://localhost:5984").
-define(COUCH_URL, "http://localhost").
-define(OPTIONS, [{basic_auth, {"admin", "admin"}}]).
%-define(OPTIONS, []).

-spec run() -> ok.
run() ->
    start(),
    Db = ?MODULE:get(),
    %generate_and_store_docs(Db, 500),
    Pid = start_client(),
    %create_view(Db),
    query_view(Db, Pid),
    io:format("~p~n", [hackney_pool:get_stats(default)]),
    ok.

-spec start() -> ok.
start() -> 
    application:ensure_all_started(couchbeam).

    -spec client() -> ok.
client() ->
    couchbed_client:start().
-spec server() -> ok.
server() ->
    couchbed_server:start().

-spec get() -> couchbeam:db().
get() ->
    S = couchbeam:server_connection(?COUCH_URL, ?OPTIONS),
    case couchbeam:db_exists(S, "_users") of
        false -> couchbeam:create_db(S, "_users");
        true -> ok
    end,
    {ok, Db} = case couchbeam:db_exists(S, ?DB_NAME) of
        true -> couchbeam:open_db(S, ?DB_NAME, []);
        false -> couchbeam:create_db(S, ?DB_NAME),
                    couchbeam:open_db(S, ?DB_NAME, [])
    end,
    
    Db.

-spec generate_and_store_docs(couchbeam:db(), non_neg_integer()) -> ok.
generate_and_store_docs(Db, N) ->
    lists:foreach(fun(_) ->
        Doc = {[{<<"type">>,<<"random_doc">>}, {<<"value">>, rand:uniform(1000)}]},
        {ok, _Doc} = couchbeam:save_doc(Db, Doc)
    end, lists:seq(1, N)),
    ok.

-spec create_view(couchbeam:db()) -> ok.
create_view(Db) ->
    create_view(Db, <<"couchbeam">>, <<"view1">>).

-spec create_view(couchbeam:db(), binary(), binary()) -> couchbeam:document().  
create_view(Db, Design, View) ->
    DesignDoc = {[
        {<<"_id">>, <<"_design/",Design/binary>>},
        {<<"language">>,<<"javascript">>},
        {<<"views">>,
            {[{View,
                {[{<<"map">>,
                    <<"function (doc) {\n emit(doc._id, doc); \n}">>
                }]}
            }]}
        }
    ]},
    {ok, DesignDoc1} = couchbeam:save_doc(Db, DesignDoc),
    DesignDoc1.


-spec query_view(couchbeam:db(), pid()) -> ok.
query_view(Db, Pid) ->
    {ok, _Ref } = couchbeam_view:stream(Db, {"couchbeam", "view1"}, [{stream_to, Pid}]),
    timer:sleep(14),
    kill_view_stream_process(),
    %{error,simple_one_for_one} = couchbeam_view:cancel_stream(Ref),
    %io:format("View result: ~p~n", [ViewResult]).
    ok.

-spec start_client() -> pid().
start_client() -> 
    Pid = spawn(fun() -> loop() end),
    Pid.

-spec kill_view_stream_process() -> ok.
kill_view_stream_process() ->
    io:format("~p~n", [ets:tab2list(couchbeam_view_streams)]),
    killall(ets:tab2list(couchbeam_view_streams)).
    
-spec killall(list()) -> ok.   
killall([]) -> ok;
killall([{_Ref, Pid} | Pids]) ->
    exit(Pid, kill),
    killall(Pids).

-spec loop() -> no_return().
loop() ->
    receive
        _Message ->
            io:format(".", []),
            %loop()
            case rand:uniform(400) of
            1 ->
                exit(normal);
            _ -> loop() % Wait for 0.1 second
            end
    end.