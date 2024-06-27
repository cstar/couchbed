%%%-------------------------------------------------------------------
%% @doc couchbed public API
%% @end
%%%-------------------------------------------------------------------

-module(couchbed).
-export([run/0, start_client/0, start/0, get/0, generate_and_store_docs/2, create_view/1, query_view/2]).

-include_lib("couchbeam/include/couchbeam.hrl").

-define(DB_NAME, "test").
-define(COUCH_URL, "http://localhost").
-define(OPTIONS, [{basic_auth, {"admin", "admin"}}]).
%-define(OPTIONS, []).

run() ->
    start(),
    Db = ?MODULE:get(),
    %generate_and_store_docs(Db, 50000),
    Pid = start_client(),
    % create_view(Db),
    query_view(Db, Pid),
    hackney_pool:get_stats(default),
    ok.
start() -> 
    application:ensure_all_started(couchbeam).

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

generate_and_store_docs(Db, N) ->
    lists:foreach(fun(_) ->
        Doc = {[{<<"type">>,<<"random_doc">>}, {<<"value">>, rand:uniform(1000)}]},
        {ok, _Doc} = couchbeam:save_doc(Db, Doc)
    end, lists:seq(1, N)).

create_view(Db) ->
    View = {
        [{<<"views">>, [{
            <<"random_view">>,{
               [{ <<"map">>, <<"fun(Doc) -> emit(Doc.id, Doc.value) end">>}]
            }
        }]}]
    },
    couchbeam:save_doc(Db, View).

query_view(Db, Pid) ->
    {ok, _Ref } = couchbeam_view:stream(Db, all_docs, [{stream_to, Pid}]),
    timer:sleep(14),
    %kill_view_stream_process(),
    %{error,simple_one_for_one} = couchbeam_view:cancel_stream(Ref),
    %io:format("View result: ~p~n", [ViewResult]).
    ok.

start_client() -> 
    Pid = spawn(fun() -> loop() end),
    Pid.

% kill_view_stream_process() ->
%     [{_Ref, Pid}|_R] = ets:tab2list(couchbeam_view_streams),
%     io:format("~p~n", [ets:tab2list(couchbeam_view_streams)]),
%     exit(Pid, kill).

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
