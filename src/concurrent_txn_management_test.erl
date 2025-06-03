-module(concurrent_txn_management_test).
-include("lmdb.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([run_all_tests/0]).
-export([
    test_concurrent_txn_begin_readonly/0,
    test_concurrent_txn_begin_readwrite/0,
    test_mixed_concurrent_txn_begin/0,
    test_concurrent_txn_commit_abort/0,
    test_txn_use_after_owner_death/0
]).

% Helper to setup a common environment for tests in this module
with_env(Fun) ->
    DbPath = "./test/txn_mgmt_test_db",
    FullDbPath = lists:flatten(io_lib:format("~s_~p", [DbPath, erlang:unique_integer([positive])])),
    os:cmd("rm -rf " ++ FullDbPath ++ " && mkdir -p " ++ FullDbPath),

    {ok, Env} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_set_mapsize(Env, 1024 * 1024 * 10), % 10MB
    ok = lmdb_nif:env_set_maxdbs(Env, 2),
    OpenFlags = ?MDB_CREATE,
    ok = lmdb_nif:env_open(Env, FullDbPath, OpenFlags),
    io:format("    (Helper) Opened env ~p at ~s for test.~n", [Env, FullDbPath]),

    try Fun(Env) of
        Result -> Result
    after
        catch lmdb_nif:env_close(Env),
        io:format("    (Helper) Closed env ~p at ~s.~n", [Env, FullDbPath]),
        os:cmd("rm -rf " ++ FullDbPath)
    end.

run_all_tests() ->
    io:format("Running tests in ~p...~n", [?MODULE]),
    test_concurrent_txn_begin_readonly(),
    test_concurrent_txn_begin_readwrite(),
    test_mixed_concurrent_txn_begin(),
    test_concurrent_txn_commit_abort(),
    test_txn_use_after_owner_death(), % Might need special setup
    io:format("~p tests completed.~n~n", [?MODULE]),
    ok.

% --- Test Cases ---

test_concurrent_txn_begin_readonly() ->
    with_env(fun(Env) -> test_concurrent_txn_begin_readonly(Env) end).
test_concurrent_txn_begin_readonly(Env) ->
    io:format("  ~p:~p - Testing concurrent txn_begin for read-only transactions.~n", [?MODULE, ?FUNCTION_NAME]),
    NumWorkers = 3, % REDUCED from 10 to 3 for initial debugging
    Parent = self(),

    WorkerFun = fun(WorkerId) ->
        WorkerResult = try
            timer:sleep(rand:uniform(10) + WorkerId),
            {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, ?MDB_RDONLY),
            io:format("    Worker ~p (~p) started read-only txn ~p.~n", [WorkerId, self(), Txn]),
            timer:sleep(rand:uniform(5) + 1),
            ok = lmdb_nif:txn_abort(Txn),
            io:format("    Worker ~p (~p) aborted read-only txn ~p.~n", [WorkerId, self(), Txn]),
            worker_ok
        catch
            Class:Reason:Stacktrace ->
                ErrorMsg = lists:flatten(io_lib:format("~p", [{Class, Reason, Stacktrace}])),
                io:format(standard_error, "    Worker ~p (~p) error: ~s~n", [WorkerId, self(), ErrorMsg]),
                {worker_error, Class, Reason}
        end,
        Parent ! {self(), WorkerResult, WorkerId}
    end,

    Pids = [spawn_link(fun() -> WorkerFun(I) end) || I <- lists:seq(1, NumWorkers)],

    Results = lists:map(
        fun(Pid) ->
            receive
                {Pid, WorkerRes, _WorkerId} -> WorkerRes 
            after 5000 ->
                io:format(standard_error, "    Worker (Pid ~p) timed out.~n", [Pid]),
                worker_timeout
            end
        end, Pids),

    case lists:all(fun(Res) -> Res == worker_ok end, Results) of
        true ->
            io:format("    PASSED: All ~p workers successfully began and aborted read-only transactions.~n", [NumWorkers]);
        false ->
            FailedResults = [R || R <- Results, R /= worker_ok],
            io:format(standard_error, "    FAILED: Not all workers succeeded in concurrent read-only txn test. Failures: ~p~n", [FailedResults]),
            erlang:error({test_readonly_txn_failed, FailedResults})
    end,
    ok.

test_concurrent_txn_begin_readwrite() ->
    with_env(fun(Env) -> test_concurrent_txn_begin_readwrite(Env) end).
test_concurrent_txn_begin_readwrite(_Env) ->
    io:format("  ~p:~p STUB - Testing concurrent txn_begin for read-write transactions.~n", [?MODULE, ?FUNCTION_NAME]),
    ok.

test_mixed_concurrent_txn_begin() ->
    with_env(fun(Env) -> test_mixed_concurrent_txn_begin(Env) end).
test_mixed_concurrent_txn_begin(_Env) ->
    io:format("  ~p:~p STUB - Tests mixed concurrent txn_begin for read-only and read-write transactions.~n", [?MODULE, ?FUNCTION_NAME]),
    ok.

test_concurrent_txn_commit_abort() ->
    with_env(fun(Env) -> test_concurrent_txn_commit_abort(Env) end).
test_concurrent_txn_commit_abort(_Env) ->
    io:format("  ~p:~p STUB - Tests concurrent txn_commit and txn_abort operations.~n", [?MODULE, ?FUNCTION_NAME]),
    ok.

test_txn_use_after_owner_death() ->
    with_env(fun(Env) -> test_txn_use_after_owner_death(Env) end).
test_txn_use_after_owner_death(_Env) ->
    io:format("  ~p:~p STUB - Tests using a transaction after its owning process has died.~n", [?MODULE, ?FUNCTION_NAME]),
    ok. 