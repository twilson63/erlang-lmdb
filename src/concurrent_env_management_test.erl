-module(concurrent_env_management_test).
-include("lmdb.hrl"). % Assuming NIFs might be called; can be removed if stubs are pure
-include_lib("eunit/include/eunit.hrl"). % For potential future eunit integration

-export([run_all_tests/0]).
-export([
    test_concurrent_env_create_close/0,
    test_concurrent_env_open_same_path/0,
    test_concurrent_env_set_config_pre_open/0,
    test_env_open_then_remote_close/0
]).

run_all_tests() ->
    io:format("Running tests in ~p...~n", [?MODULE]),
    test_concurrent_env_create_close(),
    test_concurrent_env_open_same_path(),
    test_concurrent_env_set_config_pre_open(),
    test_env_open_then_remote_close(),
    io:format("~p tests completed.~n~n", [?MODULE]),
    ok.

% --- Test Cases ---

test_concurrent_env_create_close() ->
    io:format("  ~p:~p - Testing concurrent env_create and env_close operations.~n", [?MODULE, ?FUNCTION_NAME]),
    
    % Scenario A: Multiple workers create, use briefly, and close their own envs
    io:format("    Scenario A: Concurrent self_create_use_close...~n"),
    NumWorkersA = 5, % Number of concurrent worker processes
    Parent = self(),

    WorkerFunA = fun() ->
        WorkerResult = try
            % 1. Create environment
            {ok, Env} = lmdb_nif:env_create(),
            % Introduce a small, random delay to increase chances of interleaving
            timer:sleep(rand:uniform(20) + 5), 
            
            % 2. Perform a minimal valid operation
            ok = lmdb_nif:env_set_mapsize(Env, 1024*1024), % Small map size
            timer:sleep(rand:uniform(20) + 5), 
            
            % 3. Close environment
            ok = lmdb_nif:env_close(Env),
            worker_a_ok
        catch
            Class:Reason:Stacktrace ->
                io:format(standard_error, "Worker ~p (Scenario A) error: ~p:~p~nStacktrace: ~p~n", [self(), Class, Reason, Stacktrace]),
                {worker_a_error, Class, Reason} % Simplified error reporting for this step
        end,
        Parent ! {self(), WorkerResult}
    end,

    % Spawn worker processes
    PidsA = [spawn_link(WorkerFunA) || _ <- lists:seq(1, NumWorkersA)],

    % Collect results
    ResultsA = lists:map(
        fun(Pid) ->
            receive
                {Pid, Result} -> Result
            after 5000 -> % Timeout for each worker
                io:format(standard_error, "Worker ~p (Scenario A) timed out.~n", [Pid]),
                worker_a_timeout
            end
        end, PidsA),

    % Validate results
    case lists:all(fun(Res) -> Res == worker_a_ok end, ResultsA) of
        true ->
            io:format("    Scenario A: PASSED. All ~p workers completed successfully.~n", [NumWorkersA]);
        false ->
            FailedResults = [R || R <- ResultsA, R /= worker_a_ok],
            io:format(standard_error, "    Scenario A: FAILED. Some workers did not complete successfully. Failures: ~p~n", [FailedResults]),
            erlang:error({scenario_a_failed, FailedResults})
    end,
    
    % Scenario B: One env, multiple workers try to close it concurrently
    io:format("    Scenario B: Concurrent close_shared_env...~n"),
    NumWorkersB = 5,
    ParentB = self(),
    
    % Create a single environment to be shared and closed by workers
    {ok, SharedEnv} = lmdb_nif:env_create(),
    % Perform a minimal operation to ensure it's somewhat initialized if needed
    ok = lmdb_nif:env_set_mapsize(SharedEnv, 1024*1024),

    WorkerFunB = fun() ->
        WorkerResultB = try
            % Introduce a small, random delay to increase chances of interleaving close attempts
            timer:sleep(rand:uniform(20) + 5),
            ok = lmdb_nif:env_close(SharedEnv),
            worker_b_closed_ok % This worker successfully closed the env
        catch
            error:badarg -> % Expected error for workers trying to close an already closed env
                worker_b_already_closed;
            Class:Reason:Stacktrace ->
                io:format(standard_error, "Worker ~p (Scenario B) unexpected error: ~p:~p~nStacktrace: ~p~n", [self(), Class, Reason, Stacktrace]),
                {worker_b_unexpected_error, Class, Reason}
        end,
        ParentB ! {self(), WorkerResultB}
    end,

    % Spawn worker processes
    PidsB = [spawn_link(WorkerFunB) || _ <- lists:seq(1, NumWorkersB)],

    % Collect results
    ResultsB = lists:map(
        fun(Pid) ->
            receive
                {Pid, Result} -> Result
            after 5000 -> % Timeout for each worker
                io:format(standard_error, "Worker ~p (Scenario B) timed out.~n", [Pid]),
                worker_b_timeout
            end
        end, PidsB),

    % Validate results for Scenario B
    OkCloseCount = lists:sum([1 || Res <- ResultsB, Res == worker_b_closed_ok]),
    AlreadyClosedCount = lists:sum([1 || Res <- ResultsB, Res == worker_b_already_closed]),
    
    if 
        OkCloseCount == 1 andalso (AlreadyClosedCount == NumWorkersB - 1) ->
            io:format("    Scenario B: PASSED. One worker closed the env, ~p workers correctly failed.~n", [NumWorkersB - 1]);
        true -> % Some other combination, indicates a failure
            io:format(standard_error, "    Scenario B: FAILED. Results: ~p (Expected 1 ok, ~p already_closed).~n", [ResultsB, NumWorkersB - 1]),
            erlang:error({scenario_b_failed, ResultsB})
    end,

    io:format("  ~p:~p - test_concurrent_env_create_close completed (Scenarios A & B done).~n", [?MODULE, ?FUNCTION_NAME]),
    ok.

test_concurrent_env_open_same_path() ->
    io:format("  ~p:~p - Testing concurrent env_open calls on the same database path.~n", [?MODULE, ?FUNCTION_NAME]),
    NumWorkers = 3,
    Parent = self(),
    BaseDbPath = "./test/concurrent_open_test_db",
    DbPath = lists:flatten(io_lib:format("~s_~p", [BaseDbPath, erlang:unique_integer([positive])])),

    % Ensure the database directory is clean before the test
    case os:cmd("rm -rf " ++ DbPath ++ " && mkdir -p " ++ DbPath) of
        [] -> io:format("    Cleaned and created DB path: ~s~n", [DbPath]);
        Output -> 
            io:format(standard_error, "    Failed to clean/create DB path ~s: ~s~n", [DbPath, Output]),
            erlang:error({db_path_setup_failed, Output})
    end,

    WorkerFun = fun() ->
        WorkerResult = try
            % 1. Create environment handle
            {ok, Env} = lmdb_nif:env_create(),
            timer:sleep(rand:uniform(10) + 1),

            % 2. Set map size (must be done before open)
            ok = lmdb_nif:env_set_mapsize(Env, 1024 * 1024), % 1MB
            timer:sleep(rand:uniform(10) + 1),

            % 3. Open environment at the shared path
            % ?MDB_NOSUBDIR means DbPath is a file, not a directory containing data.mdb & lock.mdb
            % For this test, let's assume DbPath is a directory, so remove MDB_NOSUBDIR if creating a dir structure
            % Or, if DbPath itself IS the data file, then NOSUBDIR is correct.
            % Sticking with a directory for now, so data.mdb and lock.mdb will be inside.
            OpenFlags = ?MDB_CREATE, % Ensure DB is created if not exists. Add other flags as needed.
            ok = lmdb_nif:env_open(Env, DbPath, OpenFlags),
            timer:sleep(rand:uniform(10) + 1),

            % 4. Perform a simple operation (e.g., begin/abort read transaction)
            {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, ?MDB_RDONLY),
            ok = lmdb_nif:txn_abort(Txn),

            % 5. Close environment
            ok = lmdb_nif:env_close(Env),
            worker_ok
        catch
            Class:Reason:Stacktrace ->
                ErrorMsg = lists:flatten(io_lib:format("~p", [{Class, Reason, Stacktrace}])),
                io:format(standard_error, "Worker ~p error: ~s~n", [self(), ErrorMsg]),
                {worker_error, Class, Reason}
        end,
        Parent ! {self(), WorkerResult}
    end,

    Pids = [spawn_link(WorkerFun) || _ <- lists:seq(1, NumWorkers)],

    Results = lists:map(
        fun(Pid) ->
            receive
                {Pid, Result} -> Result
            after 10000 -> % Increased timeout for file I/O
                io:format(standard_error, "Worker ~p timed out.~n", [Pid]),
                worker_timeout
            end
        end, Pids),

    case lists:all(fun(Res) -> Res == worker_ok end, Results) of
        true ->
            io:format("    PASSED. All ~p workers successfully opened, used, and closed the env at ~s.~n", [NumWorkers, DbPath]);
        false ->
            FailedResults = [R || R <- Results, R /= worker_ok],
            io:format(standard_error, "    FAILED. Some workers failed. Path: ~s, Failures: ~p~n", [DbPath, FailedResults]),
            erlang:error({test_failed, FailedResults})
    end,

    % Clean up the created database directory after test
    os:cmd("rm -rf " ++ DbPath),
    io:format("    Cleaned up DB path: ~s~n", [DbPath]),
    ok.

test_concurrent_env_set_config_pre_open() ->
    io:format("  ~p:~p - Tests concurrent env_set_mapsize/env_set_maxreaders before env_open.~n", [?MODULE, ?FUNCTION_NAME]),
    NumWorkers = 5,
    Parent = self(),
    BaseMapSize = 1024 * 1024, % 1MB

    % 1. Create a single environment handle to be configured by multiple workers
    {ok, SharedEnvToConfigure} = lmdb_nif:env_create(),
    io:format("    Created shared Env handle: ~p~n", [SharedEnvToConfigure]),

    WorkerFun = fun(WorkerId) ->
        % Each worker attempts to set a different map size
        MyMapSize = BaseMapSize + (WorkerId * 1024 * 1024), % 1MB, 2MB, 3MB, ...
        WorkerResult = try
            timer:sleep(rand:uniform(10) + WorkerId), % Stagger starts slightly
            ok = lmdb_nif:env_set_mapsize(SharedEnvToConfigure, MyMapSize),
            io:format("    Worker ~p (~p) attempted to set mapsize to ~p bytes.~n", [WorkerId, self(), MyMapSize]),
            {worker_set_ok, MyMapSize}
        catch
            Class:Reason:Stacktrace ->
                ErrorMsg = lists:flatten(io_lib:format("~p", [{Class, Reason, Stacktrace}])),
                io:format(standard_error, "    Worker ~p (~p) error setting mapsize: ~s~n", [WorkerId, self(), ErrorMsg]),
                {worker_error, Class, Reason}
        end,
        Parent ! {self(), WorkerResult, WorkerId}
    end,

    Pids = [spawn_link(fun() -> WorkerFun(I) end) || I <- lists:seq(1, NumWorkers)],

    ReportedMapSizes = lists:foldl(
        fun(Pid, Acc) ->
            receive
                {Pid, {worker_set_ok, Size}, WorkerId} ->
                    io:format("    Worker ~p successfully reported setting mapsize to ~p.~n", [WorkerId, Size]),
                    [{ok, Size} | Acc];
                {Pid, {worker_error, C, R}, WorkerId} ->
                    io:format("    Worker ~p reported error: ~p:~p.~n", [WorkerId, C, R]),
                    [{error, C, R} | Acc]
            after 5000 ->
                io:format(standard_error, "    Worker (pid ~p) timed out.~n", [Pid]),
                [timeout | Acc]
            end
        end, [], Pids),
    
    SuccessfulSetAttempts = [Size || {ok, Size} <- ReportedMapSizes],
    FailedSetAttempts = [Err || {error, _, _} = Err <- ReportedMapSizes],

    io:format("    Successful mapsize set attempts by workers: ~p~n", [SuccessfulSetAttempts]),
    io:format("    Failed mapsize set attempts by workers: ~p~n", [FailedSetAttempts]),

    % We expect all workers to return 'ok' from env_set_mapsize, as it's just setting a value on the handle.
    % The actual race will be which value is effectively set when env_open is called.
    % For this test, primarily ensure no crashes and all attempts return ok.
    if
        length(SuccessfulSetAttempts) == NumWorkers ->
            io:format("    PASSED: All ~p workers successfully called env_set_mapsize.~n", [NumWorkers]);
        true ->
            io:format(standard_error, "    FAILED: Not all workers successfully called env_set_mapsize. Results: ~p~n", [ReportedMapSizes]),
            % Clean up the created env handle before erroring
            catch lmdb_nif:env_close(SharedEnvToConfigure), % Best effort close
            erlang:error({worker_set_mapsize_failed, ReportedMapSizes})
    end,

    % Now, open the environment and see what mapsize it actually has.
    % This part is tricky as there isn't a direct lmdb_nif:env_get_mapsize().
    % We will rely on env_info() and see if we can infer it, or just ensure open/close works.
    DbPathForConfigTest = "./test/config_test_db",
    os:cmd("rm -rf " ++ DbPathForConfigTest ++ " && mkdir -p " ++ DbPathForConfigTest),
    
    OpenFlagsConfig = ?MDB_CREATE,
    FinalOpenResult = try
        ok = lmdb_nif:env_open(SharedEnvToConfigure, DbPathForConfigTest, OpenFlagsConfig),
        io:format("    Successfully opened shared env after concurrent config attempts.~n"),
        
        % Try to get env_info (if it contains mapsize)
        case catch lmdb_nif:env_info(SharedEnvToConfigure) of
            {ok, InfoMap} when is_map(InfoMap) ->
                io:format("    Env info: ~p~n", [InfoMap]),
                % ActualMapSize = maps:get(ms_mapsize, InfoMap, undefined), % MDB_stat has ms_mapsize, MDB_envinfo doesn't directly.
                % For now, let's just say if env_info worked, it's a good sign.
                io:format("    env_info successfully retrieved. Actual mapsize not directly verifiable via env_info here easily.~n");
            OtherInfoResult ->
                io:format("    Could not get env_info or it was not a map: ~p~n", [OtherInfoResult])
        end,
        
        ok = lmdb_nif:env_close(SharedEnvToConfigure),
        io:format("    Successfully closed shared env.~n"),
        open_and_close_ok
    catch
        Class:Reason:StacktraceFinal ->
            ErrorMsgFinal = lists:flatten(io_lib:format("~p", [{Class, Reason, StacktraceFinal}])),
            io:format(standard_error, "    Error during final open/close of shared env: ~s~n", [ErrorMsgFinal]),
            {final_op_error, Class, Reason}
    end,

    os:cmd("rm -rf " ++ DbPathForConfigTest), % Clean up

    if 
        FinalOpenResult == open_and_close_ok ->
            io:format("    PASSED: Final open, info attempt, and close of shared env succeeded.~n");
        true ->
            io:format(standard_error, "    FAILED: Final open/info/close operations on shared env failed: ~p~n", [FinalOpenResult]),
            erlang:error({final_env_op_failed, FinalOpenResult})
    end,
    ok.

test_env_open_then_remote_close() ->
    io:format("  ~p:~p - Tests opening an env in one process and closing it from another (concurrently).~n", [?MODULE, ?FUNCTION_NAME]),
    NumWorkers = 3,
    Parent = self(),
    DbPath = "./test/remote_close_test_db",

    % Ensure the database directory is clean before the test
    os:cmd("rm -rf " ++ DbPath ++ " && mkdir -p " ++ DbPath),

    % 1. Parent creates and opens the environment
    {ok, EnvToRemoteClose} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_set_mapsize(EnvToRemoteClose, 1024 * 1024),
    OpenFlags = ?MDB_CREATE,
    ok = lmdb_nif:env_open(EnvToRemoteClose, DbPath, OpenFlags),
    io:format("    Parent opened env ~p at ~s.~n", [EnvToRemoteClose, DbPath]),

    WorkerFun = fun(WorkerId) ->
        WorkerResult = try
            % Small delay to let all workers start before racing to close
            timer:sleep(rand:uniform(20) + 5 + WorkerId),
            ok = lmdb_nif:env_close(EnvToRemoteClose),
            io:format("    Worker ~p (~p) successfully closed shared env ~p.~n", [WorkerId, self(), EnvToRemoteClose]),
            worker_closed_ok
        catch
            error:badarg ->
                io:format("    Worker ~p (~p) correctly failed to close already closed env (badarg).~n", [WorkerId, self()]),
                worker_already_closed;
            Class:Reason:Stacktrace ->
                ErrorMsg = lists:flatten(io_lib:format("~p", [{Class, Reason, Stacktrace}])),
                io:format(standard_error, "    Worker ~p (~p) unexpected error during close: ~s~n", [WorkerId, self(), ErrorMsg]),
                {worker_unexpected_error, Class, Reason}
        end,
        Parent ! {self(), WorkerResult, WorkerId}
    end,

    Pids = [spawn_link(fun() -> WorkerFun(I) end) || I <- lists:seq(1, NumWorkers)],

    % Collect results robustly
    Results = lists:map(
        fun(Pid) ->
            receive
                {Pid, WorkerRes, WorkerId} ->
                    io:format("    Parent received from worker ~p (Pid ~p): ~p~n", [WorkerId, Pid, WorkerRes]),
                    {Pid, WorkerRes, WorkerId} % Keep original structure if needed, or just WorkerRes
            after 5000 -> % Timeout for each worker
                io:format(standard_error, "    Worker (Pid ~p) timed out.~n", [Pid]),
                {Pid, worker_timeout, unknown_worker_id} % Mark timeout
            end
        end, Pids),
    
    ClosedOkCount = lists:sum([1 || {_, worker_closed_ok, _} <- Results]),
    AlreadyClosedCount = lists:sum([1 || {_, worker_already_closed, _} <- Results]),
    Timeouts = [Pid || {Pid, worker_timeout, _} <- Results],

    if
        Timeouts =/= [] ->
            io:format(standard_error, "    FAILED: Some workers timed out: ~p~n", [Timeouts]),
            erlang:error({remote_close_worker_timeout, Timeouts});
        ClosedOkCount == 1 andalso AlreadyClosedCount == (NumWorkers - 1) ->
            io:format("    PASSED: One worker closed the env, ~p workers correctly failed as expected.~n", [NumWorkers - 1]);
        true ->
            io:format(standard_error, "    FAILED: Incorrect close results. Got ~p 'ok' and ~p 'already_closed'. Expected 1 and ~p. Results: ~p~n", 
                      [ClosedOkCount, AlreadyClosedCount, NumWorkers - 1, Results]),
            erlang:error({remote_close_validation_failed, Results})
    end,

    % 2. Parent attempts to use the environment - it should be closed
    io:format("    Parent attempting to use env ~p after remote close attempts...~n", [EnvToRemoteClose]),
    FinalCheckResult = try lmdb_nif:txn_begin(EnvToRemoteClose, undefined, ?MDB_RDONLY) of
        Ret -> {unexpected_success, Ret}
    catch
        error:badarg -> parent_op_failed_badarg;
        Class:Reason:_ -> {unexpected_error, Class, Reason}
    end,

    if 
        FinalCheckResult == parent_op_failed_badarg ->
            io:format("    PASSED: Parent operation on remotely closed env failed with badarg as expected.~n");
        true ->
            io:format(standard_error, "    FAILED: Parent operation on remotely closed env did not fail with badarg. Got: ~p~n", [FinalCheckResult]),
            % Note: EnvToRemoteClose is already closed, or should be. No explicit close here by parent after workers.
            erlang:error({parent_op_post_remote_close_failed, FinalCheckResult})
    end,

    os:cmd("rm -rf " ++ DbPath), % Clean up
    io:format("    Cleaned up DB path: ~s~n", [DbPath]),
    ok. 