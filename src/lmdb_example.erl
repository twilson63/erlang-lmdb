%%% @doc Example usage of LMDB NIF
-module(lmdb_example).
-export([
    basic_example/0,
    transaction_example/0,
    batch_example/0,
    iteration_example/0,
    multi_db_example/0,
    performance_test/0,
    run_all_examples/0
]).
-include("lmdb.hrl").

-define(TEST_FILES, [
    <<"test/example_db">>,
    <<"test/tx_example_db">>,
    <<"test/batch_example_db">>,
    <<"test/iter_example_db">>,
    <<"test/multi_db_example">>,
    <<"test/perf_test_db">>
]).

%% @doc Basic key-value operations
basic_example() ->
    filelib:ensure_dir("test/example_db"),
    io:format("=== Basic LMDB Example ===~n"),
    
    % Create and configure environment
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760), % 10MB
    ok = lmdb:env_open(Env, "test/example_db", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    % Simple put/get operations
    {ok, Value} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        
        % Store some data
        ok = lmdb:put(Txn, Dbi, <<"user:alice">>, <<"Alice Smith">>),
        ok = lmdb:put(Txn, Dbi, <<"user:bob">>, <<"Bob Jones">>),
        ok = lmdb:put(Txn, Dbi, <<"user:charlie">>, <<"Charlie Brown">>),
        
        % Retrieve data
        {ok, Name} = lmdb:get(Txn, Dbi, <<"user:alice">>),
        io:format("Retrieved: ~s~n", [Name]),
        
        Name
    end),
    
    % Read-only access
    {ok, {ok, <<"Bob Jones">>}} = lmdb:with_ro_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        lmdb:get(Txn, Dbi, <<"user:bob">>)
    end),
    
    ok = lmdb:env_close(Env),
    io:format("Basic example completed successfully~n"),
    Value.

%% @doc Transaction rollback example
transaction_example() ->
    filelib:ensure_dir("test/tx_example_db"),
    io:format("=== Transaction Example ===~n"),
    
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, "test/tx_example_db", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    % Successful transaction
    {ok, success} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        ok = lmdb:put(Txn, Dbi, <<"committed_key">>, <<"committed_value">>),
        io:format("Data will be committed~n"),
        success
    end),
    
    % Failed transaction (will rollback)
    {error, rollback_test} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        ok = lmdb:put(Txn, Dbi, <<"rolled_back_key">>, <<"rolled_back_value">>),
        io:format("Data will be rolled back~n"),
        throw(rollback_test)
    end),
    
    % Verify rollback worked
    {ok, Results} = lmdb:with_ro_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        Committed = lmdb:get(Txn, Dbi, <<"committed_key">>),
        RolledBack = lmdb:get(Txn, Dbi, <<"rolled_back_key">>),
        {Committed, RolledBack}
    end),
    
    {{ok, <<"committed_value">>}, not_found} = Results,
    io:format("Transaction rollback worked correctly~n"),
    
    ok = lmdb:env_close(Env),
    io:format("Transaction example completed~n").

%% @doc Batch operations example
batch_example() ->
    filelib:ensure_dir("test/batch_example_db"),
    io:format("=== Batch Operations Example ===~n"),
    
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, "test/batch_example_db", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    % Get database handle
    {ok, Dbi} = lmdb:with_txn(Env, fun(Txn) ->
        lmdb:open_db(Txn, default)
    end),
    
    % Batch write operations
    Operations = [
        {put, Dbi, <<"product:1">>, <<"Laptop">>},
        {put, Dbi, <<"product:2">>, <<"Mouse">>},
        {put, Dbi, <<"product:3">>, <<"Keyboard">>},
        {put, Dbi, <<"category:electronics">>, <<"Electronics">>}
    ],
    
    ok = lmdb:write_batch(Env, Operations),
    io:format("Batch write completed~n"),
    
    % Verify batch results
    {ok, Count} = lmdb:count(Env, Dbi),
    io:format("Database contains ~p entries~n", [Count]),
    
    ok = lmdb:env_close(Env),
    io:format("Batch example completed~n").

%% @doc Database iteration example
iteration_example() ->
    filelib:ensure_dir("test/iter_example_db"),
    io:format("=== Iteration Example ===~n"),
    
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, "test/iter_example_db", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    % Setup test data
    {ok, Dbi} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        lists:foreach(fun(N) ->
            Key = list_to_binary("key" ++ integer_to_list(N)),
            Value = list_to_binary("value" ++ integer_to_list(N)),
            ok = lmdb:put(Txn, Dbi, Key, Value)
        end, lists:seq(1, 10)),
        Dbi
    end),
    
    % Iterate and collect all keys
    {ok, Keys} = lmdb:fold(Env, Dbi, fun(Key, _Value, Acc) ->
        [Key | Acc]
    end, []),
    
    io:format("Found ~p keys: ~p~n", [length(Keys), Keys]),
    
    % Count entries with specific prefix
    {ok, PrefixCount} = lmdb:fold(Env, Dbi, fun(Key, _Value, Acc) ->
        case binary:match(Key, <<"key">>) of
            {0, _} -> Acc + 1;
            _ -> Acc
        end
    end, 0),
    
    io:format("Keys with 'key' prefix: ~p~n", [PrefixCount]),
    
    % Execute function for each entry
    ok = lmdb:foreach(Env, Dbi, fun(Key, Value) ->
        io:format("  ~s -> ~s~n", [Key, Value])
    end),
    
    ok = lmdb:env_close(Env),
    io:format("Iteration example completed~n").

%% @doc Multiple databases example
multi_db_example() ->
    filelib:ensure_dir("test/multi_db_example"),
    io:format("=== Multiple Databases Example ===~n"),
    
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_maxdbs(Env, 5), % Allow multiple databases
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, "test/multi_db_example", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    {ok, Result} = lmdb:with_txn(Env, fun(Txn) ->
        % Open multiple databases
        {ok, UsersDbi} = lmdb:open_db(Txn, "users", ?MDB_CREATE),
        {ok, ProductsDbi} = lmdb:open_db(Txn, "products", ?MDB_CREATE),
        {ok, OrdersDbi} = lmdb:open_db(Txn, "orders", ?MDB_CREATE),
        
        % Store data in different databases
        ok = lmdb:put(Txn, UsersDbi, <<"1">>, <<"Alice">>),
        ok = lmdb:put(Txn, UsersDbi, <<"2">>, <<"Bob">>),
        
        ok = lmdb:put(Txn, ProductsDbi, <<"laptop">>, <<"$999">>),
        ok = lmdb:put(Txn, ProductsDbi, <<"mouse">>, <<"$29">>),
        
        ok = lmdb:put(Txn, OrdersDbi, <<"order1">>, <<"user:1,product:laptop">>),
        
        % Retrieve from different databases
        {ok, UserName} = lmdb:get(Txn, UsersDbi, <<"1">>),
        {ok, ProductPrice} = lmdb:get(Txn, ProductsDbi, <<"laptop">>),
        {ok, OrderInfo} = lmdb:get(Txn, OrdersDbi, <<"order1">>),
        
        {UserName, ProductPrice, OrderInfo}
    end),
    
    {<<"Alice">>, <<"$999">>, <<"user:1,product:laptop">>} = Result,
    io:format("Multi-database operations successful~n"),
    
    ok = lmdb:env_close(Env),
    io:format("Multi-database example completed~n").

%% @doc Performance test example. By default, it performs 1M writes and 1M reads
%% over a 100MB database.
performance_test() ->
    performance_test(1_000_000, 1_000_000, 600*1024*1024). % 600MB

performance_test(WriteOps, ReadOps, DBSize) ->
    filelib:ensure_dir("test/perf_test_db"),
    io:format("=== Performance Test ===~n"),
    io:format("    Performing writes: ~w ops.~n", [WriteOps]),
    % Initialize environment
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, DBSize),
    ok = lmdb:env_open(Env, "test/perf_test_db", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    % Write `WriteOps' records to the new database.
    RandomData = base64:encode(crypto:strong_rand_bytes(32)),
    {WriteTime, {ok, ok}} =
        timer:tc(
            fun() ->
            lmdb:with_txn(Env, fun(Txn) ->
                {ok, Dbi} = lmdb:open_db(Txn, default),
                % Write records directly instead of using write_batch.
                lists:foreach(
                    fun(N) ->
                        ok =
                            lmdb:put(
                                Txn,
                                Dbi,
                                <<"key", N:256/integer>>,
                                RandomData
                            )
                    end,
                    lists:seq(1, WriteOps)
                ),
                ok
            end)
        end),
    % Calculate write rate.
    WriteRate = erlang:round(WriteOps / (WriteTime / 1000000)),
    io:format("    Write time: ~w ms~n", [WriteTime/1000]),
    io:format("    Write rate: ~w records/second~n", [WriteRate]),
    io:format("    Performing reads: ~w ops.~n", [ReadOps]),
    % Generate keys to read ahead of time.
    Keys =
        lists:map(
            fun(_) ->
                << "key", (rand:uniform(ReadOps)):256/integer >>
            end,
            lists:seq(1, ReadOps)
        ),
    % Time random reads.
    {ReadTime, {ok, _}} = timer:tc(fun() ->
        lmdb:with_ro_txn(Env, fun(Txn) ->
            {ok, Dbi} = lmdb:open_db(Txn, default),
            lists:foreach(fun(Key) ->
                lmdb:get(Txn, Dbi, Key)
            end, Keys),
            ok
        end)
    end),
    % Calculate read rate.
    ReadRate = erlang:round(ReadOps / (ReadTime / 1000000)),
    io:format("    Read time: ~w ms~n", [ReadTime/1000]),
    io:format("    Read rate: ~w records/second~n", [ReadRate]),
    % Get database statistics
    {ok, Stats} =
        lmdb:with_ro_txn(
            Env,
            fun(Txn) ->
                {ok, Dbi} = lmdb:open_db(Txn, default),
                lmdb_nif:dbi_stat(Txn, Dbi)
            end
        ),
    io:format("    Database statistics: ~p~n", [Stats]),
    ok = lmdb:env_close(Env),
    io:format("Performance test completed~n").

%% @doc Run all examples
run_all_examples() ->
    basic_example(),
    transaction_example(),
    batch_example(),
    iteration_example(),
    multi_db_example(),
    performance_test(),
    % Cleanup
    os:cmd(
        "rm -rf " ++
            string:join(
                lists:map(fun binary_to_list/1, ?TEST_FILES),
                " "
            )
    ),
    io:format("All examples completed successfully!~n").
