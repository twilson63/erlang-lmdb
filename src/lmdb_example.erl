%% @doc Example usage of LMDB NIF
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

%% @doc Basic key-value operations
basic_example() ->
    io:format("=== Basic LMDB Example ===~n"),
    
    % Create and configure environment
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760), % 10MB
    ok = lmdb:env_open(Env, "example_db", ?MDB_CREATE),
    
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
    {ok, <<"Bob Jones">>} = lmdb:with_ro_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        lmdb:get(Txn, Dbi, <<"user:bob">>)
    end),
    
    ok = lmdb:env_close(Env),
    io:format("Basic example completed successfully~n"),
    Value.

%% @doc Transaction rollback example
transaction_example() ->
    io:format("=== Transaction Example ===~n"),
    
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, "tx_example_db", ?MDB_CREATE),
    
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
    io:format("=== Batch Operations Example ===~n"),
    
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, "batch_example_db", ?MDB_CREATE),
    
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
    io:format("=== Iteration Example ===~n"),
    
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, "iter_example_db", ?MDB_CREATE),
    
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
    io:format("=== Multiple Databases Example ===~n"),
    
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_maxdbs(Env, 5), % Allow multiple databases
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, "multi_db_example", ?MDB_CREATE),
    
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

%% @doc Performance test example
performance_test() ->
    io:format("=== Performance Test ===~n"),
    
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 104857600), % 100MB
    ok = lmdb:env_open(Env, "perf_test_db", ?MDB_CREATE),
    
    NumRecords = 10000,
    
    % Time batch write
    {WriteTime, ok} = timer:tc(fun() ->
        {ok, Dbi} = lmdb:with_txn(Env, fun(Txn) ->
            lmdb:open_db(Txn, default)
        end),
        
        % Create batch operations
        Operations = lists:map(fun(N) ->
            Key = list_to_binary("key" ++ integer_to_list(N)),
            Value = list_to_binary("value" ++ integer_to_list(N) ++ 
                                   "_with_some_additional_data_to_make_it_longer"),
            {put, Dbi, Key, Value}
        end, lists:seq(1, NumRecords)),
        
        lmdb:write_batch(Env, Operations)
    end),
    
    WriteRate = NumRecords / (WriteTime / 1000000),
    io:format("Wrote ~p records in ~.2f seconds (~.0f records/sec)~n", 
              [NumRecords, WriteTime/1000000, WriteRate]),
    
    % Time random reads
    Keys = [list_to_binary("key" ++ integer_to_list(N)) || 
            N <- [rand:uniform(NumRecords) || _ <- lists:seq(1, 1000)]],
    
    {ReadTime, _} = timer:tc(fun() ->
        lmdb:with_ro_txn(Env, fun(Txn) ->
            {ok, Dbi} = lmdb:open_db(Txn, default),
            lists:foreach(fun(Key) ->
                lmdb:get(Txn, Dbi, Key)
            end, Keys)
        end)
    end),
    
    ReadRate = 1000 / (ReadTime / 1000000),
    io:format("Read 1000 random records in ~.2f seconds (~.0f reads/sec)~n", 
              [ReadTime/1000000, ReadRate]),
    
    % Get database statistics
    {ok, Stats} = lmdb:with_ro_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        lmdb_nif:dbi_stat(Txn, Dbi)
    end),
    
    io:format("Database statistics: ~p~n", [Stats]),
    
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
    os:cmd("rm -rf example_db tx_example_db batch_example_db iter_example_db multi_db_example perf_test_db"),
    io:format("All examples completed successfully!~n").
