%%% @doc Unit tests for LMDB NIF
-module(lmdb_nif_tests).
-include_lib("eunit/include/eunit.hrl").
-include("lmdb.hrl").
-define(TEST_DB_PATH, "test_lmdb").

%% Test fixtures
lmdb_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_env_create_and_open/0,
         fun test_erlang_api/0,
         fun test_basic_operations/0,
         fun test_transactions/0,
         fun test_batch_operations/0,
         fun test_iteration/0,
         fun test_error_handling/0
     ]}.

setup() ->
    % Clean up any existing test databases
    os:cmd("rm -rf " ++ ?TEST_DB_PATH ++ "*"),
    % Create the test directory
    ok = file:make_dir("test_lmdb_dir"),
    ok.

cleanup(_) ->
    % Clean up all test databases and directory
    os:cmd("rm -rf " ++ ?TEST_DB_PATH ++ "*"),
    os:cmd("rm -rf test_lmdb_dir").

test_erlang_api() ->
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_open(Env, ?TEST_DB_PATH ++ "_erlang_api", [nosubdir, create]),
    ok = lmdb:put(Env, <<"key1">>, <<"value1">>),
    {ok, <<"value1">>} = lmdb:get(Env, <<"key1">>),
    ok = lmdb:env_close(Env).

%% Test environment creation and opening
test_env_create_and_open() ->
    {ok, Env} = lmdb:env_create(),
    ?assert(is_reference(Env)),
    
    %% Set some environment parameters
    ok = lmdb:env_set_maxdbs(Env, 10),
    ok = lmdb:env_set_mapsize(Env, 10485760), % 10MB
    
    %% Open the environment with NOSUBDIR flag for simpler setup
    ok = lmdb:env_open(Env, ?TEST_DB_PATH, ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    %% Close the environment
    ok = lmdb:env_close(Env).

%% Test basic put/get/delete operations
test_basic_operations() ->
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, ?TEST_DB_PATH ++ "_basic", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    {ok, Result} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        
        %% Test put and get
        ok = lmdb:put(Txn, Dbi, <<"key1">>, <<"value1">>),
        {ok, <<"value1">>} = lmdb:get(Txn, Dbi, <<"key1">>),
        
        %% Test overwrite
        ok = lmdb:put(Txn, Dbi, <<"key1">>, <<"value2">>),
        {ok, <<"value2">>} = lmdb:get(Txn, Dbi, <<"key1">>),
        
        %% Test not found
        not_found = lmdb:get(Txn, Dbi, <<"nonexistent">>),
        
        %% Test delete
        ok = lmdb:delete(Txn, Dbi, <<"key1">>),
        not_found = lmdb:get(Txn, Dbi, <<"key1">>),
        
        success
    end),
    
    ?assertEqual(success, Result),
    ok = lmdb:env_close(Env).

%% Test transaction handling
test_transactions() ->
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, ?TEST_DB_PATH ++ "_tx", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    %% Test successful transaction
    {ok, success} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        ok = lmdb:put(Txn, Dbi, <<"txn_key">>, <<"txn_value">>),
        success
    end),
    
    %% Verify data was committed - fix the double wrapping issue
    {ok, Value} = lmdb:with_ro_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        case lmdb:get(Txn, Dbi, <<"txn_key">>) of
            {ok, V} -> V;
            Other -> Other
        end
    end),
    ?assertEqual(<<"txn_value">>, Value),
    
    %% Test transaction rollback on exception
    {error, test_error} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        ok = lmdb:put(Txn, Dbi, <<"rollback_key">>, <<"rollback_value">>),
        throw(test_error)
    end),
    
    %% Verify data was not committed
    {ok, Result} = lmdb:with_ro_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        case lmdb:get(Txn, Dbi, <<"rollback_key">>) of
            not_found -> not_found;
            Other -> Other
        end
    end),
    ?assertEqual(not_found, Result),
    
    ok = lmdb:env_close(Env).

%% Test batch operations
test_batch_operations() ->
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, ?TEST_DB_PATH ++ "_batch", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    %% Test batch write - don't reuse Dbi across transactions
    Operations = [
        {put, <<"batch1">>, <<"value1">>},
        {put, <<"batch2">>, <<"value2">>},
        {put, <<"batch3">>, <<"value3">>}
    ],
    
    %% Write batch data
    {ok, _} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        lists:foreach(fun({put, Key, Value}) ->
            ok = lmdb:put(Txn, Dbi, Key, Value)
        end, Operations),
        ok
    end),
    
    %% Verify results
    {ok, Values} = lmdb:with_ro_txn(Env, fun(Txn) ->
        {ok, ReadDbi} = lmdb:open_db(Txn, default),
        [
            lmdb:get(Txn, ReadDbi, <<"batch1">>),
            lmdb:get(Txn, ReadDbi, <<"batch2">>),
            lmdb:get(Txn, ReadDbi, <<"batch3">>)
        ]
    end),
    
    ?assertEqual([{ok, <<"value1">>}, {ok, <<"value2">>}, {ok, <<"value3">>}], Values),
    
    ok = lmdb:env_close(Env).

%% Test iteration over database
test_iteration() ->
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, ?TEST_DB_PATH ++ "_iter", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    %% Setup test data
    {ok, Dbi} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        ok = lmdb:put(Txn, Dbi, <<"key1">>, <<"value1">>),
        ok = lmdb:put(Txn, Dbi, <<"key2">>, <<"value2">>),
        ok = lmdb:put(Txn, Dbi, <<"key3">>, <<"value3">>),
        Dbi
    end),
    
    %% Test fold
    {ok, Keys} = lmdb:fold(Env, default, fun(Key, _Value, Acc) ->
        [Key | Acc]
    end, []),
    
    ?assertEqual(3, length(Keys)),
    ?assert(lists:member(<<"key1">>, Keys)),
    ?assert(lists:member(<<"key2">>, Keys)),
    ?assert(lists:member(<<"key3">>, Keys)),
    
    %% Test count
    {ok, 3} = lmdb:count(Env, Dbi),
    
    ok = lmdb:env_close(Env).

%% Test error handling
test_error_handling() ->
    {ok, Env} = lmdb:env_create(),
    ok = lmdb:env_set_mapsize(Env, 10485760),
    ok = lmdb:env_open(Env, ?TEST_DB_PATH ++ "_error", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    %% Test duplicate key with NOOVERWRITE flag
    {ok, Result} = lmdb:with_txn(Env, fun(Txn) ->
        {ok, Dbi} = lmdb:open_db(Txn, default),
        ok = lmdb:put(Txn, Dbi, <<"dup_key">>, <<"value1">>),
        
        %% This should fail
        case lmdb:put(Txn, Dbi, <<"dup_key">>, <<"value2">>, ?MDB_NOOVERWRITE) of
            {error, _} -> duplicate_error_caught;
            _ -> unexpected_success
        end
    end),
    
    ?assertEqual(duplicate_error_caught, Result),
    
    ok = lmdb:env_close(Env).
