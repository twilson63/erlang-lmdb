%%% @doc Simple segfault tests to verify guards work
-module(simple_segfault_test).
-include("lmdb.hrl").
-export([run_tests/0]).

run_tests() ->
    io:format("Running simple segfault tests...~n"),
    
    % Test 1: Invalid handles should throw badarg, not segfault
    try lmdb_nif:env_open(make_ref(), "/tmp/invalid", 0) of
        _ -> io:format("ERROR: Should have failed~n")
    catch
        error:badarg -> io:format("✓ Invalid env handle caught~n");
        E1:R1 -> io:format("? Got ~p:~p~n", [E1, R1])
    end,
    
    % Test 2: Wrong parameter types should throw badarg
    try lmdb_nif:env_set_mapsize(make_ref(), "not_an_int") of
        _ -> io:format("ERROR: Should have failed~n")
    catch
        error:badarg -> io:format("✓ Invalid parameter type caught~n");
        E2:R2 -> io:format("? Got ~p:~p~n", [E2, R2])
    end,
    
    % Test 3: Extreme values should be caught
    {ok, Env} = lmdb_nif:env_create(),
    try lmdb_nif:env_set_mapsize(Env, 0) of
        _ -> io:format("ERROR: Should have failed~n")
    catch
        error:badarg -> io:format("✓ Zero mapsize caught~n");
        E3:R3 -> io:format("? Got ~p:~p~n", [E3, R3])
    end,
    
    % Test 4: Check committed transaction abort
    ok = lmdb_nif:env_set_mapsize(Env, 1048576),
    ok = lmdb_nif:env_open(Env, "simple_test", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, 0),
    ok = lmdb_nif:txn_commit(Txn),
    
    try lmdb_nif:txn_abort(Txn) of
        ok -> io:format("WARNING: Should have failed but didn't~n");
        Other4 -> io:format("? Got ~p~n", [Other4])
    catch
        error:badarg -> io:format("✓ Committed txn abort caught~n");
        E4:R4 -> io:format("? Got ~p:~p~n", [E4, R4])
    end,
    
    % Test 5: Operations on committed transaction
    try lmdb_nif:txn_commit(Txn) of
        _ -> io:format("WARNING: Double commit should fail~n")
    catch
        error:badarg -> io:format("✓ Double commit caught~n");
        E5:R5 -> io:format("? Got ~p:~p~n", [E5, R5])
    end,
    
    ok = lmdb_nif:env_close(Env),
    
    % Test 6: Invalid binary parameters  
    {ok, Env2} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_set_mapsize(Env2, 1048576),
    ok = lmdb_nif:env_open(Env2, "simple_test2", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    {ok, Txn2} = lmdb_nif:txn_begin(Env2, undefined, 0),
    {ok, Dbi} = lmdb_nif:dbi_open(Txn2, undefined, ?MDB_CREATE),
    
    try lmdb_nif:get(Txn2, Dbi, not_a_binary) of
        _ -> io:format("ERROR: Should have failed~n")
    catch
        error:badarg -> io:format("✓ Non-binary key caught~n");
        E6:R6 -> io:format("? Got ~p:~p~n", [E6, R6])
    end,
    
    try lmdb_nif:put(Txn2, Dbi, <<"key">>, not_a_binary, 0) of
        _ -> io:format("ERROR: Should have failed~n")  
    catch
        error:badarg -> io:format("✓ Non-binary value caught~n");
        E7:R7 -> io:format("? Got ~p:~p~n", [E7, R7])
    end,
    
    % Test 7: Key size limits
    EmptyKey = <<>>,
    HugeKey = binary:copy(<<"x">>, 1000),
    
    try lmdb_nif:get(Txn2, Dbi, EmptyKey) of
        _ -> io:format("ERROR: Empty key should fail~n")
    catch
        error:badarg -> io:format("✓ Empty key caught~n");
        E8:R8 -> io:format("? Got ~p:~p~n", [E8, R8])
    end,
    
    try lmdb_nif:get(Txn2, Dbi, HugeKey) of
        _ -> io:format("ERROR: Huge key should fail~n")
    catch
        error:badarg -> io:format("✓ Huge key caught~n");
        E9:R9 -> io:format("? Got ~p:~p~n", [E9, R9])
    end,
    
    ok = lmdb_nif:txn_abort(Txn2),
    ok = lmdb_nif:env_close(Env2),
    
    io:format("Simple segfault tests completed~n"),
    ok.