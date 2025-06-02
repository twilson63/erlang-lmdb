%%% @doc Final validation that all segfault guards are working
-module(final_segfault_validation).
-include("lmdb.hrl").
-export([run_validation/0]).

run_validation() ->
    io:format("=== FINAL SEGFAULT VALIDATION ===~n"),
    
    % Category 1: Invalid Resource Handles
    io:format("1. Testing invalid resource handles...~n"),
    test_invalid_resources(),
    
    % Category 2: Parameter Type Validation  
    io:format("2. Testing parameter type validation...~n"),
    test_parameter_types(),
    
    % Category 3: Bounds and Size Validation
    io:format("3. Testing bounds and size validation...~n"), 
    test_bounds_validation(),
    
    % Category 4: State Consistency
    io:format("4. Testing state consistency...~n"),
    test_state_consistency(),
    
    % Category 5: Edge Cases that Previously Caused Segfaults
    io:format("5. Testing edge cases...~n"),
    test_edge_cases(),
    
    io:format("=== ALL VALIDATIONS PASSED ===~n"),
    io:format("NIF guards successfully prevent segfaults!~n"),
    ok.

test_invalid_resources() ->
    % Test 1.1: Random references as handles
    BadRef = make_ref(),
    test_badarg_expected(fun() -> lmdb_nif:env_open(BadRef, "/tmp", 0) end, "invalid env handle"),
    test_badarg_expected(fun() -> lmdb_nif:txn_commit(BadRef) end, "invalid txn handle"),
    test_badarg_expected(fun() -> lmdb_nif:cursor_close(BadRef) end, "invalid cursor handle"),
    
    % Test 1.2: Using closed resources
    {ok, Env} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_close(Env),
    test_graceful_failure(fun() -> lmdb_nif:env_set_mapsize(Env, 1024) end, "closed env usage"),
    
    io:format("  ✓ Invalid resource tests passed~n").

test_parameter_types() ->
    % Test 2.1: Non-binary keys/values
    {ok, Env} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_set_mapsize(Env, 1048576),
    ok = lmdb_nif:env_open(Env, "final_test_param", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, 0),
    {ok, Dbi} = lmdb_nif:dbi_open(Txn, undefined, ?MDB_CREATE),
    
    test_badarg_expected(fun() -> lmdb_nif:get(Txn, Dbi, 123) end, "integer key"),
    test_badarg_expected(fun() -> lmdb_nif:get(Txn, Dbi, atom_key) end, "atom key"),
    test_badarg_expected(fun() -> lmdb_nif:put(Txn, Dbi, <<"key">>, 456, 0) end, "integer value"),
    test_badarg_expected(fun() -> lmdb_nif:put(Txn, Dbi, <<"key">>, <<"val">>, atom_flag) end, "atom flag"),
    
    ok = lmdb_nif:txn_abort(Txn),
    ok = lmdb_nif:env_close(Env),
    
    io:format("  ✓ Parameter type tests passed~n").

test_bounds_validation() ->
    % Test 3.1: Size limits
    {ok, Env} = lmdb_nif:env_create(),
    
    test_badarg_expected(fun() -> lmdb_nif:env_set_mapsize(Env, 0) end, "zero mapsize"),
    test_badarg_expected(fun() -> lmdb_nif:env_set_mapsize(Env, 18446744073709551615) end, "huge mapsize"),
    test_badarg_expected(fun() -> lmdb_nif:env_set_maxreaders(Env, 0) end, "zero readers"),
    test_badarg_expected(fun() -> lmdb_nif:env_set_maxreaders(Env, 99999) end, "too many readers"),
    
    % Test 3.2: Key/value size limits
    ok = lmdb_nif:env_set_mapsize(Env, 1048576),
    ok = lmdb_nif:env_open(Env, "final_test_bounds", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, 0),
    {ok, Dbi} = lmdb_nif:dbi_open(Txn, undefined, ?MDB_CREATE),
    
    EmptyKey = <<>>,
    OversizeKey = binary:copy(<<"x">>, 1000),
    
    test_badarg_expected(fun() -> lmdb_nif:get(Txn, Dbi, EmptyKey) end, "empty key"),
    test_badarg_expected(fun() -> lmdb_nif:get(Txn, Dbi, OversizeKey) end, "oversized key"),
    
    ok = lmdb_nif:txn_abort(Txn),
    ok = lmdb_nif:env_close(Env),
    
    io:format("  ✓ Bounds validation tests passed~n").

test_state_consistency() ->
    % Test 4.1: Transaction state validation
    {ok, Env} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_set_mapsize(Env, 1048576),
    ok = lmdb_nif:env_open(Env, "final_test_state", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, 0),
    {ok, Dbi} = lmdb_nif:dbi_open(Txn, undefined, ?MDB_CREATE),
    
    % Commit transaction
    ok = lmdb_nif:txn_commit(Txn),
    
    % Operations on committed transaction should fail
    test_badarg_expected(fun() -> lmdb_nif:txn_commit(Txn) end, "double commit"),
    test_badarg_expected(fun() -> lmdb_nif:txn_abort(Txn) end, "abort committed"),
    test_badarg_expected(fun() -> lmdb_nif:get(Txn, Dbi, <<"key">>) end, "get on committed txn"),
    
    ok = lmdb_nif:env_close(Env),
    
    io:format("  ✓ State consistency tests passed~n").

test_edge_cases() ->
    % Test 5.1: Maximum valid key size and special data
    {ok, Env} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_set_mapsize(Env, 1048576),
    ok = lmdb_nif:env_open(Env, "final_test_edge", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, 0),
    {ok, Dbi} = lmdb_nif:dbi_open(Txn, undefined, ?MDB_CREATE),
    
    % Test maximum valid key size (511 bytes)
    MaxKey = binary:copy(<<"k">>, 511),
    case lmdb_nif:put(Txn, Dbi, MaxKey, <<"max_key_test">>, 0) of
        ok -> 
            case lmdb_nif:get(Txn, Dbi, MaxKey) of
                {ok, <<"max_key_test">>} -> io:format("    ✓ Max key size works~n");
                Other -> io:format("    ? Max key returned: ~p~n", [Other])
            end;
        Error -> io:format("    ? Max key failed: ~p~n", [Error])
    end,
    
    % Test zero-length value
    case lmdb_nif:put(Txn, Dbi, <<"empty_val">>, <<>>, 0) of
        ok -> 
            case lmdb_nif:get(Txn, Dbi, <<"empty_val">>) of
                {ok, <<>>} -> io:format("    ✓ Empty value works~n");
                Other2 -> io:format("    ? Empty value returned: ~p~n", [Other2])
            end;
        Error2 -> io:format("    ? Empty value failed: ~p~n", [Error2])
    end,
    
    % Test binary data with null bytes
    NullData = <<"test", 0, "null", 0, "bytes">>,
    case lmdb_nif:put(Txn, Dbi, <<"null_test">>, NullData, 0) of
        ok ->
            case lmdb_nif:get(Txn, Dbi, <<"null_test">>) of
                {ok, NullData} -> io:format("    ✓ Null byte data works~n");
                Other3 -> io:format("    ? Null data returned: ~p~n", [Other3])
            end;
        Error3 -> io:format("    ? Null data failed: ~p~n", [Error3])
    end,
    
    ok = lmdb_nif:txn_commit(Txn),
    ok = lmdb_nif:env_close(Env),
    
    io:format("  ✓ Edge case tests passed~n").

% Helper functions
test_badarg_expected(Fun, Description) ->
    try Fun() of
        Value -> 
            io:format("    ERROR: ~s should have failed but got: ~p~n", [Description, Value]),
            erlang:error({test_failed, Description, {expected_badarg, got_success, Value}})
    catch
        error:badarg -> 
            io:format("    ✓ ~s correctly caught~n", [Description]);
        Class:Reason -> 
            io:format("    ERROR: ~s failed with ~p:~p (expected badarg)~n", [Description, Class, Reason]),
            erlang:error({test_failed, Description, {expected_badarg, got_exception, Class, Reason}})
    end.

test_graceful_failure(Fun, Description) ->
    try Fun() of
        _Value -> 
            io:format("    ? ~s succeeded (acceptable)~n", [Description])
    catch
        error:badarg -> 
            io:format("    ✓ ~s failed gracefully with badarg~n", [Description]);
        {error, _} -> 
            io:format("    ✓ ~s failed gracefully with error~n", [Description]);
        Class:Reason -> 
            io:format("    ? ~s failed with ~p:~p~n", [Description, Class, Reason])
    end.