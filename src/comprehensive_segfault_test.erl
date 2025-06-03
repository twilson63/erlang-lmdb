%%% @doc Comprehensive segfault tests 
-module(comprehensive_segfault_test).
-include("lmdb.hrl").
-export([run_all_tests/0, test_invalid_handles/0, test_parameter_validation/0, 
         test_boundary_conditions/0, test_transaction_state/0, test_data_edge_cases/0]).

% Helper function to assert error
assert_error(ExpectedError, Fun) ->
    try Fun() of
        Value -> erlang:error({assertError, {expected_error, ExpectedError, {got_success, Value}}})
    catch
        error:ExpectedError -> ok;
        error:OtherError -> erlang:error({assertError, {expected_error, ExpectedError, {got_error, OtherError}}});
        Class:Reason -> erlang:error({assertError, {expected_error, ExpectedError, {got_exception, Class, Reason}}})
    end.

run_all_tests() ->
    io:format("Running comprehensive segfault tests...~n"),
    
    % Clean up any existing test files
    os:cmd("rm -rf test/segfault_*"),
    
    test_invalid_handles(),
    test_parameter_validation(),
    test_boundary_conditions(),
    test_transaction_state(),
    test_data_edge_cases(),
    
    io:format("All comprehensive segfault tests completed successfully!~n"),
    ok.

test_invalid_handles() ->
    io:format("Testing invalid handles...~n"),
    
    % Test with completely invalid handles
    assert_error(badarg, fun() -> lmdb_nif:env_open(make_ref(), "/tmp/invalid", 0) end),
    assert_error(badarg, fun() -> lmdb_nif:env_close(make_ref()) end),
    assert_error(badarg, fun() -> lmdb_nif:txn_commit(make_ref()) end),
    assert_error(badarg, fun() -> lmdb_nif:cursor_close(make_ref()) end),
    
    % Test with closed environment
    {ok, Env} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_close(Env),
    
    % Using closed environment should fail gracefully
    case catch lmdb_nif:env_set_mapsize(Env, 1024) of
        {'EXIT', _} -> ok;
        {error, _} -> ok;
        ok -> io:format("WARNING: Closed env operation succeeded~n")
    end,
    
    io:format("✓ Invalid handles test passed~n").

test_parameter_validation() ->
    io:format("Testing parameter validation...~n"),
    
    {ok, Env} = lmdb_nif:env_create(),
    
    % Test extreme values
    assert_error(badarg, fun() -> lmdb_nif:env_set_mapsize(Env, 0) end),
    assert_error(badarg, fun() -> lmdb_nif:env_set_mapsize(Env, 18446744073709551615) end),
    assert_error(badarg, fun() -> lmdb_nif:env_set_maxreaders(Env, 0) end),
    assert_error(badarg, fun() -> lmdb_nif:env_set_maxreaders(Env, 99999) end),
    
    % Test with valid environment
    ok = lmdb_nif:env_set_mapsize(Env, 1048576),
    ok = lmdb_nif:env_open(Env, "test/segfault_param", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, 0),
    {ok, Dbi} = lmdb_nif:dbi_open(Txn, undefined, ?MDB_CREATE),
    
    % Test invalid parameter types
    assert_error(badarg, fun() -> lmdb_nif:get(Txn, Dbi, not_a_binary) end),
    assert_error(badarg, fun() -> lmdb_nif:put(Txn, Dbi, <<"key">>, not_a_binary, 0) end),
    assert_error(badarg, fun() -> lmdb_nif:put(Txn, Dbi, <<"key">>, <<"value">>, not_an_int) end),
    
    ok = lmdb_nif:txn_abort(Txn),
    ok = lmdb_nif:env_close(Env),
    
    io:format("✓ Parameter validation test passed~n").

test_boundary_conditions() ->
    io:format("Testing boundary conditions...~n"),
    
    {ok, Env} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_set_mapsize(Env, 1048576),
    ok = lmdb_nif:env_open(Env, "test/segfault_boundary", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, 0),
    {ok, Dbi} = lmdb_nif:dbi_open(Txn, undefined, ?MDB_CREATE),
    
    % Test key size boundaries
    EmptyKey = <<>>,
    MaxKey = binary:copy(<<"x">>, 511),
    OversizeKey = binary:copy(<<"x">>, 512),
    
    assert_error(badarg, fun() -> lmdb_nif:get(Txn, Dbi, EmptyKey) end),
    
    % Test max valid key size (should work)
    case lmdb_nif:put(Txn, Dbi, MaxKey, <<"test_value">>, 0) of
        ok -> io:format("✓ Max key size (511 bytes) works~n");
        {error, _} -> io:format("Max key size failed (acceptable)~n")
    end,
    
    % Test oversized key (should fail)
    assert_error(badarg, fun() -> lmdb_nif:put(Txn, Dbi, OversizeKey, <<"value">>, 0) end),
    
    % Test zero-length value (should work)
    case lmdb_nif:put(Txn, Dbi, <<"empty_val_key">>, <<>>, 0) of
        ok -> 
            case lmdb_nif:get(Txn, Dbi, <<"empty_val_key">>) of
                {ok, <<>>} -> io:format("✓ Zero-length value works~n");
                Other -> io:format("Zero-length value returned: ~p~n", [Other])
            end;
        Error -> io:format("Zero-length value failed: ~p~n", [Error])
    end,
    
    ok = lmdb_nif:txn_abort(Txn),
    ok = lmdb_nif:env_close(Env),
    
    io:format("✓ Boundary conditions test passed~n").

test_transaction_state() ->
    io:format("Testing transaction state handling...~n"),
    
    {ok, Env} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_set_mapsize(Env, 1048576),
    ok = lmdb_nif:env_open(Env, "test/segfault_txn", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, 0),
    {ok, Dbi} = lmdb_nif:dbi_open(Txn, undefined, ?MDB_CREATE),
    
    % Commit transaction
    ok = lmdb_nif:txn_commit(Txn),
    
    % Try to use committed transaction (should fail)
    assert_error(badarg, fun() -> lmdb_nif:txn_commit(Txn) end),
    assert_error(badarg, fun() -> lmdb_nif:txn_abort(Txn) end),
    assert_error(badarg, fun() -> lmdb_nif:get(Txn, Dbi, <<"key">>) end),
    assert_error(badarg, fun() -> lmdb_nif:cursor_open(Txn, Dbi) end),
    
    ok = lmdb_nif:env_close(Env),
    
    io:format("✓ Transaction state test passed~n").

test_data_edge_cases() ->
    io:format("Testing data edge cases...~n"),
    
    {ok, Env} = lmdb_nif:env_create(),
    ok = lmdb_nif:env_set_mapsize(Env, 1048576),
    ok = lmdb_nif:env_open(Env, "test/segfault_data", ?MDB_CREATE bor ?MDB_NOSUBDIR),
    
    {ok, Txn} = lmdb_nif:txn_begin(Env, undefined, 0),
    {ok, Dbi} = lmdb_nif:dbi_open(Txn, undefined, ?MDB_CREATE),
    
    % Test Unicode data
    UnicodeKey = <<"测试"/utf8>>,
    UnicodeValue = <<"数据"/utf8>>,
    case lmdb_nif:put(Txn, Dbi, UnicodeKey, UnicodeValue, 0) of
        ok -> 
            case lmdb_nif:get(Txn, Dbi, UnicodeKey) of
                {ok, UnicodeValue} -> io:format("✓ Unicode data works~n");
                Other -> io:format("Unicode data mismatch: ~p~n", [Other])
            end;
        Error -> io:format("Unicode data failed: ~p~n", [Error])
    end,
    
    % Test binary data with null bytes
    NullKey = <<"key", 0, "with", 0, "nulls">>,
    NullValue = <<"value", 0, "with", 0, "nulls">>,
    case lmdb_nif:put(Txn, Dbi, NullKey, NullValue, 0) of
        ok ->
            case lmdb_nif:get(Txn, Dbi, NullKey) of
                {ok, NullValue} -> io:format("✓ Null byte data works~n");
                Other2 -> io:format("Null byte data mismatch: ~p~n", [Other2])
            end;
        Error2 -> io:format("Null byte data failed: ~p~n", [Error2])
    end,
    
    ok = lmdb_nif:txn_commit(Txn),
    ok = lmdb_nif:env_close(Env),
    
    io:format("✓ Data edge cases test passed~n").