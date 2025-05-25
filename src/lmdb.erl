%% @doc High-level LMDB wrapper providing a more convenient API
-module(lmdb).

-export([
    % Environment management
    env_create/0,
    env_open/2,
    env_open/3,
    env_close/1,
    env_set_maxreaders/2,
    env_set_maxdbs/2,
    env_set_mapsize/2,
    env_sync/1,
    env_sync/2,
    
    % Database operations
    open_db/2,
    open_db/3,
    close_db/2,
    
    % Simple key-value operations
    get/3,
    put/4,
    put/5,
    delete/3,
    delete/4,
    
    % Transaction operations
    with_txn/2,
    with_txn/3,
    with_ro_txn/2,
    
    % Batch operations
    write_batch/2,
    
    % Iteration
    fold/4,
    foreach/3,
    
    % Utilities
    exists/3,
    count/2
]).

-include("lmdb.hrl").

%% @doc Create a new LMDB environment
-spec env_create() -> {ok, lmdb_env()} | {error, term()}.
env_create() ->
    lmdb_nif:env_create().

%% @doc Open an LMDB environment with default flags
-spec env_open(lmdb_env(), string()) -> ok | {error, term()}.
env_open(Env, Path) ->
    env_open(Env, Path, 0).

%% @doc Open an LMDB environment with specified flags
-spec env_open(lmdb_env(), string(), non_neg_integer()) -> ok | {error, term()}.
env_open(Env, Path, Flags) ->
    lmdb_nif:env_open(Env, Path, Flags).

%% @doc Close an LMDB environment
-spec env_close(lmdb_env()) -> ok.
env_close(Env) ->
    lmdb_nif:env_close(Env).

%% @doc Set maximum number of reader threads
-spec env_set_maxreaders(lmdb_env(), pos_integer()) -> ok | {error, term()}.
env_set_maxreaders(Env, Readers) ->
    lmdb_nif:env_set_maxreaders(Env, Readers).

%% @doc Set maximum number of databases
-spec env_set_maxdbs(lmdb_env(), pos_integer()) -> ok | {error, term()}.
env_set_maxdbs(Env, Dbs) ->
    lmdb_nif:env_set_maxdbs(Env, Dbs).

%% @doc Set memory map size for the environment
-spec env_set_mapsize(lmdb_env(), pos_integer()) -> ok | {error, term()}.
env_set_mapsize(Env, Size) ->
    lmdb_nif:env_set_mapsize(Env, Size).

%% @doc Sync environment to disk (force=false)
-spec env_sync(lmdb_env()) -> ok | {error, term()}.
env_sync(Env) ->
    env_sync(Env, false).

%% @doc Sync environment to disk with force option
-spec env_sync(lmdb_env(), boolean()) -> ok | {error, term()}.
env_sync(Env, Force) ->
    lmdb_nif:env_sync(Env, case Force of true -> 1; false -> 0 end).

%% @doc Open a database with default flags
-spec open_db(lmdb_txn(), atom() | string()) -> {ok, lmdb_dbi()} | {error, term()}.
open_db(Txn, Name) ->
    open_db(Txn, Name, ?MDB_CREATE).

%% @doc Open a database with specified flags
-spec open_db(lmdb_txn(), atom() | string(), non_neg_integer()) -> {ok, lmdb_dbi()} | {error, term()}.
open_db(Txn, Name, Flags) ->
    DbName = case Name of
        default -> undefined;
        undefined -> undefined;
        _ when is_atom(Name) -> atom_to_list(Name);
        _ -> Name
    end,
    lmdb_nif:dbi_open(Txn, DbName, Flags).

%% @doc Close a database
-spec close_db(lmdb_env(), lmdb_dbi()) -> ok.
close_db(Env, Dbi) ->
    lmdb_nif:dbi_close(Env, Dbi).

%% @doc Get a value by key
-spec get(lmdb_txn(), lmdb_dbi(), binary()) -> {ok, binary()} | not_found | {error, term()}.
get(Txn, Dbi, Key) when is_binary(Key) ->
    lmdb_nif:get(Txn, Dbi, Key);
get(Txn, Dbi, Key) ->
    get(Txn, Dbi, term_to_binary(Key)).

%% @doc Put a key-value pair with default flags
-spec put(lmdb_txn(), lmdb_dbi(), binary(), binary()) -> ok | {error, term()}.
put(Txn, Dbi, Key, Value) ->
    put(Txn, Dbi, Key, Value, 0).

%% @doc Put a key-value pair with specified flags
-spec put(lmdb_txn(), lmdb_dbi(), binary(), binary(), non_neg_integer()) -> ok | {error, term()}.
put(Txn, Dbi, Key, Value, Flags) when is_binary(Key), is_binary(Value) ->
    lmdb_nif:put(Txn, Dbi, Key, Value, Flags);
put(Txn, Dbi, Key, Value, Flags) ->
    BinKey = case is_binary(Key) of true -> Key; false -> term_to_binary(Key) end,
    BinValue = case is_binary(Value) of true -> Value; false -> term_to_binary(Value) end,
    put(Txn, Dbi, BinKey, BinValue, Flags).

%% @doc Delete a key
-spec delete(lmdb_txn(), lmdb_dbi(), binary()) -> ok | {error, term()}.
delete(Txn, Dbi, Key) when is_binary(Key) ->
    lmdb_nif:del(Txn, Dbi, Key);
delete(Txn, Dbi, Key) ->
    delete(Txn, Dbi, term_to_binary(Key)).

%% @doc Delete a specific key-value pair
-spec delete(lmdb_txn(), lmdb_dbi(), binary(), binary()) -> ok | {error, term()}.
delete(Txn, Dbi, Key, Value) when is_binary(Key), is_binary(Value) ->
    lmdb_nif:del(Txn, Dbi, Key, Value);
delete(Txn, Dbi, Key, Value) ->
    BinKey = case is_binary(Key) of true -> Key; false -> term_to_binary(Key) end,
    BinValue = case is_binary(Value) of true -> Value; false -> term_to_binary(Value) end,
    delete(Txn, Dbi, BinKey, BinValue).

%% @doc Execute a function within a read-write transaction
-spec with_txn(lmdb_env(), fun((lmdb_txn()) -> Result)) -> {ok, Result} | {error, term()}.
with_txn(Env, Fun) ->
    with_txn(Env, Fun, 0).

%% @doc Execute a function within a transaction with specified flags
-spec with_txn(lmdb_env(), fun((lmdb_txn()) -> Result), non_neg_integer()) -> {ok, Result} | {error, term()}.
with_txn(Env, Fun, Flags) ->
    case lmdb_nif:txn_begin(Env, undefined, Flags) of
        {ok, Txn} ->
            try
                Result = Fun(Txn),
                case lmdb_nif:txn_commit(Txn) of
                    ok -> {ok, Result};
                    Error -> Error
                end
            catch
                _:Reason ->
                    lmdb_nif:txn_abort(Txn),
                    {error, Reason}
            end;
        Error ->
            Error
    end.

%% @doc Execute a function within a read-only transaction
-spec with_ro_txn(lmdb_env(), fun((lmdb_txn()) -> Result)) -> {ok, Result} | {error, term()}.
with_ro_txn(Env, Fun) ->
    with_txn(Env, Fun, ?MDB_RDONLY).

%% @doc Execute a batch of write operations in a single transaction
-spec write_batch(lmdb_env(), [{put | delete, lmdb_dbi(), term(), term()} | {delete, lmdb_dbi(), term()}]) -> 
    {ok, ok} | {error, term()}.
write_batch(Env, Operations) ->
    with_txn(Env, fun(Txn) ->
        lists:foreach(fun(Op) ->
            case Op of
                {put, Dbi, Key, Value} ->
                    case put(Txn, Dbi, Key, Value) of
                        ok -> ok;
                        Error -> throw(Error)
                    end;
                {delete, Dbi, Key} ->
                    case delete(Txn, Dbi, Key) of
                        ok -> ok;
                        Error -> throw(Error)
                    end;
                {delete, Dbi, Key, Value} ->
                    case delete(Txn, Dbi, Key, Value) of
                        ok -> ok;
                        Error -> throw(Error)
                    end
            end
        end, Operations),
        ok
    end).

%% @doc Fold over all key-value pairs in a database
-spec fold(lmdb_env(), lmdb_dbi(), fun((binary(), binary(), Acc) -> Acc), Acc) -> {ok, Acc} | {error, term()}.
fold(Env, Dbi, Fun, Acc0) ->
    with_ro_txn(Env, fun(Txn) ->
        case lmdb_nif:cursor_open(Txn, Dbi) of
            {ok, Cursor} ->
                try
                    fold_cursor(Cursor, Fun, Acc0, first)
                after
                    lmdb_nif:cursor_close(Cursor)
                end;
            Error ->
                throw(Error)
        end
    end).

%% @doc Execute a function for each key-value pair in a database
-spec foreach(lmdb_env(), lmdb_dbi(), fun((binary(), binary()) -> any())) -> ok | {error, term()}.
foreach(Env, Dbi, Fun) ->
    case fold(Env, Dbi, fun(Key, Value, _) -> Fun(Key, Value) end, ok) of
        {ok, _} -> ok;
        Error -> Error
    end.

%% @doc Check if a key exists in the database
-spec exists(lmdb_txn(), lmdb_dbi(), binary()) -> boolean() | {error, term()}.
exists(Txn, Dbi, Key) ->
    case get(Txn, Dbi, Key) of
        {ok, _} -> true;
        not_found -> false;
        Error = {error, _} -> Error
    end.

%% @doc Count the number of entries in a database
-spec count(lmdb_env(), lmdb_dbi()) -> {ok, non_neg_integer()} | {error, term()}.
count(Env, Dbi) ->
    with_ro_txn(Env, fun(Txn) ->
        case lmdb_nif:dbi_stat(Txn, Dbi) of
            {ok, Stats} ->
                proplists:get_value(entries, Stats, 0);
            Error ->
                throw(Error)
        end
    end).

%% Internal helper functions

%% @private
fold_cursor(Cursor, Fun, Acc, Op) ->
    case lmdb_nif:cursor_get(Cursor, <<>>, cursor_op_to_int(Op)) of
        {ok, Key, Value} ->
            NewAcc = Fun(Key, Value, Acc),
            fold_cursor(Cursor, Fun, NewAcc, next);
        not_found ->
            Acc;
        Error ->
            throw(Error)
    end.

%% @private
cursor_op_to_int(first) -> 0;
cursor_op_to_int(next) -> 8;
cursor_op_to_int(prev) -> 12;
cursor_op_to_int(last) -> 4.
