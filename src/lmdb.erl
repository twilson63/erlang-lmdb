%% @doc High-level LMDB wrapper providing a more convenient API
-module(lmdb).

-export([
    % Environment management
    env_create/0,
    env_create/1,
    env_create/2,
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
    get/2,
    get/3,
    put/3,
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

-define(IS_ATOM_OR_STRING(X), (is_atom(X) orelse is_binary(X) orelse is_list(X))).
-define(IS_NON_NEG_INT(X), (is_integer(X) andalso X >= 0)).

%% @doc Create a new LMDB environment
-spec env_create() -> {ok, lmdb_env()} | {error, term()}.
env_create() ->
    lmdb_nif:env_create().

-spec env_create(string() | binary()) -> {ok, lmdb_env()} | {error, term()}.
env_create(Path) ->
    env_create(Path, ?DEFAULT_MAPSIZE).

-spec env_create(string() | binary(), integer() | map()) -> {ok, lmdb_env()} | {error, term()}.
env_create(Path, Size) when is_integer(Size) ->
    env_create(Path, #{ max_mapsize => Size });
env_create(Path, Opts) when is_map(Opts) ->
    maybe
        {ok, Env} ?= env_create(maps:get(flags, Opts, [create])),
        ok ?= env_set_mapsize(Env, maps:get(max_mapsize, Opts, ?DEFAULT_MAPSIZE)),
        ok ?= env_set_maxreaders(Env, maps:get(max_readers, Opts, ?DEFAULT_MAXREADERS)),
        ok ?= env_set_maxdbs(Env, maps:get(max_dbs, Opts, ?DEFAULT_MAXDBS)),
        ok ?= env_open(Env, Path),
        {ok, Env}
    end;
env_create(_Path, _) ->
  {error, {badarg, "Path should be map"}}.

%% @doc Open an LMDB environment with default flags
-spec env_open(lmdb_env(), string()) -> ok | {error, term()}.
env_open(Env, Path) ->
    env_open(Env, Path, 0).

%% @doc Open an LMDB environment with specified flags
-spec env_open(lmdb_env(), string(), non_neg_integer()) -> ok | {error, term()}.
env_open(Env, Path, Flags) when is_reference(Env), is_binary(Path) ->
    env_open(Env, binary_to_list(Path), Flags);
env_open(Env, Path, Flags) when is_reference(Env), is_list(Flags) ->
    env_open(Env, Path, merge_flags(Flags));
env_open(Env, Path, Flags) when is_reference(Env) ->
    case Flags band ?MDB_NOSUBDIR of
        0 -> filelib:ensure_dir(Path ++ "/data.mdb");
        _ -> ok
    end,
    lmdb_nif:env_open(Env, Path, Flags);
env_open(_, _, _) ->
   {error, {badarg, "Env should be reference."}}.

%% @doc Close an LMDB environment
-spec env_close(lmdb_env()) -> ok.
env_close(Env) when is_reference(Env) ->
    lmdb_nif:env_close(Env);
env_close(_) -> 
   {error, {badarg, "Env must be a reference"}}.

%% @doc Set maximum number of reader threads
-spec env_set_maxreaders(lmdb_env(), pos_integer()) -> ok | {error, term()}.
env_set_maxreaders(Env, Readers) when is_reference(Env), is_integer(Readers) ->
    lmdb_nif:env_set_maxreaders(Env, Readers);
env_set_maxreaders(_, _) ->
   {error, {badarg, "Readers must be a number and Env must be a reference."}}.

%% @doc Set maximum number of databases
-spec env_set_maxdbs(lmdb_env(), pos_integer()) -> ok | {error, term()}.
env_set_maxdbs(Env, Dbs) when is_reference(Env), is_integer(Dbs) ->
    lmdb_nif:env_set_maxdbs(Env, Dbs);
env_set_maxdbs(_, _) ->
    {error, {badarg, "Dbs must be a number and Env must be a reference"}}.

%% @doc Set memory map size for the environment
-spec env_set_mapsize(lmdb_env(), pos_integer()) -> ok | {error, term()}.
env_set_mapsize(Env, Size) when is_reference(Env), is_integer(Size) ->
    lmdb_nif:env_set_mapsize(Env, Size);
env_set_mapsize(_, _) ->
    {error, {badarg, "Size must be a number and Env must be a reference"}}.

%% @doc Sync environment to disk (force=false)
-spec env_sync(lmdb_env()) -> ok | {error, term()}.
env_sync(Env) when is_reference(Env) ->
    env_sync(Env, false);
env_sync(_) ->
    {error, {badarg, "Env must be a reference"}}.

%% @doc Sync environment to disk with force option
-spec env_sync(lmdb_env(), boolean()) -> ok | {error, term()}.
env_sync(Env, Force) when is_reference(Env), is_boolean(Force) ->
    lmdb_nif:env_sync(Env, case Force of true -> 1; false -> 0 end);
env_sync(_, _) ->
    {error, {badarg, "Force must be boolean and Env must be a reference"}}.

%% @doc Open a database with default flags
-spec open_db(lmdb_txn(), atom() | string()) -> {ok, lmdb_dbi()} | {error, term()}.
open_db(Txn, Name) when is_reference(Txn), ?IS_ATOM_OR_STRING(Name) ->
    open_db(Txn, Name, ?MDB_CREATE);
open_db(_, _) ->
    {error, {badarg, "Txn must be reference and Name must be an atom or string"}}.

%% @doc Open a database with specified flags
-spec open_db(lmdb_txn(), atom() | string(), non_neg_integer()) -> {ok, lmdb_dbi()} | {error, term()}.
open_db(Txn, Name, Flags) when is_reference(Txn), ?IS_ATOM_OR_STRING(Name), ?IS_NON_NEG_INT(Flags) ->
    DbName = case Name of
        default -> undefined;
        undefined -> undefined;
        _ when is_atom(Name) -> atom_to_list(Name);
        _ -> Name
    end,
    lmdb_nif:dbi_open(Txn, DbName, Flags);
open_db(_, _, _) ->
    {error, {badargs, 
        "Txn must be reference, Name, must be atom or string, and Flags, must be non negative integer"
    }}.

%% @doc Close a database
-spec close_db(lmdb_env(), lmdb_dbi()) -> ok.
close_db(Env, Dbi) ->
    lmdb_nif:dbi_close(Env, Dbi).

%% @doc Get a value by key
get(Env, Key) ->
    get(Env, Key, [create]).
get(Env, Key, Flags) when is_list(Flags) ->
    get(Env, Key, merge_flags(Flags));
get(Env, Key, FlagsInt) when is_integer(FlagsInt) ->
    Res = 
        with_ro_txn(
            Env,
            fun(Txn) ->
                case open_db(Txn, default, FlagsInt) of
                    {ok, Dbi} ->
                        get(Txn, Dbi, Key);
                    Error ->
                        Error
                end
            end
        ),
    case Res of
        {ok, Value} -> Value;
        Error -> Error
    end;
get(Txn, Dbi, Key) when is_binary(Key) ->
    lmdb_nif:get(Txn, Dbi, Key);
get(Txn, Dbi, Key) ->
    get(Txn, Dbi, term_to_binary(Key)).

%% @doc Put a key-value pair with default flags
put(Env, Key, Value) ->
    put(Env, Key, Value, [create]).
put(Env, Key, Value, Flags) when is_list(Flags) ->
    put(Env, Key, Value, merge_flags(Flags));
put(Env, Key, Value, FlagsInt) when is_integer(FlagsInt) ->
    Res =
        with_txn(
            Env,
            fun(Txn) ->
                case open_db(Txn, default, FlagsInt) of
                    {ok, Dbi} ->
                        ok = put(Txn, Dbi, Key, Value),
                        ok = close_db(Env, Dbi),
                        ok;
                    Error ->
                        Error
                end
            end
        ),
    case Res of
        {ok, ok} -> ok;
        Error -> Error
    end;
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

merge_flags(List) ->
    lists:foldl(
        fun(Flag, Acc) ->
            flag_to_enum(Flag) bor Acc
        end,
        0,
        List
    ).

%% @doc Convert flag atoms to their enum equivalents.
flag_to_enum(read_only) -> ?MDB_RDONLY;
flag_to_enum(write) -> 0;
flag_to_enum(create) -> ?MDB_CREATE;
flag_to_enum(no_overwrite) -> ?MDB_NOOVERWRITE;
flag_to_enum(no_dup_data) -> ?MDB_NODUPDATA;
flag_to_enum(current) -> ?MDB_CURRENT;
flag_to_enum(reserve) -> ?MDB_RESERVE;
flag_to_enum(append) -> ?MDB_APPEND;
flag_to_enum(append_dup) -> ?MDB_APPENDDUP;
flag_to_enum(multiple) -> ?MDB_MULTIPLE;
%% Environment flags
flag_to_enum(fixedmap) -> ?MDB_FIXEDMAP;
flag_to_enum(nosubdir) -> ?MDB_NOSUBDIR;
flag_to_enum(nosync) -> ?MDB_NOSYNC;
flag_to_enum(nometasync) -> ?MDB_NOMETASYNC;
flag_to_enum(writemap) -> ?MDB_WRITEMAP;
flag_to_enum(mapasync) -> ?MDB_MAPASYNC;
flag_to_enum(notls) -> ?MDB_NOTLS;
flag_to_enum(nolock) -> ?MDB_NOLOCK;
flag_to_enum(nordahead) -> ?MDB_NORDAHEAD;
flag_to_enum(nomeminit) -> ?MDB_NOMEMINIT;
%% Database flags
flag_to_enum(reversekey) -> ?MDB_REVERSEKEY;
flag_to_enum(dupsort) -> ?MDB_DUPSORT;
flag_to_enum(integerkey) -> ?MDB_INTEGERKEY;
flag_to_enum(dupfixed) -> ?MDB_DUPFIXED;
flag_to_enum(integerdup) -> ?MDB_INTEGERDUP;
flag_to_enum(reversedup) -> ?MDB_REVERSEDUP;
%% Cursor operations
flag_to_enum(first) -> ?MDB_FIRST;
flag_to_enum(first_dup) -> ?MDB_FIRST_DUP;
flag_to_enum(get_both) -> ?MDB_GET_BOTH;
flag_to_enum(get_both_range) -> ?MDB_GET_BOTH_RANGE;
flag_to_enum(get_current) -> ?MDB_GET_CURRENT;
flag_to_enum(get_multiple) -> ?MDB_GET_MULTIPLE;
flag_to_enum(last) -> ?MDB_LAST;
flag_to_enum(last_dup) -> ?MDB_LAST_DUP;
flag_to_enum(next) -> ?MDB_NEXT;
flag_to_enum(next_dup) -> ?MDB_NEXT_DUP;
flag_to_enum(next_multiple) -> ?MDB_NEXT_MULTIPLE;
flag_to_enum(next_nodup) -> ?MDB_NEXT_NODUP;
flag_to_enum(prev) -> ?MDB_PREV;
flag_to_enum(prev_dup) -> ?MDB_PREV_DUP;
flag_to_enum(prev_nodup) -> ?MDB_PREV_NODUP;
flag_to_enum(set) -> ?MDB_SET;
flag_to_enum(set_key) -> ?MDB_SET_KEY;
flag_to_enum(set_range) -> ?MDB_SET_RANGE;
%% Error codes (commonly used)
flag_to_enum(success) -> ?MDB_SUCCESS;
flag_to_enum(keyexist) -> ?MDB_KEYEXIST;
flag_to_enum(notfound) -> ?MDB_NOTFOUND;
flag_to_enum(page_notfound) -> ?MDB_PAGE_NOTFOUND;
flag_to_enum(corrupted) -> ?MDB_CORRUPTED;
flag_to_enum(panic) -> ?MDB_PANIC;
flag_to_enum(version_mismatch) -> ?MDB_VERSION_MISMATCH;
flag_to_enum(invalid) -> ?MDB_INVALID;
flag_to_enum(map_full) -> ?MDB_MAP_FULL;
flag_to_enum(dbs_full) -> ?MDB_DBS_FULL;
flag_to_enum(readers_full) -> ?MDB_READERS_FULL;
flag_to_enum(tls_full) -> ?MDB_TLS_FULL;
flag_to_enum(txn_full) -> ?MDB_TXN_FULL;
flag_to_enum(cursor_full) -> ?MDB_CURSOR_FULL;
flag_to_enum(page_full) -> ?MDB_PAGE_FULL;
flag_to_enum(map_resized) -> ?MDB_MAP_RESIZED;
flag_to_enum(incompatible) -> ?MDB_INCOMPATIBLE;
flag_to_enum(bad_rslot) -> ?MDB_BAD_RSLOT;
flag_to_enum(bad_txn) -> ?MDB_BAD_TXN;
flag_to_enum(bad_valsize) -> ?MDB_BAD_VALSIZE;
flag_to_enum(bad_dbi) -> ?MDB_BAD_DBI.
