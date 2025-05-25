%% @doc LMDB Erlang NIF Wrapper
%% Provides Erlang bindings for Lightning Memory-Mapped Database
-module(lmdb_nif).

-export([
    % Environment operations
    env_create/0,
    env_open/3,
    env_close/1,
    env_set_maxreaders/2,
    env_set_maxdbs/2,
    env_set_mapsize/2,
    env_sync/2,
    env_stat/1,
    env_info/1,
    
    % Transaction operations
    txn_begin/3,
    txn_commit/1,
    txn_abort/1,
    
    % Database operations
    dbi_open/3,
    dbi_close/2,
    dbi_stat/2,
    
    % Data operations
    get/3,
    put/5,
    del/3,
    del/4,
    
    % Cursor operations
    cursor_open/2,
    cursor_close/1,
    cursor_get/3,
    cursor_put/4,
    cursor_del/2
]).

-on_load(init/0).

%% @doc Initialize the NIF
init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, lmdb_nif]);
                _ ->
                    filename:join([priv, lmdb_nif])
            end;
        Dir ->
            filename:join(Dir, lmdb_nif)
    end,
    erlang:load_nif(SoName, 0).

%% NIF function stubs - will be replaced by actual NIF implementations
env_create() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_open(_Env, _Path, _Flags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_close(_Env) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_set_maxreaders(_Env, _Readers) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_set_maxdbs(_Env, _Dbs) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_set_mapsize(_Env, _Size) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_sync(_Env, _Force) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_stat(_Env) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

env_info(_Env) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

txn_begin(_Env, _Parent, _Flags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

txn_commit(_Txn) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

txn_abort(_Txn) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

dbi_open(_Txn, _Name, _Flags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

dbi_close(_Env, _Dbi) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

dbi_stat(_Txn, _Dbi) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

get(_Txn, _Dbi, _Key) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

put(_Txn, _Dbi, _Key, _Data, _Flags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

del(_Txn, _Dbi, _Key) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

del(_Txn, _Dbi, _Key, _Data) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cursor_open(_Txn, _Dbi) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cursor_close(_Cursor) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cursor_get(_Cursor, _Key, _Op) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cursor_put(_Cursor, _Key, _Data, _Flags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

cursor_del(_Cursor, _Flags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
