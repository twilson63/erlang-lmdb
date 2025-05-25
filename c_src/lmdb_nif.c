#include <erl_nif.h>
#include <string.h>
#include <stdlib.h>
#include "lmdb.h"

// Resource types
static ErlNifResourceType* env_resource_type;
static ErlNifResourceType* txn_resource_type;
static ErlNifResourceType* cursor_resource_type;

// Resource structures
typedef struct {
    MDB_env* env;
} env_handle;

typedef struct {
    MDB_txn* txn;
} txn_handle;

typedef struct {
    MDB_cursor* cursor;
} cursor_handle;

// Helper functions
static ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name) {
    ERL_NIF_TERM ret;
    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

// Removed unused make_error function - using make_mdb_error instead

static ERL_NIF_TERM make_mdb_error(ErlNifEnv* env, int rc) {
    return enif_make_tuple2(env, make_atom(env, "error"), 
                           enif_make_string(env, mdb_strerror(rc), ERL_NIF_LATIN1));
}

// Resource destructors
static void env_resource_dtor(ErlNifEnv* env, void* obj) {
    env_handle* handle = (env_handle*)obj;
    if (handle->env) {
        mdb_env_close(handle->env);
    }
}

static void txn_resource_dtor(ErlNifEnv* env, void* obj) {
    txn_handle* handle = (txn_handle*)obj;
    if (handle->txn) {
        mdb_txn_abort(handle->txn);
    }
}

static void cursor_resource_dtor(ErlNifEnv* env, void* obj) {
    cursor_handle* handle = (cursor_handle*)obj;
    if (handle->cursor) {
        mdb_cursor_close(handle->cursor);
    }
}

// NIF functions
static ERL_NIF_TERM nif_env_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle = enif_alloc_resource(env_resource_type, sizeof(env_handle));
    if (!handle) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_create(&handle->env);
    if (rc != 0) {
        enif_release_resource(handle);
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, make_atom(env, "ok"), result);
}

static ERL_NIF_TERM nif_env_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle;
    char path[1024];
    unsigned int flags;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle) ||
        !enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) ||
        !enif_get_uint(env, argv[2], &flags)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_open(handle->env, path, flags, 0664);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle)) {
        return enif_make_badarg(env);
    }
    
    if (handle->env) {
        mdb_env_close(handle->env);
        handle->env = NULL;
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_set_maxreaders(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle;
    unsigned int readers;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[1], &readers)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_set_maxreaders(handle->env, readers);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_set_maxdbs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle;
    unsigned int dbs;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[1], &dbs)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_set_maxdbs(handle->env, dbs);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_set_mapsize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle;
    unsigned long size;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle) ||
        !enif_get_ulong(env, argv[1], &size)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_set_mapsize(handle->env, size);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle;
    int force;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle) ||
        !enif_get_int(env, argv[1], &force)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_sync(handle->env, force);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_stat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle;
    MDB_stat stat;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_stat(handle->env, &stat);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM stat_list[] = {
        enif_make_tuple2(env, make_atom(env, "psize"), enif_make_uint(env, stat.ms_psize)),
        enif_make_tuple2(env, make_atom(env, "depth"), enif_make_uint(env, stat.ms_depth)),
        enif_make_tuple2(env, make_atom(env, "branch_pages"), enif_make_ulong(env, stat.ms_branch_pages)),
        enif_make_tuple2(env, make_atom(env, "leaf_pages"), enif_make_ulong(env, stat.ms_leaf_pages)),
        enif_make_tuple2(env, make_atom(env, "overflow_pages"), enif_make_ulong(env, stat.ms_overflow_pages)),
        enif_make_tuple2(env, make_atom(env, "entries"), enif_make_ulong(env, stat.ms_entries))
    };
    
    return enif_make_tuple2(env, make_atom(env, "ok"), enif_make_list_from_array(env, stat_list, 6));
}

static ERL_NIF_TERM nif_env_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle;
    MDB_envinfo info;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_info(handle->env, &info);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM info_list[] = {
        enif_make_tuple2(env, make_atom(env, "mapaddr"), enif_make_ulong(env, (unsigned long)info.me_mapaddr)),
        enif_make_tuple2(env, make_atom(env, "mapsize"), enif_make_ulong(env, info.me_mapsize)),
        enif_make_tuple2(env, make_atom(env, "last_pgno"), enif_make_ulong(env, info.me_last_pgno)),
        enif_make_tuple2(env, make_atom(env, "last_txnid"), enif_make_ulong(env, info.me_last_txnid)),
        enif_make_tuple2(env, make_atom(env, "maxreaders"), enif_make_uint(env, info.me_maxreaders)),
        enif_make_tuple2(env, make_atom(env, "numreaders"), enif_make_uint(env, info.me_numreaders))
    };
    
    return enif_make_tuple2(env, make_atom(env, "ok"), enif_make_list_from_array(env, info_list, 6));
}

static ERL_NIF_TERM nif_txn_begin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* env_handle_ptr;
    txn_handle* parent_handle = NULL;
    unsigned int flags;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&env_handle_ptr) ||
        !enif_get_uint(env, argv[2], &flags)) {
        return enif_make_badarg(env);
    }
    
    // Check if parent transaction is provided
    if (!enif_is_atom(env, argv[1])) {
        if (!enif_get_resource(env, argv[1], txn_resource_type, (void**)&parent_handle)) {
            return enif_make_badarg(env);
        }
    }
    
    txn_handle* handle = enif_alloc_resource(txn_resource_type, sizeof(txn_handle));
    if (!handle) {
        return enif_make_badarg(env);
    }
    
    MDB_txn* parent_txn = parent_handle ? parent_handle->txn : NULL;
    int rc = mdb_txn_begin(env_handle_ptr->env, parent_txn, flags, &handle->txn);
    if (rc != 0) {
        enif_release_resource(handle);
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, make_atom(env, "ok"), result);
}

static ERL_NIF_TERM nif_txn_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    txn_handle* handle;
    
    if (!enif_get_resource(env, argv[0], txn_resource_type, (void**)&handle)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_txn_commit(handle->txn);
    handle->txn = NULL; // Transaction is no longer valid after commit
    
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_txn_abort(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    txn_handle* handle;
    
    if (!enif_get_resource(env, argv[0], txn_resource_type, (void**)&handle)) {
        return enif_make_badarg(env);
    }
    
    if (handle->txn) {
        mdb_txn_abort(handle->txn);
        handle->txn = NULL;
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_dbi_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    txn_handle* handle;
    char name[256];
    unsigned int flags;
    MDB_dbi dbi;
    
    if (!enif_get_resource(env, argv[0], txn_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[2], &flags)) {
        return enif_make_badarg(env);
    }
    
    const char* db_name = NULL;
    if (!enif_is_atom(env, argv[1])) {
        if (!enif_get_string(env, argv[1], name, sizeof(name), ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }
        db_name = name;
    }
    
    int rc = mdb_dbi_open(handle->txn, db_name, flags, &dbi);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return enif_make_tuple2(env, make_atom(env, "ok"), enif_make_uint(env, dbi));
}

static ERL_NIF_TERM nif_dbi_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    env_handle* handle;
    unsigned int dbi;
    
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[1], &dbi)) {
        return enif_make_badarg(env);
    }
    
    mdb_dbi_close(handle->env, dbi);
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_dbi_stat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    txn_handle* handle;
    unsigned int dbi;
    MDB_stat stat;
    
    if (!enif_get_resource(env, argv[0], txn_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[1], &dbi)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_stat(handle->txn, dbi, &stat);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM stat_list[] = {
        enif_make_tuple2(env, make_atom(env, "psize"), enif_make_uint(env, stat.ms_psize)),
        enif_make_tuple2(env, make_atom(env, "depth"), enif_make_uint(env, stat.ms_depth)),
        enif_make_tuple2(env, make_atom(env, "branch_pages"), enif_make_ulong(env, stat.ms_branch_pages)),
        enif_make_tuple2(env, make_atom(env, "leaf_pages"), enif_make_ulong(env, stat.ms_leaf_pages)),
        enif_make_tuple2(env, make_atom(env, "overflow_pages"), enif_make_ulong(env, stat.ms_overflow_pages)),
        enif_make_tuple2(env, make_atom(env, "entries"), enif_make_ulong(env, stat.ms_entries))
    };
    
    return enif_make_tuple2(env, make_atom(env, "ok"), enif_make_list_from_array(env, stat_list, 6));
}

static ERL_NIF_TERM nif_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    txn_handle* handle;
    unsigned int dbi;
    ErlNifBinary key_bin;
    MDB_val key, data;
    
    if (!enif_get_resource(env, argv[0], txn_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[1], &dbi) ||
        !enif_inspect_binary(env, argv[2], &key_bin)) {
        return enif_make_badarg(env);
    }
    
    key.mv_size = key_bin.size;
    key.mv_data = key_bin.data;
    
    int rc = mdb_get(handle->txn, dbi, &key, &data);
    if (rc == MDB_NOTFOUND) {
        return make_atom(env, "not_found");
    } else if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM data_term;
    unsigned char* data_ptr = enif_make_new_binary(env, data.mv_size, &data_term);
    memcpy(data_ptr, data.mv_data, data.mv_size);
    
    return enif_make_tuple2(env, make_atom(env, "ok"), data_term);
}

static ERL_NIF_TERM nif_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    txn_handle* handle;
    unsigned int dbi;
    ErlNifBinary key_bin, data_bin;
    unsigned int flags;
    MDB_val key, data;
    
    if (!enif_get_resource(env, argv[0], txn_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[1], &dbi) ||
        !enif_inspect_binary(env, argv[2], &key_bin) ||
        !enif_inspect_binary(env, argv[3], &data_bin) ||
        !enif_get_uint(env, argv[4], &flags)) {
        return enif_make_badarg(env);
    }
    
    key.mv_size = key_bin.size;
    key.mv_data = key_bin.data;
    data.mv_size = data_bin.size;
    data.mv_data = data_bin.data;
    
    int rc = mdb_put(handle->txn, dbi, &key, &data, flags);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_del(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    txn_handle* handle;
    unsigned int dbi;
    ErlNifBinary key_bin;
    MDB_val key, *data_ptr = NULL;
    
    if (!enif_get_resource(env, argv[0], txn_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[1], &dbi) ||
        !enif_inspect_binary(env, argv[2], &key_bin)) {
        return enif_make_badarg(env);
    }
    
    key.mv_size = key_bin.size;
    key.mv_data = key_bin.data;
    
    // Check if data parameter is provided (4 arguments means key+data delete)
    MDB_val data;
    if (argc == 4) {
        ErlNifBinary data_bin;
        if (!enif_inspect_binary(env, argv[3], &data_bin)) {
            return enif_make_badarg(env);
        }
        data.mv_size = data_bin.size;
        data.mv_data = data_bin.data;
        data_ptr = &data;
    }
    
    int rc = mdb_del(handle->txn, dbi, &key, data_ptr);
    if (rc == MDB_NOTFOUND) {
        return make_atom(env, "not_found");
    } else if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_cursor_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    txn_handle* handle;
    unsigned int dbi;
    
    if (!enif_get_resource(env, argv[0], txn_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[1], &dbi)) {
        return enif_make_badarg(env);
    }
    
    cursor_handle* cursor_handle_ptr = enif_alloc_resource(cursor_resource_type, sizeof(cursor_handle));
    if (!cursor_handle_ptr) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_cursor_open(handle->txn, dbi, &cursor_handle_ptr->cursor);
    if (rc != 0) {
        enif_release_resource(cursor_handle_ptr);
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM result = enif_make_resource(env, cursor_handle_ptr);
    enif_release_resource(cursor_handle_ptr);
    return enif_make_tuple2(env, make_atom(env, "ok"), result);
}

static ERL_NIF_TERM nif_cursor_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cursor_handle* handle;
    
    if (!enif_get_resource(env, argv[0], cursor_resource_type, (void**)&handle)) {
        return enif_make_badarg(env);
    }
    
    if (handle->cursor) {
        mdb_cursor_close(handle->cursor);
        handle->cursor = NULL;
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_cursor_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cursor_handle* handle;
    ErlNifBinary key_bin;
    unsigned int op;
    MDB_val key, data;
    
    if (!enif_get_resource(env, argv[0], cursor_resource_type, (void**)&handle) ||
        !enif_inspect_binary(env, argv[1], &key_bin) ||
        !enif_get_uint(env, argv[2], &op)) {
        return enif_make_badarg(env);
    }
    
    key.mv_size = key_bin.size;
    key.mv_data = key_bin.data;
    
    int rc = mdb_cursor_get(handle->cursor, &key, &data, op);
    if (rc == MDB_NOTFOUND) {
        return make_atom(env, "not_found");
    } else if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM key_term, data_term;
    unsigned char* key_ptr = enif_make_new_binary(env, key.mv_size, &key_term);
    unsigned char* data_ptr = enif_make_new_binary(env, data.mv_size, &data_term);
    
    memcpy(key_ptr, key.mv_data, key.mv_size);
    memcpy(data_ptr, data.mv_data, data.mv_size);
    
    return enif_make_tuple3(env, make_atom(env, "ok"), key_term, data_term);
}

static ERL_NIF_TERM nif_cursor_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cursor_handle* handle;
    ErlNifBinary key_bin, data_bin;
    unsigned int flags;
    MDB_val key, data;
    
    if (!enif_get_resource(env, argv[0], cursor_resource_type, (void**)&handle) ||
        !enif_inspect_binary(env, argv[1], &key_bin) ||
        !enif_inspect_binary(env, argv[2], &data_bin) ||
        !enif_get_uint(env, argv[3], &flags)) {
        return enif_make_badarg(env);
    }
    
    key.mv_size = key_bin.size;
    key.mv_data = key_bin.data;
    data.mv_size = data_bin.size;
    data.mv_data = data_bin.data;
    
    int rc = mdb_cursor_put(handle->cursor, &key, &data, flags);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_cursor_del(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cursor_handle* handle;
    unsigned int flags;
    
    if (!enif_get_resource(env, argv[0], cursor_resource_type, (void**)&handle) ||
        !enif_get_uint(env, argv[1], &flags)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_cursor_del(handle->cursor, flags);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

// NIF function table
static ErlNifFunc nif_funcs[] = {
    {"env_create", 0, nif_env_create},
    {"env_open", 3, nif_env_open},
    {"env_close", 1, nif_env_close},
    {"env_set_maxreaders", 2, nif_env_set_maxreaders},
    {"env_set_maxdbs", 2, nif_env_set_maxdbs},
    {"env_set_mapsize", 2, nif_env_set_mapsize},
    {"env_sync", 2, nif_env_sync},
    {"env_stat", 1, nif_env_stat},
    {"env_info", 1, nif_env_info},
    {"txn_begin", 3, nif_txn_begin},
    {"txn_commit", 1, nif_txn_commit},
    {"txn_abort", 1, nif_txn_abort},
    {"dbi_open", 3, nif_dbi_open},
    {"dbi_close", 2, nif_dbi_close},
    {"dbi_stat", 2, nif_dbi_stat},
    {"get", 3, nif_get},
    {"put", 5, nif_put},
    {"del", 3, nif_del},
    {"del", 4, nif_del},
    {"cursor_open", 2, nif_cursor_open},
    {"cursor_close", 1, nif_cursor_close},
    {"cursor_get", 3, nif_cursor_get},
    {"cursor_put", 4, nif_cursor_put},
    {"cursor_del", 2, nif_cursor_del}
};

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    env_resource_type = enif_open_resource_type(env, NULL, "env_resource",
                                                env_resource_dtor,
                                                ERL_NIF_RT_CREATE, NULL);
    txn_resource_type = enif_open_resource_type(env, NULL, "txn_resource",
                                                txn_resource_dtor,
                                                ERL_NIF_RT_CREATE, NULL);
    cursor_resource_type = enif_open_resource_type(env, NULL, "cursor_resource",
                                                   cursor_resource_dtor,
                                                   ERL_NIF_RT_CREATE, NULL);
    
    if (!env_resource_type || !txn_resource_type || !cursor_resource_type) {
        return -1;
    }
    
    return 0;
}

ERL_NIF_INIT(lmdb_nif, nif_funcs, load, NULL, NULL, NULL)
