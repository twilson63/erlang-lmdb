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
    if (!env || !name) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM ret;
    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

// Guard macros for common validation patterns
#define VALIDATE_ARGC(expected) \
    if (argc != (expected)) { \
        return enif_make_badarg(env); \
    }

#define VALIDATE_ENV_HANDLE(handle_var, arg_index) \
    env_handle* handle_var; \
    if (!enif_get_resource(env, argv[arg_index], env_resource_type, (void**)&handle_var) || \
        !handle_var || !handle_var->env) { \
        return enif_make_badarg(env); \
    }

#define VALIDATE_TXN_HANDLE(handle_var, arg_index) \
    txn_handle* handle_var; \
    if (!enif_get_resource(env, argv[arg_index], txn_resource_type, (void**)&handle_var) || \
        !handle_var || !handle_var->txn) { \
        return enif_make_badarg(env); \
    }

#define VALIDATE_CURSOR_HANDLE(handle_var, arg_index) \
    cursor_handle* handle_var; \
    if (!enif_get_resource(env, argv[arg_index], cursor_resource_type, (void**)&handle_var) || \
        !handle_var || !handle_var->cursor) { \
        return enif_make_badarg(env); \
    }

#define VALIDATE_UINT(var_name, arg_index) \
    unsigned int var_name; \
    if (!enif_get_uint(env, argv[arg_index], &var_name)) { \
        return enif_make_badarg(env); \
    }

#define VALIDATE_ULONG(var_name, arg_index) \
    unsigned long var_name; \
    if (!enif_get_ulong(env, argv[arg_index], &var_name)) { \
        return enif_make_badarg(env); \
    }

#define VALIDATE_INT(var_name, arg_index) \
    int var_name; \
    if (!enif_get_int(env, argv[arg_index], &var_name)) { \
        return enif_make_badarg(env); \
    }

#define VALIDATE_STRING(var_name, size, arg_index) \
    char var_name[size]; \
    if (!enif_get_string(env, argv[arg_index], var_name, sizeof(var_name), ERL_NIF_LATIN1)) { \
        return enif_make_badarg(env); \
    }

#define VALIDATE_BINARY(var_name, arg_index) \
    ErlNifBinary var_name; \
    if (!enif_inspect_binary(env, argv[arg_index], &var_name)) { \
        return enif_make_badarg(env); \
    }

// Removed unused make_error function - using make_mdb_error instead

static ERL_NIF_TERM make_mdb_error(ErlNifEnv* env, int rc) {
    if (!env) {
        return enif_make_badarg(env);
    }
    const char* error_str = mdb_strerror(rc);
    if (!error_str) {
        error_str = "unknown_error";
    }
    return enif_make_tuple2(env, make_atom(env, "error"), 
                           enif_make_string(env, error_str, ERL_NIF_LATIN1));
}

// Resource destructors
static void env_resource_dtor(ErlNifEnv* env, void* obj) {
    if (!obj) return;
    env_handle* handle = (env_handle*)obj;
    if (handle && handle->env) {
        mdb_env_close(handle->env);
        handle->env = NULL;
    }
}

static void txn_resource_dtor(ErlNifEnv* env, void* obj) {
    if (!obj) return;
    txn_handle* handle = (txn_handle*)obj;
    if (handle && handle->txn) {
        mdb_txn_abort(handle->txn);
        handle->txn = NULL;
    }
}

static void cursor_resource_dtor(ErlNifEnv* env, void* obj) {
    if (!obj) return;
    cursor_handle* handle = (cursor_handle*)obj;
    if (handle && handle->cursor) {
        mdb_cursor_close(handle->cursor);
        handle->cursor = NULL;
    }
}

// NIF functions
static ERL_NIF_TERM nif_env_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(0);
    
    if (!env_resource_type) {
        return enif_make_badarg(env);
    }
    
    env_handle* handle = enif_alloc_resource(env_resource_type, sizeof(env_handle));
    if (!handle) {
        return enif_make_badarg(env);
    }
    
    // Initialize handle to safe state
    handle->env = NULL;
    
    int rc = mdb_env_create(&handle->env);
    if (rc != 0 || !handle->env) {
        enif_release_resource(handle);
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, make_atom(env, "ok"), result);
}

static ERL_NIF_TERM nif_env_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(3);
    VALIDATE_ENV_HANDLE(handle, 0);
    VALIDATE_STRING(path, 1024, 1);
    VALIDATE_UINT(flags, 2);
    
    // Additional path validation
    if (strlen(path) == 0) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_open(handle->env, path, flags, 0664);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(1);
    
    env_handle* handle;
    if (!enif_get_resource(env, argv[0], env_resource_type, (void**)&handle) || !handle) {
        return enif_make_badarg(env);
    }
    
    if (handle->env) {
        mdb_env_close(handle->env);
        handle->env = NULL;
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_set_maxreaders(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(2);
    VALIDATE_ENV_HANDLE(handle, 0);
    VALIDATE_UINT(readers, 1);
    
    // Validate reasonable bounds for readers
    if (readers == 0 || readers > 1024) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_set_maxreaders(handle->env, readers);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_set_maxdbs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(2);
    VALIDATE_ENV_HANDLE(handle, 0);
    VALIDATE_UINT(dbs, 1);
    
    // Validate reasonable bounds for databases
    if (dbs == 0 || dbs > 1024) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_set_maxdbs(handle->env, dbs);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_set_mapsize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(2);
    VALIDATE_ENV_HANDLE(handle, 0);
    VALIDATE_ULONG(size, 1);
    
    // Validate reasonable bounds for map size (minimum 1KB, maximum 1TB)
    if (size < 1024 || size > (1024UL * 1024UL * 1024UL * 1024UL)) {
        return enif_make_badarg(env);
    }
    
    int rc = mdb_env_set_mapsize(handle->env, size);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(2);
    VALIDATE_ENV_HANDLE(handle, 0);
    VALIDATE_INT(force, 1);
    
    int rc = mdb_env_sync(handle->env, force);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_env_stat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(1);
    VALIDATE_ENV_HANDLE(handle, 0);
    
    MDB_stat stat;
    memset(&stat, 0, sizeof(stat));
    
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
    VALIDATE_ARGC(1);
    VALIDATE_ENV_HANDLE(handle, 0);
    
    MDB_envinfo info;
    memset(&info, 0, sizeof(info));
    
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
    VALIDATE_ARGC(3);
    VALIDATE_ENV_HANDLE(env_handle_ptr, 0);
    VALIDATE_UINT(flags, 2);
    
    if (!txn_resource_type) {
        return enif_make_badarg(env);
    }
    
    txn_handle* parent_handle = NULL;
    
    // Check if parent transaction is provided
    if (!enif_is_atom(env, argv[1])) {
        if (!enif_get_resource(env, argv[1], txn_resource_type, (void**)&parent_handle) ||
            !parent_handle || !parent_handle->txn) {
            return enif_make_badarg(env);
        }
    }
    
    txn_handle* handle = enif_alloc_resource(txn_resource_type, sizeof(txn_handle));
    if (!handle) {
        return enif_make_badarg(env);
    }
    
    // Initialize handle to safe state
    handle->txn = NULL;
    
    MDB_txn* parent_txn = parent_handle ? parent_handle->txn : NULL;
    int rc = mdb_txn_begin(env_handle_ptr->env, parent_txn, flags, &handle->txn);
    if (rc != 0 || !handle->txn) {
        enif_release_resource(handle);
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, make_atom(env, "ok"), result);
}

static ERL_NIF_TERM nif_txn_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(1);
    VALIDATE_TXN_HANDLE(handle, 0);
    
    int rc = mdb_txn_commit(handle->txn);
    handle->txn = NULL; // Transaction is no longer valid after commit
    
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_txn_abort(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(1);
    
    txn_handle* handle;
    if (!enif_get_resource(env, argv[0], txn_resource_type, (void**)&handle) || !handle) {
        return enif_make_badarg(env);
    }
    
    if (handle->txn) {
        mdb_txn_abort(handle->txn);
        handle->txn = NULL;
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_dbi_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(3);
    VALIDATE_TXN_HANDLE(handle, 0);
    VALIDATE_UINT(flags, 2);
    
    char name[256];
    const char* db_name = NULL;
    
    if (!enif_is_atom(env, argv[1])) {
        if (!enif_get_string(env, argv[1], name, sizeof(name), ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }
        // Validate name length
        if (strlen(name) == 0 || strlen(name) >= sizeof(name) - 1) {
            return enif_make_badarg(env);
        }
        db_name = name;
    }
    
    MDB_dbi dbi;
    int rc = mdb_dbi_open(handle->txn, db_name, flags, &dbi);
    if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    return enif_make_tuple2(env, make_atom(env, "ok"), enif_make_uint(env, dbi));
}

static ERL_NIF_TERM nif_dbi_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(2);
    VALIDATE_ENV_HANDLE(handle, 0);
    VALIDATE_UINT(dbi, 1);
    
    mdb_dbi_close(handle->env, dbi);
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_dbi_stat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(2);
    VALIDATE_TXN_HANDLE(handle, 0);
    VALIDATE_UINT(dbi, 1);
    
    MDB_stat stat;
    memset(&stat, 0, sizeof(stat));
    
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
    VALIDATE_ARGC(3);
    VALIDATE_TXN_HANDLE(handle, 0);
    VALIDATE_UINT(dbi, 1);
    VALIDATE_BINARY(key_bin, 2);
    
    // Validate key size (LMDB has limits)
    if (key_bin.size == 0 || key_bin.size > 511) {
        return enif_make_badarg(env);
    }
    
    if (!key_bin.data) {
        return enif_make_badarg(env);
    }
    
    MDB_val key, data;
    key.mv_size = key_bin.size;
    key.mv_data = key_bin.data;
    
    int rc = mdb_get(handle->txn, dbi, &key, &data);
    if (rc == MDB_NOTFOUND) {
        return make_atom(env, "not_found");
    } else if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    if (!data.mv_data || data.mv_size == 0) {
        ERL_NIF_TERM empty_binary;
        enif_make_new_binary(env, 0, &empty_binary);
        return enif_make_tuple2(env, make_atom(env, "ok"), empty_binary);
    }
    
    ERL_NIF_TERM data_term;
    unsigned char* data_ptr = enif_make_new_binary(env, data.mv_size, &data_term);
    if (!data_ptr) {
        return enif_make_badarg(env);
    }
    memcpy(data_ptr, data.mv_data, data.mv_size);
    
    return enif_make_tuple2(env, make_atom(env, "ok"), data_term);
}

static ERL_NIF_TERM nif_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(5);
    VALIDATE_TXN_HANDLE(handle, 0);
    VALIDATE_UINT(dbi, 1);
    VALIDATE_BINARY(key_bin, 2);
    VALIDATE_BINARY(data_bin, 3);
    VALIDATE_UINT(flags, 4);
    
    // Validate key and data sizes (LMDB has limits)
    if (key_bin.size == 0 || key_bin.size > 511) {
        return enif_make_badarg(env);
    }
    
    if (data_bin.size > 2147483647U) { // ~2GB limit
        return enif_make_badarg(env);
    }
    
    if (!key_bin.data || (!data_bin.data && data_bin.size > 0)) {
        return enif_make_badarg(env);
    }
    
    MDB_val key, data;
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
    if (argc != 3 && argc != 4) {
        return enif_make_badarg(env);
    }
    
    VALIDATE_TXN_HANDLE(handle, 0);
    VALIDATE_UINT(dbi, 1);
    VALIDATE_BINARY(key_bin, 2);
    
    // Validate key size
    if (key_bin.size == 0 || key_bin.size > 511 || !key_bin.data) {
        return enif_make_badarg(env);
    }
    
    MDB_val key, *data_ptr = NULL;
    key.mv_size = key_bin.size;
    key.mv_data = key_bin.data;
    
    // Check if data parameter is provided (4 arguments means key+data delete)
    MDB_val data;
    if (argc == 4) {
        ErlNifBinary data_bin;
        if (!enif_inspect_binary(env, argv[3], &data_bin)) {
            return enif_make_badarg(env);
        }
        
        if (data_bin.size > 2147483647U || (!data_bin.data && data_bin.size > 0)) {
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
    VALIDATE_ARGC(2);
    VALIDATE_TXN_HANDLE(handle, 0);
    VALIDATE_UINT(dbi, 1);
    
    if (!cursor_resource_type) {
        return enif_make_badarg(env);
    }
    
    cursor_handle* cursor_handle_ptr = enif_alloc_resource(cursor_resource_type, sizeof(cursor_handle));
    if (!cursor_handle_ptr) {
        return enif_make_badarg(env);
    }
    
    // Initialize handle to safe state
    cursor_handle_ptr->cursor = NULL;
    
    int rc = mdb_cursor_open(handle->txn, dbi, &cursor_handle_ptr->cursor);
    if (rc != 0 || !cursor_handle_ptr->cursor) {
        enif_release_resource(cursor_handle_ptr);
        return make_mdb_error(env, rc);
    }
    
    ERL_NIF_TERM result = enif_make_resource(env, cursor_handle_ptr);
    enif_release_resource(cursor_handle_ptr);
    return enif_make_tuple2(env, make_atom(env, "ok"), result);
}

static ERL_NIF_TERM nif_cursor_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(1);
    
    cursor_handle* handle;
    if (!enif_get_resource(env, argv[0], cursor_resource_type, (void**)&handle) || !handle) {
        return enif_make_badarg(env);
    }
    
    if (handle->cursor) {
        mdb_cursor_close(handle->cursor);
        handle->cursor = NULL;
    }
    
    return make_atom(env, "ok");
}

static ERL_NIF_TERM nif_cursor_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(3);
    VALIDATE_CURSOR_HANDLE(handle, 0);
    VALIDATE_BINARY(key_bin, 1);
    VALIDATE_UINT(op, 2);
    
    // Validate key size for operations that use it
    if (key_bin.size > 511 || (!key_bin.data && key_bin.size > 0)) {
        return enif_make_badarg(env);
    }
    
    MDB_val key, data;
    key.mv_size = key_bin.size;
    key.mv_data = key_bin.data;
    
    int rc = mdb_cursor_get(handle->cursor, &key, &data, op);
    if (rc == MDB_NOTFOUND) {
        return make_atom(env, "not_found");
    } else if (rc != 0) {
        return make_mdb_error(env, rc);
    }
    
    // Validate returned data
    if (!key.mv_data || !data.mv_data) {
        return make_mdb_error(env, MDB_CORRUPTED);
    }
    
    ERL_NIF_TERM key_term, data_term;
    unsigned char* key_ptr = enif_make_new_binary(env, key.mv_size, &key_term);
    unsigned char* data_ptr = enif_make_new_binary(env, data.mv_size, &data_term);
    
    if (!key_ptr || !data_ptr) {
        return enif_make_badarg(env);
    }
    
    memcpy(key_ptr, key.mv_data, key.mv_size);
    memcpy(data_ptr, data.mv_data, data.mv_size);
    
    return enif_make_tuple3(env, make_atom(env, "ok"), key_term, data_term);
}

static ERL_NIF_TERM nif_cursor_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    VALIDATE_ARGC(4);
    VALIDATE_CURSOR_HANDLE(handle, 0);
    VALIDATE_BINARY(key_bin, 1);
    VALIDATE_BINARY(data_bin, 2);
    VALIDATE_UINT(flags, 3);
    
    // Validate key and data sizes
    if (key_bin.size == 0 || key_bin.size > 511) {
        return enif_make_badarg(env);
    }
    
    if (data_bin.size > 2147483647U) {
        return enif_make_badarg(env);
    }
    
    if (!key_bin.data || (!data_bin.data && data_bin.size > 0)) {
        return enif_make_badarg(env);
    }
    
    MDB_val key, data;
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
    VALIDATE_ARGC(2);
    VALIDATE_CURSOR_HANDLE(handle, 0);
    VALIDATE_UINT(flags, 1);
    
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
    if (!env) {
        return -1;
    }
    
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
