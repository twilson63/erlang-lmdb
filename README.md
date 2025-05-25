# LMDB NIF - Erlang Bindings for Lightning Memory-Mapped Database

This project provides Erlang NIF (Native Implemented Functions) bindings for [LMDB (Lightning Memory-Mapped Database)](http://www.lmdb.tech/doc/), a fast, memory-efficient embedded database.

## Features

- **High Performance**: Direct C bindings to LMDB with minimal overhead
- **Memory Efficient**: Zero-copy operations where possible
- **ACID Compliance**: Full transaction support with commit/rollback
- **Thread Safe**: Safe for concurrent access from multiple Erlang processes
- **Embedded**: No separate database server required
- **Auto-build**: Automatically downloads and compiles LMDB during build

## Quick Start

### Prerequisites

- Erlang/OTP 20+ with development headers
- GCC or compatible C compiler
- Make
- curl (for downloading LMDB source)

On Ubuntu/Debian:
```bash
sudo apt-get install erlang-dev build-essential curl
```

On CentOS/RHEL:
```bash
sudo yum install erlang-devel gcc make curl
```

On macOS:
```bash
brew install erlang
```

### Building

```bash
git clone https://github.com/yourorg/lmdb_nif.git
cd lmdb_nif
make
```

Or with rebar3:
```bash
rebar3 compile
```

### Basic Usage

```erlang
%% Create and open an environment
{ok, Env} = lmdb:env_create(),
ok = lmdb:env_set_mapsize(Env, 10485760), % 10MB
ok = lmdb:env_open(Env, "/path/to/db", 0),

%% Simple operations within a transaction
{ok, Result} = lmdb:with_txn(Env, fun(Txn) ->
    {ok, Dbi} = lmdb:open_db(Txn, default),
    
    %% Store data
    ok = lmdb:put(Txn, Dbi, <<"my_key">>, <<"my_value">>),
    
    %% Retrieve data
    {ok, Value} = lmdb:get(Txn, Dbi, <<"my_key">>),
    
    Value
end),

%% Result is <<"my_value">>

%% Clean up
ok = lmdb:env_close(Env).
```

## API Reference

### Environment Management

- `lmdb:env_create/0` - Create a new LMDB environment
- `lmdb:env_open/2,3` - Open an environment at a given path
- `lmdb:env_close/1` - Close an environment
- `lmdb:env_set_mapsize/2` - Set the memory map size
- `lmdb:env_set_maxdbs/2` - Set maximum number of databases
- `lmdb:env_set_maxreaders/2` - Set maximum number of reader threads

### Database Operations

- `lmdb:open_db/2,3` - Open a database within an environment
- `lmdb:close_db/2` - Close a database handle

### Data Operations

- `lmdb:get/3` - Retrieve a value by key
- `lmdb:put/4,5` - Store a key-value pair
- `lmdb:delete/3,4` - Delete a key or key-value pair
- `lmdb:exists/3` - Check if a key exists

### Transaction Management

- `lmdb:with_txn/2,3` - Execute a function within a read-write transaction
- `lmdb:with_ro_txn/2` - Execute a function within a read-only transaction

### Batch Operations

- `lmdb:write_batch/2` - Execute multiple write operations in a single transaction

### Iteration

- `lmdb:fold/4` - Fold over all key-value pairs
- `lmdb:foreach/3` - Execute a function for each key-value pair
- `lmdb:count/2` - Count entries in a database

## Advanced Usage

### Custom Database Names

```erlang
{ok, Result} = lmdb:with_txn(Env, fun(Txn) ->
    {ok, UsersDbi} = lmdb:open_db(Txn, "users", ?MDB_CREATE),
    {ok, ProductsDbi} = lmdb:open_db(Txn, "products", ?MDB_CREATE),
    
    ok = lmdb:put(Txn, UsersDbi, <<"user:1">>, <<"alice">>),
    ok = lmdb:put(Txn, ProductsDbi, <<"prod:1">>, <<"laptop">>),
    
    success
end).
```

### Batch Operations

```erlang
Operations = [
    {put, Dbi, <<"key1">>, <<"value1">>},
    {put, Dbi, <<"key2">>, <<"value2">>},
    {delete, Dbi, <<"old_key">>}
],
ok = lmdb:write_batch(Env, Operations).
```

### Iteration and Aggregation

```erlang
%% Count all keys starting with "user:"
{ok, UserCount} = lmdb:fold(Env, Dbi, fun(Key, _Value, Acc) ->
    case binary:match(Key, <<"user:">>) of
        {0, _} -> Acc + 1;
        _ -> Acc
    end
end, 0).
```

### Error Handling

```erlang
case lmdb:with_txn(Env, fun(Txn) ->
    {ok, Dbi} = lmdb:open_db(Txn, default),
    lmdb:put(Txn, Dbi, Key, Value, ?MDB_NOOVERWRITE)
end) of
    ok -> 
        io:format("Value stored successfully~n");
    {error, Reason} -> 
        io:format("Failed to store: ~p~n", [Reason])
end.
```

## Configuration

### Environment Flags

Common environment flags (can be combined with bitwise OR):

- `?MDB_RDONLY` - Open in read-only mode
- `?MDB_NOSYNC` - Don't fsync after commit (faster, less durable)
- `?MDB_NOMETASYNC` - Don't fsync meta page after commit
- `?MDB_WRITEMAP` - Use writable memory map
- `?MDB_NOLOCK` - Disable file locking (single process only)

### Database Flags

- `?MDB_CREATE` - Create database if it doesn't exist
- `?MDB_DUPSORT` - Allow duplicate keys
- `?MDB_INTEGERKEY` - Keys are binary integers

### Write Flags

- `?MDB_NOOVERWRITE` - Don't overwrite existing keys
- `?MDB_NODUPDATA` - Don't write duplicate key-value pairs
- `?MDB_APPEND` - Append data (keys must be in sort order)

## Performance Tips

1. **Set appropriate map size**: Use `env_set_mapsize/2` to set a reasonable upper bound
2. **Use batch operations**: Group multiple writes in single transactions
3. **Read-only transactions**: Use `with_ro_txn/2` for read-only operations
4. **Avoid long-running transactions**: Keep transactions short to prevent blocking
5. **Consider NOSYNC**: Use `?MDB_NOSYNC` for better performance if you can tolerate some data loss

## Testing

Run the test suite:

```bash
make test
```

Or with rebar3:
```bash
rebar3 eunit
```

## Building from Source

The build process automatically:

1. Downloads LMDB source code
2. Compiles LMDB as a static library
3. Compiles the NIF with static linking
4. No runtime dependencies on LMDB

Manual build steps:
```bash
make deps    # Download and build LMDB
make compile # Build NIF and Erlang modules
make test    # Run tests
```

## Troubleshooting

### Common Issues

**Build fails with missing headers**:
```
error: erl_nif.h: No such file or directory
```
Install Erlang development headers (`erlang-dev` on Ubuntu).

**LMDB download fails**:
Check internet connection and firewall settings. You can manually download LMDB and extract it to `c_src/`.

**Permission errors on database files**:
Ensure the database directory is writable and not on a network filesystem.

**Map size errors**:
Increase the map size with `env_set_mapsize/2`. LMDB requires declaring the maximum database size upfront.

## License

MIT License - see LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## Acknowledgments

- [LMDB](http://www.lmdb.tech/) by Howard Chu and Symas Corp
- Inspired by various LMDB bindings for other languages
