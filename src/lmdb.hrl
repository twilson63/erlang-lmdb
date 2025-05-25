%% @doc LMDB type definitions and constants
%% include/lmdb.hrl

%% Type definitions
-type lmdb_env() :: reference().
-type lmdb_txn() :: reference().
-type lmdb_dbi() :: non_neg_integer().
-type lmdb_cursor() :: reference().

%% Environment flags
-define(MDB_FIXEDMAP,     16#01).    %% Use a fixed address for the mmap region
-define(MDB_NOSUBDIR,     16#4000).  %% Don't use a subdirectory for the data file
-define(MDB_NOSYNC,       16#10000). %% Don't fsync after commit
-define(MDB_RDONLY,       16#20000). %% Open the environment in read-only mode
-define(MDB_NOMETASYNC,   16#40000). %% Don't fsync the meta page after commit
-define(MDB_WRITEMAP,     16#80000). %% Use a writeable memory map
-define(MDB_MAPASYNC,     16#100000). %% Use asynchronous msync when WRITEMAP is used
-define(MDB_NOTLS,        16#200000). %% Don't use Thread-Local Storage
-define(MDB_NOLOCK,       16#400000). %% Don't do any locking
-define(MDB_NORDAHEAD,    16#800000). %% Don't do readahead (no effect on Windows)
-define(MDB_NOMEMINIT,    16#1000000). %% Don't initialize malloc'd memory before writing

%% Database flags
-define(MDB_REVERSEKEY,   16#02).    %% Keys are strings to be compared in reverse order
-define(MDB_DUPSORT,      16#04).    %% Duplicate keys may be used in the database
-define(MDB_INTEGERKEY,   16#08).    %% Keys are binary integers in native byte order
-define(MDB_DUPFIXED,     16#10).    %% With DUPSORT, sorted dup items have fixed size
-define(MDB_INTEGERDUP,   16#20).    %% With DUPSORT, dups are binary integers
-define(MDB_REVERSEDUP,   16#40).    %% With DUPSORT, dup items are compared as strings in reverse order
-define(MDB_CREATE,       16#40000). %% Create DB if not already existing

%% Write flags
-define(MDB_NOOVERWRITE,  16#10).    %% For put: Don't write if the key already exists
-define(MDB_NODUPDATA,    16#20).    %% For put: Don't write if the key and data pair already exist
-define(MDB_CURRENT,      16#40).    %% For put: Replace the item at the current cursor position
-define(MDB_RESERVE,      16#10000). %% For put: Reserve space for data, don't copy it
-define(MDB_APPEND,       16#20000). %% For put: Append the given key/data pair to the end of the database
-define(MDB_APPENDDUP,    16#40000). %% For put: As above, but for sorted dup data
-define(MDB_MULTIPLE,     16#80000). %% For put: Store multiple data items in one call

%% Cursor operations
-define(MDB_FIRST,        0).        %% Position at first key/data item
-define(MDB_FIRST_DUP,    1).        %% Position at first data item of current key
-define(MDB_GET_BOTH,     2).        %% Position at key/data pair
-define(MDB_GET_BOTH_RANGE, 3).      %% Position at key, nearest data
-define(MDB_GET_CURRENT,  4).        %% Return current key/data pair
-define(MDB_GET_MULTIPLE, 5).        %% Return all the duplicate data items at the current cursor position
-define(MDB_LAST,         6).        %% Position at last key/data item
-define(MDB_LAST_DUP,     7).        %% Position at last data item of current key
-define(MDB_NEXT,         8).        %% Position at next data item
-define(MDB_NEXT_DUP,     9).        %% Position at next data item of current key
-define(MDB_NEXT_MULTIPLE, 10).      %% Return all duplicate data items at the next cursor position
-define(MDB_NEXT_NODUP,   11).       %% Position at first data item of next key
-define(MDB_PREV,         12).       %% Position at previous data item
-define(MDB_PREV_DUP,     13).       %% Position at previous data item of current key
-define(MDB_PREV_NODUP,   14).       %% Position at last data item of previous key
-define(MDB_SET,          15).       %% Position at specified key
-define(MDB_SET_KEY,      16).       %% Position at specified key, return key + data
-define(MDB_SET_RANGE,    17).       %% Position at first key greater than or equal to specified key

%% Error codes (commonly used)
-define(MDB_SUCCESS,      0).        %% Successful result
-define(MDB_KEYEXIST,     -30799).   %% Key/data pair already exists
-define(MDB_NOTFOUND,     -30798).   %% Key/data pair not found (EOF)
-define(MDB_PAGE_NOTFOUND, -30797).  %% Requested page not found
-define(MDB_CORRUPTED,    -30796).   %% Located page was wrong type
-define(MDB_PANIC,        -30795).   %% Update of meta page failed
-define(MDB_VERSION_MISMATCH, -30794). %% Environment version mismatch
-define(MDB_INVALID,      -30793).   %% File is not a valid LMDB file
-define(MDB_MAP_FULL,     -30792).   %% Environment mapsize reached
-define(MDB_DBS_FULL,     -30791).   %% Environment maxdbs reached
-define(MDB_READERS_FULL, -30790).   %% Environment maxreaders reached
-define(MDB_TLS_FULL,     -30789).   %% Too many TLS keys in use
-define(MDB_TXN_FULL,     -30788).   %% Txn has too many dirty pages
-define(MDB_CURSOR_FULL,  -30787).   %% Cursor stack too deep
-define(MDB_PAGE_FULL,    -30786).   %% Page has not enough space
-define(MDB_MAP_RESIZED,  -30785).   %% Database contents grew beyond environment mapsize
-define(MDB_INCOMPATIBLE, -30784).   %% Operation and DB incompatible, or DB type changed
-define(MDB_BAD_RSLOT,    -30783).   %% Invalid reuse of reader locktable slot
-define(MDB_BAD_TXN,      -30782).   %% Transaction must abort, has a child, or is invalid
-define(MDB_BAD_VALSIZE,  -30781).   %% Unsupported size of key/DB name/data, or wrong DUPFIXED size
-define(MDB_BAD_DBI,      -30780).   %% The specified DBI was changed unexpectedly

%% Statistics record
-record(mdb_stat, {
    psize = 0,          %% Size of a database page
    depth = 0,          %% Depth (height) of the B-tree
    branch_pages = 0,   %% Number of internal (non-leaf) pages
    leaf_pages = 0,     %% Number of leaf pages
    overflow_pages = 0, %% Number of overflow pages
    entries = 0         %% Number of data items
}).

%% Environment info record
-record(mdb_envinfo, {
    mapaddr = 0,        %% Address of map, if fixed
    mapsize = 0,        %% Size of the data memory map
    last_pgno = 0,      %% ID of the last used page
    last_txnid = 0,     %% ID of the last committed transaction
    maxreaders = 0,     %% Max reader slots in the environment
    numreaders = 0      %% Max reader slots used in the environment
}).
