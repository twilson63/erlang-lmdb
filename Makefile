# Makefile for LMDB Erlang NIF

LMDB_VERSION = 0.9.31
LMDB_URL = https://github.com/LMDB/lmdb/archive/LMDB_$(LMDB_VERSION).tar.gz
LMDB_DIR = lmdb-LMDB_$(LMDB_VERSION)
LMDB_LIB_DIR = $(LMDB_DIR)/libraries/liblmdb

# Erlang/OTP paths
ERL_EI_INCLUDE_DIR ?= $(shell erl -eval 'io:format("~s", [code:lib_dir(erl_interface, include)]), halt().' -noshell)
ERL_EI_LIB_DIR ?= $(shell erl -eval 'io:format("~s", [code:lib_dir(erl_interface, lib)]), halt().' -noshell)
ERL_INTERFACE_LIB_DIR ?= $(ERL_EI_LIB_DIR)
ERTS_INCLUDE_DIR ?= $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])]), halt().' -noshell)

# Compiler settings
CC = gcc
CFLAGS = -fPIC -O2 -std=c99 -finline-functions -Wall -Wmissing-prototypes
CFLAGS += -I$(ERTS_INCLUDE_DIR) -I$(ERL_EI_INCLUDE_DIR) -I$(LMDB_LIB_DIR)

# Platform-specific settings
UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
    LDFLAGS = -arch arm64 -flat_namespace -undefined suppress -bundle
else ifeq ($(UNAME_SYS), FreeBSD)
    LDFLAGS = -shared
else ifeq ($(UNAME_SYS), OpenBSD)
    LDFLAGS = -shared
else ifeq ($(UNAME_SYS), NetBSD)
    LDFLAGS = -shared
else
    LDFLAGS = -shared
endif

# Build directories
PRIV_DIR = priv
C_SRC_DIR = c_src
EBIN_DIR = ebin

# Source files
NIF_SRC = $(C_SRC_DIR)/lmdb_nif.c
NIF_SO = $(PRIV_DIR)/lmdb_nif.so
LMDB_LIB = $(LMDB_LIB_DIR)/liblmdb.a

# Erlang source files
ERL_SOURCES = $(wildcard src/*.erl)
BEAM_FILES = $(patsubst src/%.erl,$(EBIN_DIR)/%.beam,$(ERL_SOURCES))

.PHONY: all clean deps compile test

all: deps compile

deps: $(LMDB_LIB)

compile: $(NIF_SO) $(BEAM_FILES)

# Download and build LMDB
$(LMDB_DIR).tar.gz:
	curl -L -o $@ $(LMDB_URL)

$(LMDB_DIR): $(LMDB_DIR).tar.gz
	tar -xzf $<
	touch $@

$(LMDB_LIB): $(LMDB_DIR)
	$(MAKE) -C $(LMDB_LIB_DIR) liblmdb.a CC=$(CC) CFLAGS="$(CFLAGS)"

# Build NIF shared library
$(PRIV_DIR):
	mkdir -p $(PRIV_DIR)

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

$(NIF_SO): $(NIF_SRC) $(LMDB_LIB) | $(PRIV_DIR)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< $(LMDB_LIB)

# Compile Erlang modules
$(EBIN_DIR)/%.beam: src/%.erl | $(EBIN_DIR)
	erlc -o $(EBIN_DIR) $<

# Test target
test: compile
	erl -pa $(EBIN_DIR) -eval 'eunit:test(lmdb_nif_tests), halt().' -noshell

# Clean target
clean:
	rm -rf $(PRIV_DIR) $(EBIN_DIR)
	rm -rf $(LMDB_DIR) $(LMDB_DIR).tar.gz

# Development helpers
shell: compile
	erl -pa $(EBIN_DIR)

# Install development dependencies
dev-deps:
	@echo "Make sure you have the following installed:"
	@echo "- Erlang/OTP development headers"
	@echo "- GCC or compatible C compiler"
	@echo "- curl for downloading LMDB"
	@echo ""
	@echo "On Ubuntu/Debian: sudo apt-get install erlang-dev build-essential curl"
	@echo "On CentOS/RHEL: sudo yum install erlang-devel gcc make curl"
	@echo "On macOS: brew install erlang"
