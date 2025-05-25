# Makefile for LMDB Erlang NIF

LMDB_VERSION = 0.9.31
LMDB_URL = https://github.com/LMDB/lmdb/archive/LMDB_$(LMDB_VERSION).tar.gz
LMDB_BASE_DIR = lmdb
LMDB_ARCHIVE = $(LMDB_BASE_DIR)/LMDB_$(LMDB_VERSION).tar.gz
LMDB_EXTRACTED_DIR = $(LMDB_BASE_DIR)/lmdb-LMDB_$(LMDB_VERSION)
LMDB_LIB_DIR = $(LMDB_EXTRACTED_DIR)/libraries/liblmdb

# Erlang/OTP paths
ERL_EI_INCLUDE_DIR ?= $(shell erl -eval 'io:format("~s", [code:lib_dir(erl_interface, include)]), halt().' -noshell)
ERL_EI_LIB_DIR ?= $(shell erl -eval 'io:format("~s", [code:lib_dir(erl_interface, lib)]), halt().' -noshell)
ERL_INTERFACE_LIB_DIR ?= $(ERL_EI_LIB_DIR)
ERTS_INCLUDE_DIR ?= $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])]), halt().' -noshell)

# Compiler settings
CC = gcc
CFLAGS = -fPIC -O2 -std=c99 -finline-functions -Wall -Wmissing-prototypes
CFLAGS += -I$(ERTS_INCLUDE_DIR) -I$(ERL_EI_INCLUDE_DIR) -I$(LMDB_LIB_DIR)

# Platform detection
UNAME_SYS := $(shell uname -s)
UNAME_ARCH := $(shell uname -m)

# Platform-specific settings
ifeq ($(UNAME_SYS), Darwin)
    # macOS settings
    LDFLAGS = -bundle -flat_namespace -undefined suppress
    # Handle both Intel and Apple Silicon
    ifeq ($(UNAME_ARCH), arm64)
        CFLAGS += -arch arm64
        LDFLAGS += -arch arm64
    else
        CFLAGS += -arch x86_64
        LDFLAGS += -arch x86_64
    endif
    # macOS specific compiler flags
    CFLAGS += -mmacosx-version-min=10.14
    # Use Homebrew paths if available
    ifneq ($(shell command -v brew 2> /dev/null),)
        BREW_PREFIX := $(shell brew --prefix)
        CFLAGS += -I$(BREW_PREFIX)/include
        LDFLAGS += -L$(BREW_PREFIX)/lib
    endif
else ifeq ($(UNAME_SYS), FreeBSD)
    LDFLAGS = -shared -fPIC
    CFLAGS += -fPIC
else ifeq ($(UNAME_SYS), OpenBSD)
    LDFLAGS = -shared -fPIC
    CFLAGS += -fPIC
else ifeq ($(UNAME_SYS), NetBSD)
    LDFLAGS = -shared -fPIC
    CFLAGS += -fPIC
else
    # Linux and other Unix-like systems
    LDFLAGS = -shared -fPIC
    CFLAGS += -fPIC
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

.PHONY: all

all: deps

deps: $(LMDB_LIB)

compile: $(NIF_SO) $(BEAM_FILES)

# Target for rebar3 - only builds NIF, lets rebar3 handle Erlang compilation
compile-nif: $(NIF_SO)

# Debug target to check LMDB status
debug-lmdb:
	@echo "=== LMDB Debug Information ==="
	@echo "LMDB_VERSION: $(LMDB_VERSION)"
	@echo "LMDB_BASE_DIR: $(LMDB_BASE_DIR)"
	@echo "LMDB_ARCHIVE: $(LMDB_ARCHIVE)"
	@echo "LMDB_EXTRACTED_DIR: $(LMDB_EXTRACTED_DIR)"
	@echo "LMDB_LIB_DIR: $(LMDB_LIB_DIR)"
	@echo "LMDB_LIB: $(LMDB_LIB)"
	@echo ""
	@echo "Checking if LMDB base directory exists..."
	@if [ -d "$(LMDB_BASE_DIR)" ]; then \
		echo "✓ LMDB base directory exists: $(LMDB_BASE_DIR)"; \
		echo "Contents:"; \
		ls -la $(LMDB_BASE_DIR)/ | head -10; \
	else \
		echo "✗ LMDB base directory not found: $(LMDB_BASE_DIR)"; \
	fi
	@echo ""
	@echo "Checking if LMDB extracted directory exists..."
	@if [ -d "$(LMDB_EXTRACTED_DIR)" ]; then \
		echo "✓ LMDB extracted directory exists: $(LMDB_EXTRACTED_DIR)"; \
		echo "Contents:"; \
		ls -la $(LMDB_EXTRACTED_DIR)/ | head -10; \
	else \
		echo "✗ LMDB extracted directory not found: $(LMDB_EXTRACTED_DIR)"; \
	fi
	@echo ""
	@echo "Checking if LMDB lib directory exists..."
	@if [ -d "$(LMDB_LIB_DIR)" ]; then \
		echo "✓ LMDB lib directory exists: $(LMDB_LIB_DIR)"; \
		echo "Contents:"; \
		ls -la $(LMDB_LIB_DIR)/; \
	else \
		echo "✗ LMDB lib directory not found: $(LMDB_LIB_DIR)"; \
	fi
	@echo ""
	@echo "Looking for lmdb.h files..."
	@find . -name "lmdb.h" 2>/dev/null || echo "No lmdb.h files found"
	@echo ""
	@echo "Looking for liblmdb.a files..."
	@find . -name "liblmdb.a" 2>/dev/null || echo "No liblmdb.a files found"

# Download and build LMDB
$(LMDB_ARCHIVE):
	@echo "Downloading LMDB $(LMDB_VERSION)..."
	@mkdir -p $(LMDB_BASE_DIR)
	@if command -v curl >/dev/null 2>&1; then \
		curl -L -o $@ $(LMDB_URL); \
	elif command -v wget >/dev/null 2>&1; then \
		wget -O $@ $(LMDB_URL); \
	else \
		echo "Error: Neither curl nor wget found. Please install one of them."; \
		exit 1; \
	fi

$(LMDB_EXTRACTED_DIR): $(LMDB_ARCHIVE)
	@echo "Extracting LMDB..."
	@cd $(LMDB_BASE_DIR) && tar -xzf $(notdir $(LMDB_ARCHIVE))
	@touch $@

$(LMDB_LIB): $(LMDB_EXTRACTED_DIR)
	@echo "Building LMDB for $(UNAME_SYS) $(UNAME_ARCH)..."
	@echo "LMDB lib directory: $(LMDB_LIB_DIR)"
	@echo "Building in: $(pwd)/$(LMDB_LIB_DIR)"
	$(MAKE) -C $(LMDB_LIB_DIR) liblmdb.a CC=$(CC) \
		CFLAGS="$(CFLAGS) -DMDB_USE_ROBUST=0" \
		AR="$(AR)" RANLIB="$(RANLIB)"
	@echo "Checking if LMDB library was built..."
	@ls -la $(LMDB_LIB_DIR)/
	@if [ -f $(LMDB_LIB) ]; then \
		echo "✓ LMDB library built successfully: $(LMDB_LIB)"; \
	else \
		echo "✗ LMDB library not found at: $(LMDB_LIB)"; \
		echo "Contents of $(LMDB_LIB_DIR):"; \
		ls -la $(LMDB_LIB_DIR)/ || true; \
		exit 1; \
	fi
	@echo "Checking for LMDB header files..."
	@if [ -f $(LMDB_LIB_DIR)/lmdb.h ]; then \
		echo "✓ LMDB header found: $(LMDB_LIB_DIR)/lmdb.h"; \
	else \
		echo "✗ LMDB header not found at: $(LMDB_LIB_DIR)/lmdb.h"; \
		find $(LMDB_EXTRACTED_DIR) -name "lmdb.h" || true; \
		exit 1; \
	fi

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
	rm -rf c_src/*.o c_src/*.d _build test
	rm -rf $(PRIV_DIR) $(EBIN_DIR)
	rm -rf $(LMDB_BASE_DIR)

# Development helpers
shell: compile
	erl -pa $(EBIN_DIR)

# Install development dependencies
dev-deps:
	@echo "Development dependencies for $(UNAME_SYS):"
ifeq ($(UNAME_SYS), Darwin)
	@echo "- Install Xcode Command Line Tools: xcode-select --install"
	@echo "- Install Erlang: brew install erlang"
	@echo "- Ensure curl is available (should be pre-installed)"
else ifeq ($(findstring BSD,$(UNAME_SYS)), BSD)
	@echo "- Install Erlang: pkg install erlang"
	@echo "- Install build tools: pkg install gmake gcc curl"
else
	@echo "- Ubuntu/Debian: sudo apt-get install erlang-dev build-essential curl"
	@echo "- CentOS/RHEL: sudo yum install erlang-devel gcc make curl"
	@echo "- Arch Linux: sudo pacman -S erlang gcc make curl"
endif
	@echo ""
	@echo "Checking current environment..."
	@command -v erl >/dev/null 2>&1 && echo "✓ Erlang found" || echo "✗ Erlang not found"
	@command -v $(CC) >/dev/null 2>&1 && echo "✓ C compiler ($(CC)) found" || echo "✗ C compiler not found"
	@command -v curl >/dev/null 2>&1 && echo "✓ curl found" || echo "✗ curl not found"
	@test -d "$(ERTS_INCLUDE_DIR)" && echo "✓ Erlang headers found" || echo "✗ Erlang headers not found at $(ERTS_INCLUDE_DIR)"

# macOS specific targets
ifeq ($(UNAME_SYS), Darwin)
install-deps-macos:
	@echo "Installing dependencies on macOS..."
	@if ! command -v brew >/dev/null 2>&1; then \
		echo "Installing Homebrew..."; \
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"; \
	fi
	@echo "Installing Erlang..."
	brew install erlang
	@echo "Dependencies installed successfully!"

check-macos:
	@echo "macOS Environment Check:"
	@echo "Architecture: $(UNAME_ARCH)"
	@echo "Xcode Command Line Tools:"
	@xcode-select -p >/dev/null 2>&1 && echo "  ✓ Installed" || echo "  ✗ Not installed (run: xcode-select --install)"
	@echo "Homebrew:"
	@command -v brew >/dev/null 2>&1 && echo "  ✓ Installed at $(brew --prefix)" || echo "  ✗ Not installed"
endif
