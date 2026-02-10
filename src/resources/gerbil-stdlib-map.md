# Gerbil Standard Library Map

Organized by domain. Import with `:std/...` paths.

## Text Processing
| Module | Description |
|--------|-------------|
| `:std/text/json` | JSON read/write (`read-json`, `write-json`, `json-object->string`) |
| `:std/text/csv` | CSV parsing and generation |
| `:std/text/base64` | Base64 encoding/decoding |
| `:std/text/base58` | Base58 encoding/decoding |
| `:std/text/hex` | Hexadecimal encoding/decoding |
| `:std/text/utf8` | UTF-8 utilities |
| `:std/text/zlib` | Zlib compression/decompression |
| `:std/format` | `format` (Common Lisp style: `~a`, `~s`, `~d`, `~n`) |
| `:std/pregexp` | Portable regular expressions |
| `:std/srfi/13` | String library (SRFI-13) |

## Networking
| Module | Description |
|--------|-------------|
| `:std/net/httpd` | HTTP server (handlers, routing) |
| `:std/net/request` | HTTP client (`http-get`, `http-post`, etc.) |
| `:std/net/websocket` | WebSocket client and server |
| `:std/net/uri` | URI parsing and construction |
| `:std/net/address` | Network address utilities |
| `:std/net/ssl` | SSL/TLS support |
| `:std/os/socket` | Low-level TCP/UDP sockets |

## Concurrency & Actors
| Module | Description |
|--------|-------------|
| `:std/actor` | Actor-oriented programming (`<-`, `-->`, `!!`, `->>`) |
| `:std/actor/message` | Message types and protocols |
| `:std/actor/proto` | Actor protocol definitions |
| `:std/event` | Event-driven programming |
| `:std/misc/channel` | Go-style channels |
| `:std/misc/barrier` | Synchronization barriers |
| `:std/misc/completion` | Completion tokens / futures |

Note: `spawn` and `spawn/name` are runtime builtins, available without import.

## Data Structures
| Module | Description |
|--------|-------------|
| `:std/misc/hash` | Extended hash table operations |
| `:std/misc/list` | Extended list operations |
| `:std/misc/alist` | Association list utilities |
| `:std/misc/pqueue` | Priority queues |
| `:std/misc/deque` | Double-ended queues |
| `:std/misc/queue` | FIFO queues |
| `:std/misc/red-black` | Red-black trees |
| `:std/sort` | Sorting (stable merge sort) |
| `:std/srfi/1` | List library (SRFI-1) |
| `:std/srfi/125` | Hash tables (SRFI-125) |
| `:std/srfi/133` | Vectors (SRFI-133) |

## Iteration
| Module | Description |
|--------|-------------|
| `:std/iter` | Iteration framework (`for`, `for/collect`, `for/fold`, `in-range`, `in-list`, `in-hash`, `in-producer`) |

## I/O & Files
| Module | Description |
|--------|-------------|
| `:std/misc/ports` | Port utilities (`read-file-string`, `read-file-lines`, `read-all-as-string`) |
| `:std/misc/path` | Path manipulation |
| `:std/os/temporaries` | Temporary file creation |
| `:std/os/pipe` | OS pipes |
| `:std/os/fd` | File descriptor operations |
| `:std/os/fcntl` | File control operations |

## OS & System
| Module | Description |
|--------|-------------|
| `:std/os/signal` | Signal handling (SIGTERM, SIGINT, etc.) |
| `:std/os/signal-handler` | Signal handler registration |
| `:std/os/env` | Environment variables (`getenv`, `setenv`) |
| `:std/os/socket` | OS-level socket API |
| `:std/os/epoll` | Linux epoll interface |
| `:std/os/inotify` | Linux file system notifications |
| `:std/os/hostname` | Hostname utilities |
| `:std/os/pid` | Process ID utilities |
| `:std/misc/process` | Process execution (`run-process`, `invoke`) |

## Database
| Module | Description |
|--------|-------------|
| `:std/db/dbi` | Database interface (generic) |
| `:std/db/sqlite` | SQLite driver |
| `:std/db/postgresql` | PostgreSQL driver |
| `:std/db/conpool` | Connection pooling |

## Cryptography
| Module | Description |
|--------|-------------|
| `:std/crypto` | Cryptographic primitives (hashing, ciphers, signing) |
| `:std/crypto/digest` | Message digests (SHA, MD5) |
| `:std/crypto/cipher` | Symmetric ciphers |
| `:std/crypto/bn` | Big number operations |
| `:std/crypto/dh` | Diffie-Hellman |
| `:std/crypto/libcrypto` | Low-level OpenSSL bindings |

## Syntax & Macros
| Module | Description |
|--------|-------------|
| `:std/sugar` | Syntactic sugar (`chain`, `when-let`, `if-let`, `try`, `hash`, `assert!`, etc.) |
| `:std/stxutil` | Syntax utilities for macro writers |

## Logging & Debugging
| Module | Description |
|--------|-------------|
| `:std/logger` | Logging framework (`deflogger`, `start-logger!`, log levels) |
| `:std/debug/DBG` | Debug printing |
| `:std/debug/heap` | Heap inspection |
| `:std/debug/threads` | Thread debugging |
| `:std/misc/repr` | Value representation / pretty printing |

## Testing
| Module | Description |
|--------|-------------|
| `:std/test` | Test framework (`test-suite`, `test-case`, `check-equal?`, `run-tests!`) |

## Serialization
| Module | Description |
|--------|-------------|
| `:std/misc/bytes` | Byte vector utilities |
| `:std/misc/number` | Number conversion utilities |
| `:std/misc/uuid` | UUID generation |

## Foreign Function Interface
| Module | Description |
|--------|-------------|
| `:std/foreign` | FFI utilities |

FFI is done via `begin-foreign` blocks and `extern` declarations that bind
C functions to Scheme identifiers. See `gerbil_ffi_inspect` tool for analysis.

## SRFIs (Scheme Requests for Implementation)
Common SRFIs available: 1, 2, 4, 6, 8, 9, 13, 14, 16, 18, 19, 26, 27, 28, 29,
31, 38, 39, 41, 42, 43, 48, 60, 62, 64, 66, 69, 71, 78, 95, 111, 113, 115,
117, 121, 124, 125, 127, 128, 130, 132, 133, 134, 135, 141, 143, 144, 145,
151, 152, 154, 156, 158, 159, 160, 171, 173, 174, 175, 179, 180, 196, 217,
227, 229, 232, 236, 239

Import as `:std/srfi/N` (e.g., `:std/srfi/1` for the list library).
