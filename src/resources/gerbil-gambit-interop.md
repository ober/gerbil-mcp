# Gerbil-Gambit Interop Guide

Gerbil is built on top of Gambit Scheme. Understanding the boundary helps
avoid confusion and write idiomatic code.

## The Relationship

- **Gambit** is the runtime: compiler, garbage collector, threads, I/O, C FFI
- **Gerbil** adds: module system, macro system (syntax-case), object system,
  standard library, actor framework, package manager
- Gambit primitives are always available but may be shadowed by Gerbil equivalents

## Accessing Gambit Primitives

### `##` Prefix
Gambit internal (private) procedures use `##` prefix:
```scheme
(##car pair)          ;; Gambit's unchecked car
(##vector-ref v i)    ;; unchecked vector-ref
```
**Warning**: `##` procedures skip safety checks. Use only in performance-critical
code after validation.

### Importing Gambit
```scheme
(import :gerbil/gambit)
```
This makes Gambit-specific exports available (threads, ports, etc.).

## Threading: Gambit vs Gerbil

### Gambit Threads (Low Level)
```scheme
(import :gerbil/gambit)

(thread-start! (make-thread (lambda () ...)))
(thread-join! th)
(thread-sleep! seconds)
(thread-send th msg)
(thread-receive)
(thread-receive timeout default)
```

### Gerbil Actors (High Level)
```scheme
(import :std/actor)
;; spawn is a runtime builtin — no import needed
(spawn fn)
(<- (pattern body) ...)
(--> reply)
(->> actor message)
(!! actor message)
```

**Rule of thumb**: Use actors for structured communication. Use raw threads for
simple parallelism or when you need direct control.

## I/O: Gambit vs Gerbil

### Gambit Ports
```scheme
;; Gambit-style port operations
(open-input-file "path")
(open-output-file "path")
(open-input-string "data")
(open-output-string)
(get-output-string port)
(read-line port)
(read port)
(write obj port)
(display obj port)
(close-port port)
(call-with-input-file "path" proc)
(call-with-output-file "path" proc)
```

### Gerbil Port Utilities
```scheme
(import :std/misc/ports)
(read-file-string "path")     ;; whole file as string
(read-file-lines "path")      ;; file as list of lines
(read-all-as-string port)     ;; read port to string
```

**Rule of thumb**: Use `:std/misc/ports` for convenience. Use Gambit ports
when you need fine-grained control.

## FFI: Gambit's C Interface

Gerbil provides FFI through Gambit's C interface, wrapped in Gerbil's
module system.

### `begin-foreign` Block
Embed C code directly:
```scheme
(begin-foreign
  (c-declare "#include <unistd.h>")
  (c-define-type pid_t int)
  (define getpid (c-lambda () pid_t "getpid")))
```

### `extern` Declarations
Bind Gambit-compiled C functions:
```scheme
(extern
  (my-c-fn my_c_function))
```

### `c-lambda`
Create a Scheme procedure from a C expression:
```scheme
(def get-pid (c-lambda () int "getpid"))
(def c-strlen (c-lambda (char-string) int "strlen"))
```

### `c-define`
Export a Scheme procedure callable from C:
```scheme
(c-define (scheme-callback x) (int) int "scheme_callback" ""
  (* x 2))
```

## `declare` Blocks for Optimization

Gambit compiler declarations affect code generation:
```scheme
(declare
  (not safe)             ;; disable runtime checks (dangerous!)
  (standard-bindings)    ;; assume standard bindings
  (extended-bindings)    ;; assume Gambit extended bindings
  (fixnum)               ;; assume fixnum arithmetic
  (block))               ;; allow block compilation
```

**Warning**: `(not safe)` removes all runtime type checks. Use only in
well-tested, performance-critical inner loops.

### Safe Performance
```scheme
;; Prefer targeted declarations:
(declare (fixnum))  ;; just arithmetic optimization

;; Or use Gerbil's type annotations where available
```

## Reader Extensions

### Gambit Reader Syntax
```scheme
#!eof             ;; end-of-file object
#!void            ;; void value
#!optional        ;; optional parameter marker
#!rest            ;; rest parameter marker
#!key             ;; keyword parameter marker
#\space           ;; character literal
#u8(1 2 3)        ;; u8vector literal
#s8(...)          ;; s8vector
#f32(...)         ;; f32vector
#f64(...)         ;; f64vector
```

### Gerbil-Specific Reader
```scheme
[1 2 3]           ;; list literal (expands to @list in :gerbil/core)
key:              ;; keyword (trailing colon)
#;expr            ;; datum comment (skip next form)
```

## When to Use Gambit Directly

| Situation | Use |
|-----------|-----|
| General application code | Gerbil's `:std/*` libraries |
| Threading/concurrency | Gerbil actors (`:std/actor`) |
| String/list operations | Gerbil or SRFI libs |
| Low-level I/O | Gambit ports |
| C interop / FFI | Gambit's `c-lambda`, `begin-foreign` |
| Performance tuning | Gambit's `declare`, `##` primitives |
| System calls | Gerbil's `:std/os/*` (wraps Gambit FFI) |
| Process execution | `:std/misc/process` (`run-process`, `invoke`) |
| Module organization | Always use Gerbil's module system |

## Common Mistakes

1. **Using `##` procedures in normal code** — unsafe, hard to debug.
   Only use after profiling shows it matters.

2. **Confusing Gambit's `define-type` with Gerbil's `defstruct`** — they are
   different systems. Use `defstruct`/`defclass` in Gerbil code.

3. **Using Gambit's reader macros in modules** — some Gambit reader extensions
   may not work in Gerbil module context. Test carefully.

4. **Mixing Gambit's exception system with Gerbil's** — Gerbil uses its own
   error/exception hierarchy. Use `try`/`catch`/`with-catch` consistently.

5. **Forgetting `(import :gerbil/gambit)`** — Gambit-specific procedures
   need this import to be visible.
