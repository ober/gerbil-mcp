# Gerbil Scheme Idiom Cheat Sheet

## Core Syntax Differences from Standard Scheme

### `def` over `define`
Gerbil uses `def` as the primary definition form. Avoid `define` in idiomatic code.
```scheme
;; GOOD
(def (greet name) (string-append "Hello, " name))
(def x 42)
(def (add a b) (+ a b))

;; BAD — works but non-idiomatic
(define (greet name) (string-append "Hello, " name))
```

### Square Brackets for Lists
Square brackets `[...]` construct lists. They are syntactic sugar for `@list`.
```scheme
;; These are equivalent:
[1 2 3]          ;=> (1 2 3)
(list 1 2 3)     ;=> (1 2 3)

;; Useful in match patterns for list destructuring:
(match obj
  ([a b c] (+ a b c))
  ([x . rest] x))
```
Note: With the `:gerbil/polydactyl` prelude, brackets are plain parentheses instead.

### Hash Tables
```scheme
;; Hash literal construction
(hash ("key1" value1) ("key2" value2))

;; Hash operations (runtime builtins — no import needed)
(hash-get ht "key")              ; lookup, returns #f if missing (strictly 2 args)
(hash-ref ht "key")              ; lookup, error if missing
(hash-ref ht "key" default)      ; lookup with default
(hash-put! ht "key" value)       ; mutate in place
(hash-key? ht "key")             ; membership test
(hash->list ht)                  ; convert to alist

;; Functional update (immutable — returns new hash)
(hash-put ht "key" value)

;; Extended hash utilities
(import :std/misc/hash)
(hash-merge ht1 ht2)             ; merge two hash tables
```

### Keyword Arguments
Keywords use trailing colon syntax: `keyword:`.
```scheme
(def (connect host: "localhost" port: 8080)
  (format "~a:~a" host port))

(connect)                    ;=> "localhost:8080"
(connect host: "example.com" port: 443)
```

### `defstruct` and `defclass`
```scheme
;; Structs (product types, auto-generated constructor/accessors)
(defstruct point (x y))
(def p (make-point 1 2))
(point-x p)  ;=> 1
(point? p)   ;=> #t

;; Struct with transparent printing
(defstruct point (x y) transparent: #t)

;; Classes (support inheritance, methods, mixins)
(defclass animal (name sound))
(defclass (dog animal) (breed))

;; Constructor with :init!
(defmethod {:init! dog}
  (lambda (self name breed)
    (set! (dog-name self) name)
    (set! (dog-breed self) breed)
    (set! (dog-sound self) "woof")))
```

### `using` for Method Dispatch
```scheme
(import :std/misc/ports)
(using (o :- Writer)
  (o.write-string "hello")
  (o.write-char #\newline))
```

### `chain` / `=>` Pipe Syntax
```scheme
(import :std/sugar)
;; Thread-first macro
(chain value
  (string-append "prefix-" <>)
  (string-upcase <>)
  (string-append <> "-suffix"))
```

## Iteration with `:std/iter`
```scheme
(import :std/iter)

;; Basic for loop
(for (x (in-range 10))
  (displayln x))

;; for/collect — map equivalent
(for/collect (x (in-range 5))
  (* x x))
;=> (0 1 4 9 16)

;; for/fold — reduce equivalent
(for/fold (sum 0) (x (in-range 10))
  (+ sum x))
;=> 45

;; Parallel iteration
(for ((x (in-list '(a b c)))
      (i (in-naturals)))
  (printf "~a: ~a~n" i x))

;; Iterating hash tables
(for ((values k v) (in-hash ht))
  (printf "~a => ~a~n" k v))

;; in-producer for generators
(for (line (in-producer read-line eof-object? port))
  (displayln line))
```

## Pattern Matching
```scheme
(import :std/sugar)  ;; for when-let, if-let, etc.

(match value
  ;; Literal match
  (0 "zero")
  ;; Binding match
  (x (format "got ~a" x)))

;; Struct destructuring by name
(match pt
  ((point x y) (+ x y)))

;; List destructuring with brackets
(match lst
  ([a b c] (+ a b c))
  ([x . rest] x)
  ([] "empty"))

;; Predicate guard with ?
(match n
  ((? number? n) (+ n 1))
  ((? string? s) (string-length s)))

;; and/or patterns
(match x
  ((and (? number?) (? positive?)) "positive number")
  ((or 'yes 'true #t) "truthy"))

;; Match lambda shorthand
(def add1 (match <> ((? number? n) (+ n 1))))

;; when-let and if-let (from :std/sugar)
(when-let (val (hash-get ht "key"))
  (process val))

(if-let (val (find pred lst))
  (use val)
  (default-action))
```

## Import / Export System
```scheme
;; Basic imports
(import :std/text/json
        :std/net/httpd
        :std/sugar)

;; Selective import
(import (only-in :std/text/json read-json write-json))

;; Rename on import
(import (rename-in :std/text/json (read-json <-json)))

;; Prefix on import
(import (prefix-in :std/net/httpd http-))

;; Relative imports (within a package)
(import ./util ../common)

;; Export
(export main start-server)
(export #t)  ;; export everything
```

## Error Handling
```scheme
;; try/catch/finally
(try
  (risky-operation)
  (catch (e IOError)
    (displayln "IO error: " (error-message e)))
  (catch (e Error)
    (displayln "General error: " (error-message e)))
  (finally
    (cleanup)))

;; with-catch (lower-level)
(with-catch
  (lambda (e) (displayln "caught: " e))
  (lambda () (error "boom")))

;; raise and error
(raise (make-IOError "file not found" irritants: [filename]))
(error "something went wrong" detail1 detail2)
```

## Common Gotchas

1. **`#f` vs `'()`**: Unlike Common Lisp, `#f` and `'()` are distinct.
   `'()` is truthy! Only `#f` is false.

2. **`void` vs `#!void`**: `(void)` returns `#!void`, the void value.
   Use `(void? x)` to check.

3. **String mutability**: Gerbil strings are immutable by default.
   Use `string-copy` if you need mutation.

4. **`equal?` vs `eq?` vs `eqv?`**: Use `equal?` for structural comparison,
   `eq?` for identity, `eqv?` for value equivalence.

5. **Module single instantiation**: Gerbil modules are instantiated once.
   Side effects in module body run exactly once at load time.

6. **Tail calls**: Gerbil/Gambit fully supports proper tail calls.
   Use named `let` loops or recursion confidently.

7. **`begin` vs `begin-annotation`**: Use plain `begin` for sequencing.

8. **Semicolons in strings**: The `;` character starts a comment.
   Use `#\;` or escape in strings as needed.

9. **`hash-get` vs `hash-ref`**: `hash-get` is strictly 2-arity and returns
   `#f` on missing keys. `hash-ref` without a default throws an error on
   missing keys. Use `hash-ref` with 3 args for a custom default.
