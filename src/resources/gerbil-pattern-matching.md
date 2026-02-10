# Gerbil Pattern Matching Reference

Gerbil provides a comprehensive pattern matching facility via `match` and related
forms. Pattern matching is pervasive in idiomatic Gerbil code.

## Basic `match` Syntax

```scheme
(match expr
  (pattern1 body1 ...)
  (pattern2 body2 ...)
  ...
  (else default-body ...))
```

## Pattern Types

### Literal Patterns
Match exact values.
```scheme
(match x
  (#t "true")
  (#f "false")
  (0 "zero")
  ('foo "the symbol foo")
  ("hello" "the string hello"))
```

### Variable Binding Patterns
A bare identifier binds the matched value.
```scheme
(match x
  (n (+ n 1)))  ;; n binds to whatever x is
```

### Wildcard Pattern
`_` matches anything, binds nothing.
```scheme
(match pair
  ([_ b] b))  ;; ignore first element
```

### List Destructuring (Brackets)
Square brackets destructure lists symmetrically to construction.
```scheme
(match lst
  ([] "empty list")
  ([x] (format "singleton: ~a" x))
  ([a b] (format "pair: ~a ~a" a b))
  ([a b c] (format "triple: ~a ~a ~a" a b c))
  ([x . rest] (format "head: ~a tail: ~a" x rest)))
```

### Cons / Dotted Pair Patterns
```scheme
(match pair
  ([a . b] (+ a b)))    ;; destructure cons cell
```

### Vector Patterns
```scheme
(match vec
  (#(a b c) (+ a b c)))
```

### Box Patterns
```scheme
(match bx
  (#&val val))
```

### Struct/Class Destructuring
Destructure by struct/class name. Fields are positional.
```scheme
(defstruct point (x y))

(match pt
  ((point x y) (sqrt (+ (* x x) (* y y)))))

(defstruct (point-3d point) (z))
(match pt
  ((point-3d x y z) (+ x y z))
  ((point x y) (+ x y)))
```

### Predicate Patterns (`?`)
Test a value with a predicate function.
```scheme
(match x
  ((? number?) "it's a number")
  ((? string?) "it's a string")
  ((? null?) "it's null"))

;; With binding
(match x
  ((? number? n) (+ n 1))
  ((? string? s) (string-length s)))

;; With lambda predicate
(match x
  ((? (lambda (n) (> n 0)) n) (format "positive: ~a" n)))
```

### `and` Patterns
All sub-patterns must match. Useful for combining predicates with binding.
```scheme
(match x
  ((and (? number?) (? positive?) n)
   (format "positive number: ~a" n)))
```

### `or` Patterns
Any sub-pattern can match.
```scheme
(match x
  ((or 'yes 'true #t) "truthy")
  ((or 'no 'false #f) "falsy"))
```

### `not` Patterns
Negation — matches if the sub-pattern does NOT match.
```scheme
(match x
  ((not #f) "truthy"))
```

### Quasiquote Patterns
Match list structures using quasiquote/unquote.
```scheme
(match expr
  (`(if ,test ,then ,else) (format "conditional"))
  (`(lambda ,args ,body) (format "lambda")))
```

### `cons` Pattern
```scheme
(match x
  ((cons a b) (format "~a . ~a" a b)))
```

### Ellipsis Patterns (`...`)
Match zero or more elements.
```scheme
(match lst
  ([a b ...] (format "head: ~a rest: ~a" a b)))
```

## Match Lambda (`match <>`)

Creates an anonymous function that pattern matches its argument.
```scheme
(def car+cdr
  (match <>
    ([a . b] (values a b))))

(car+cdr '(1 2 3))  ;=> 1 (2 3)

;; Used with map, filter, etc.
(map (match <>
       ([k v] (format "~a=~a" k v)))
     '((a 1) (b 2) (c 3)))
;=> ("a=1" "b=2" "c=3")
```

## `match*` — Multi-value Match

Match multiple values simultaneously.
```scheme
(match* (x y)
  ((0 _) "x is zero")
  ((_ 0) "y is zero")
  ((a b) (+ a b)))
```

## Related Forms from `:std/sugar`

### `when-let`
Bind and execute body only if the binding is truthy.
```scheme
(import :std/sugar)
(when-let (val (hash-get ht key))
  (process val))
```

### `if-let`
Bind and branch.
```scheme
(if-let (val (find pred lst))
  (use val)
  (handle-missing))
```

### `when-let*`
Sequential binding, short-circuits on `#f`.
```scheme
(when-let* ((a (hash-get ht "x"))
            (b (hash-get ht "y")))
  (+ a b))
```

## `case` (Standard Scheme)
For simple value dispatch, `case` can be simpler than `match`.
```scheme
(case x
  ((1 2 3) "small")
  ((4 5 6) "medium")
  (else "large"))
```

## `cond` with `=>` Arrow
```scheme
(cond
  ((assoc key alist) => cdr)
  (else default))
```

## Best Practices

1. **Prefer `match` over nested `if`/`cond`** for destructuring.
2. **Use bracket patterns** `[a b c]` for lists — they're clearer than `(cons ...)`.
3. **Use `match <>`** for inline lambda pattern matching.
4. **Always include `else`** or a catch-all pattern to avoid match failures.
5. **Struct patterns** are preferred over accessor chains for multi-field access.
6. **Combine `?` with `and`** for type-checked bindings.
