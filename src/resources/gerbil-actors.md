# Gerbil Actor System Reference

Gerbil builds on Gambit's thread messaging primitives to provide actor-oriented
programming. Actors are threads that communicate via message passing.

## Core Concepts

- **Actor**: A thread running a message loop
- **Message**: Any Scheme value sent between actors
- **Reaction (`<-`)**: Pattern-match on incoming messages
- **Reply (`-->`)**: Send a value back to the sender
- **Fire-and-forget (`->>`)**: Send a message, don't wait
- **RPC (`!!`)**: Send a message and wait for a reply

## Imports

```scheme
;; Actor message-passing primitives
(import :std/actor)

;; Note: spawn and spawn/name are Gerbil runtime builtins —
;; they do NOT require importing :std/actor
```

## Spawning Actors

`spawn` and `spawn/name` are runtime builtins available without any import.

```scheme
;; Basic spawn
(def my-actor (spawn my-actor-function))

;; Spawn with name (useful for debugging)
(def my-actor (spawn/name 'worker my-actor-function))
```

## Message Passing Operators

These require `(import :std/actor)`.

### `->>` Fire and Forget
Send a message to an actor without waiting for a response.
```scheme
(->> actor 'increment)
(->> actor (list 'set-value 42))
```

### `!!` RPC (Remote Procedure Call)
Send a message and wait for the actor to reply with `-->`.
```scheme
(def result (!! actor 'get-value))
```

### `->` Send
Low-level send (thread-send).
```scheme
(-> actor message)
```

## Receiving Messages

### `<-` Reaction
Pattern match on the next incoming message.
```scheme
(def (echo-actor)
  (<-
    ((list 'echo msg)
     (--> msg)               ;; reply to sender
     (echo-actor))           ;; loop
    ('quit (void))))         ;; stop
```

### `<<` Non-blocking Receive
```scheme
;; Try to receive, with timeout
(<< (message (handle message))
    (timeout: 5 (displayln "timed out")))
```

## Common Actor Patterns

### Stateful Loop
```scheme
(def (counter-actor)
  (let loop ((count 0))
    (<-
      ('increment
        (loop (+ count 1)))
      ('decrement
        (loop (- count 1)))
      ('get
        (--> count)
        (loop count))
      ('stop (void)))))
```

### Request-Reply with Structured Messages
```scheme
(def (kv-store-actor)
  (let loop ((store (hash)))
    (<-
      ((list 'get key)
        (--> (hash-ref store key #f))
        (loop store))
      ((list 'put key value)
        (let (new-store (hash-put store key value))
          (--> 'ok)
          (loop new-store)))
      ((list 'delete key)
        (let (new-store (hash-remove store key))
          (--> 'ok)
          (loop new-store)))
      ('keys
        (--> (hash-keys store))
        (loop store))
      ('stop
        (--> 'stopped)))))
```

### Supervisor Pattern
```scheme
(def (supervisor actors)
  (for-each
    (lambda (actor-fn)
      (spawn
        (lambda ()
          (let loop ()
            (with-catch
              (lambda (e)
                (displayln "Actor crashed, restarting: " e)
                (loop))
              actor-fn)))))
    actors))
```

### Worker Pool
```scheme
(def (worker-pool n worker-fn)
  (for/collect (_ (in-range n))
    (spawn worker-fn)))

(def (dispatch-to-pool pool msg)
  (let (worker (list-ref pool (random-integer (length pool))))
    (->> worker msg)))
```

## Actor Servers and Ensembles

For distributed actor programming across processes/machines.

### Starting an Actor Server
```scheme
(import :std/actor)

;; Start local actor server
(start-actor-server!)

;; Start named server on a specific address
(start-actor-server! "my-server"
  addresses: ["127.0.0.1:9000"])
```

### Registering Named Actors
```scheme
;; Register an actor by name on the server
(def my-actor (spawn counter-actor))
(register-actor! 'counter my-actor)
```

### Remote Actor Handles
```scheme
;; Create a handle to a remote actor
(def remote-counter
  (make-handle
    (current-actor-server)
    (reference 'remote-server 'counter)))

;; Use it like a local actor
(!! remote-counter 'get)
(->> remote-counter 'increment)
```

### Ensemble Management
```scheme
;; From the command line:
;; $ gerbil ensemble shutdown -f
;; $ gerbil ensemble status

;; Programmatic
(import :std/actor/message)
```

## Gambit Threading Primitives (Lower Level)

When you need direct thread control without actor abstractions.

```scheme
(import :gerbil/gambit)

;; Thread creation
(def th (thread-start! (make-thread (lambda () (displayln "hi")))))
(thread-join! th)

;; Thread sleep
(thread-sleep! 2)  ;; seconds (can be float)

;; Mailbox (built into every thread)
(thread-send th 'message)
(thread-receive)            ;; blocking
(thread-receive 5 'timeout) ;; with timeout

;; Mutexes
(def m (make-mutex 'my-mutex))
(mutex-lock! m)
(mutex-unlock! m)
;; Or use with-lock:
(with-lock m (lambda () (critical-section)))

;; Condition variables
(def cv (make-condition-variable 'my-cv))
(condition-variable-signal! cv)
(condition-variable-broadcast! cv)
(mutex-unlock! m cv)  ;; atomically unlock and wait
```

## When to Use What

| Need | Use |
|------|-----|
| Simple concurrent task | `spawn` + `thread-join!` |
| Message-driven component | Actor with `<-` loop |
| Request/reply service | Actor with `!!` and `-->` |
| Shared mutable state | Actor (preferred) or mutex |
| Cross-process communication | Actor server + handles |
| CPU-bound parallelism | Thread pool or worker actors |
| I/O-bound concurrency | Actors or Gambit threads (green) |

## Best Practices

1. **Prefer actors over shared state** — message passing avoids race conditions.
2. **Keep actor message loops tail-recursive** — use `let loop` pattern.
3. **Always handle unknown messages** — add a catch-all to `<-` to avoid hangs.
4. **Use `!!` sparingly** — it blocks the calling thread. Prefer `->>` when possible.
5. **Name your actors** — `spawn/name` makes debugging much easier.
6. **Handle actor crashes** — use supervisor patterns or `with-catch` in loops.
7. **Avoid blocking I/O in actors** — use separate threads or async I/O.
