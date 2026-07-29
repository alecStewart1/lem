# Undo History Bounding Implementation Plan

> **For Hermes:** Use subagent-driven-development skill to implement this plan task-by-task.

**Goal:** Cap undo history memory by limiting total size and count, with coalescing of consecutive single-character inserts.

**Architecture:** Add size tracking and eviction to `buffer-edit-history`. When the history exceeds a configurable threshold, oldest edits (beyond a separator boundary) are discarded. Coalesce adjacent single-char inserts into one edit to reduce per-keystore overhead.

**Tech Stack:** Common Lisp (SBCL), lem-core buffer/undo system

---

## Problem

The undo history (`buffer-edit-history`, `src/buffer/internal/buffer.lisp:80-82`) is an unbounded adjustable array with a fill pointer. Every edit pushes a `make-edit` struct (`src/buffer/internal/edit.lisp:6`) that stores the **full string** inserted or deleted:

```lisp
(defstruct (edit (:constructor make-edit (kind position string)))
  (kind ... :type edit-kind :read-only t)
  (position ... :type (integer 1 *))
  (string ... :type string :read-only t))
```

Issues:
1. **Unbounded memory**: Deleting a 50KB region stores 50KB in history. No cap, no compression, no eviction.
2. **No coalescing**: Typing "hello" one character at a time creates 5 separate `:insert-string` edits, each storing a 1-char string. The `:separator` boundary mechanism groups them for undo, but each edit is a separate array element + string allocation.
3. **O(history) recompute**: `recompute-undo-position-offset` (`undo.lisp:116-122`) walks the **entire** edit history + redo stack to fixup positions when undo is inhibited. This runs inside `with-inhibit-undo` blocks.

## Design

### D1: Configurable history limits

Add two buffer-level configuration variables:

```lisp
(defvar *undo-history-max-entries* 10000
  "Maximum number of edit entries in buffer-edit-history before oldest
are evicted. 0 means unlimited.")

(defvar *undo-history-max-bytes* 10485760
  "Maximum total bytes of edit strings in buffer-edit-history before
oldest are evicted. 0 means unlimited. Default: 10MB.")
```

These are global defaults. Per-buffer overrides can use buffer-local variables.

### D2: Byte tracking

Add a `history-bytes` slot to the `buffer` class, tracking the total byte count of all edit strings currently in `edit-history`. Increment on push, decrement on eviction. This avoids scanning the array on every push.

### D3: Eviction strategy

When `push-undo` would exceed either limit, evict from the oldest end:

1. Find the first `:separator` at or after the start of the array.
2. Evict all entries from array start up to and including that separator.
3. Decrement `history-bytes` by the sum of evicted edit string lengths.
4. Repeat until under both limits or only one separator group remains (never evict the current edit group — the user needs at least one undo step).

Eviction uses `array-displacement` or a start-index rather than copying. The current implementation uses a fill-pointer array. Options:

**Option A: Start index** — Add a `history-start` slot to buffer. `vector-pop` from the end (for undo). For eviction, advance `history-start`. Access: `(aref edit-history (+ history-start index))`. This avoids copying but complicates indexing.

**Option B: Replace array** — When evicting, create a new smaller array with the remaining elements. Simple but O(remaining) per eviction. Acceptable if evictions are infrequent (every ~1000 edits).

**Option C: Ring buffer** — Circular buffer with modular indexing. Most efficient but most complex.

**Recommendation: Option B** for simplicity. Evictions happen rarely (every N thousand edits), and the O(n) copy of remaining elements is small relative to the editing that filled the buffer. If profiling shows this is a bottleneck, upgrade to Option A later.

### D4: Coalescing consecutive single-char inserts

When `push-undo` receives an `:insert-string` edit where:
- The string is a single character
- The previous edit is also an `:insert-string`
- The previous edit's position + length = this edit's position
- There is no `:separator` between them

Then merge: replace the previous edit's string with `(concatenate 'string prev-string new-char)` and don't push a new entry.

This turns typing "hello" into one edit `("insert" 1 "hello")` instead of 5 edits. Memory savings: 5 struct allocations + 5 string allocations → 1 struct + 1 string (that grows). The string growth is O(n) per char (concatenate allocates), but for typical typing sessions this is negligible compared to per-edit struct overhead.

**For deletes**: Coalesce consecutive single-char deletes in the opposite direction. If deleting backward (position decreasing by 1 each time), prepend to the previous delete's string.

### D5: Separator-aware coalescing

Coalescing must not cross `:separator` boundaries. If the user called `buffer-undo-boundary` between two inserts, they expect undo to stop at that boundary. Check for `:separator` before coalescing.

### D6: recompute-undo-position-offset

`recompute-undo-position-offset` (`undo.lisp:116-122`) walks the entire history. This is called when `inhibit-undo-p` is true (inside `with-inhibit-undo` blocks) — the edit isn't pushed to history, but existing positions need fixup.

This is a separate problem from history bounding. The position-cache plan addresses the common case. For `recompute-undo-position-offset`, the fix is to make undo records use relative positions instead of absolute — but that's a larger redesign. For now, bounding the history size indirectly limits this cost (shorter history = shorter recompute walk).

**Out of scope for this plan**: Redesigning undo to use relative/delta positions. Document as future work.

---

## Tasks

### Task 1: Write failing test for history limit

**Objective:** Test that history is evicted when exceeding the entry limit.

**Files:**
- Modify: `tests/buffer/internal.lisp`

**Step 1: Write the test**

```lisp
(deftest undo-history-entry-limit
  (let* ((lem/buffer/internal::*undo-history-max-entries* 5)
         (buffer (lem:make-buffer "test" :temporary t))
         (point (lem:buffer-point buffer)))
    ;; Insert 10 characters, each as a separate undo group
    (dotimes (i 10)
      (lem:insert-character point (code-char (+ (char-code #\a) i)))
      (lem:buffer-undo-boundary buffer))
    ;; History should be bounded — undoing 5 times should work,
    ;; but the 6th undo should do nothing (already at oldest)
    (dotimes (i 5)
      (lem:buffer-undo point))
    ;; After 5 undos, we're at the oldest remaining state
    ;; The first 5 edits (a-e) were evicted, so buffer should have "fghij"
    (ok (equal "fghij" (lem:buffer-text buffer)))
    ;; 6th undo does nothing
    (lem:buffer-undo point)
    (ok (equal "fghij" (lem:buffer-text buffer)))
    (check-corruption buffer)))
```

**Step 2: Run test to verify it fails**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: FAIL — current implementation has no limit, so all 10 edits remain and 10 undos work.

**Step 3: Commit**

```bash
jj new -m "test(buffer): add undo history entry limit test"
jj squash
```

---

### Task 2: Write failing test for coalescing

**Objective:** Test that consecutive single-char inserts are coalesced.

**Files:**
- Modify: `tests/buffer/internal.lisp`

**Step 1: Write the test**

```lisp
(deftest undo-coalesce-single-char-inserts
  (let* ((buffer (lem:make-buffer "test" :temporary t))
         (point (lem:buffer-point buffer)))
    ;; Type "hello" one char at a time, no undo boundaries
    (lem:insert-character point #\h)
    (lem:insert-character point #\e)
    (lem:insert-character point #\l)
    (lem:insert-character point #\l)
    (lem:insert-character point #\o)
    ;; One undo should revert all 5 chars
    (lem:buffer-undo point)
    (ok (equal "" (lem:buffer-text buffer)))
    (check-corruption buffer)))
```

**Step 2: Run test to verify current behavior**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: May pass or fail depending on separator behavior. If `buffer-undo` undoes all edits until the last separator (and there's no separator between the 5 inserts), it already passes. If each insert is a separate undo step, it fails.

**Step 3: Commit**

```bash
jj new -m "test(buffer): add undo coalescing test"
jj squash
```

---

### Task 3: Add history-bytes slot and config variables

**Objective:** Add the tracking infrastructure.

**Files:**
- Modify: `src/buffer/internal/buffer.lisp:5-94` (add `history-bytes` slot)
- Modify: `src/buffer/internal/undo.lisp` (add config variables)

**Step 1: Add the slot to buffer**

```lisp
(history-bytes
 :initform 0
 :accessor buffer-history-bytes
 :type fixnum)
```

**Step 2: Add config variables to undo.lisp**

```lisp
(defvar *undo-history-max-entries* 10000
  "Maximum number of edit entries in buffer-edit-history before oldest
are evicted. 0 means unlimited.")

(defvar *undo-history-max-bytes* 10485760
  "Maximum total bytes of edit strings in buffer-edit-history before
oldest are evicted. 0 means unlimited. Default: 10MB.")
```

**Step 3: Verify compile + tests**

**Step 4: Commit**

```bash
jj new -m "feat(buffer): add undo history tracking slots and config"
jj squash
```

---

### Task 4: Implement byte tracking in push-undo

**Objective:** Track total bytes on push.

**Files:**
- Modify: `src/buffer/internal/undo.lisp:55-70` (`push-undo-stack`)

**Step 1: Update push-undo-stack to track bytes**

```lisp
(defun push-undo-stack (buffer elt)
  (vector-push-extend elt (buffer-edit-history buffer))
  (typecase elt
    (edit (incf (buffer-history-bytes buffer)
                (length (edit-string elt))))))
```

**Step 2: Verify tests pass**

**Step 3: Commit**

```bash
jj new -m "feat(buffer): track undo history byte count"
jj squash
```

---

### Task 5: Implement coalescing in push-undo

**Objective:** Coalesce consecutive single-char inserts/deletes.

**Files:**
- Modify: `src/buffer/internal/undo.lisp:61-70` (`push-undo`)

**Step 1: Add coalescing logic to push-undo**

In `push-undo`, before pushing, check if coalescing applies:

```lisp
(defun coalesce-edit-p (buffer edit)
  "Return T if EDIT can be coalesced with the previous edit in history."
  (let ((prev (last-edit-history buffer)))
    (and prev
         (not (eq prev :separator))
         (edit-p prev)
         (eq (edit-kind prev) (edit-kind edit))
         ;; Both must be single-char or the merge must be contiguous
         (case (edit-kind edit)
           (:insert-string
            ;; New insert continues right after previous insert
            (and (= (edit-position prev) (1- (edit-position edit)))
                 ;; Don't coalesce across separator
                 (not (eq :separator (second-last-edit-history buffer)))))
           (:delete-string
            ;; Consecutive backward deletes: position decreases by 1
            (and (= (edit-position edit) (1- (edit-position prev)))
                 (not (eq :separator (second-last-edit-history buffer)))))))))

(defun coalesce-edit (buffer edit)
  "Merge EDIT into the previous edit in history. Returns the previous edit."
  (let ((prev (last-edit-history buffer)))
    (decf (buffer-history-bytes buffer) (length (edit-string prev)))
    (setf (edit-string prev)
          (case (edit-kind edit)
            (:insert-string
             (concatenate 'string (edit-string prev) (edit-string edit)))
            (:delete-string
             (concatenate 'string (edit-string edit) (edit-string prev)))))
    (incf (buffer-history-bytes buffer) (length (edit-string prev)))
    prev))
```

Note: `edit-string` is currently `:read-only t`. Remove the `:read-only` to allow coalescing to mutate it. This is safe because the edit is only referenced from the history array.

**Step 2: Update push-undo to use coalescing**

```lisp
(defun push-undo (buffer edit)
  (when (buffer-enable-undo-p buffer)
    (ecase *undo-mode*
      (:edit
       (if (coalesce-edit-p buffer edit)
           (coalesce-edit buffer edit)
           (progn
             (push-undo-stack buffer edit)
             (maybe-evict-history buffer)))
       (setf (buffer-redo-stack buffer) nil))
      (:redo
       (push-undo-stack buffer edit))
      (:undo
       (push-redo-stack buffer edit)))))
```

**Step 3: Add helper functions**

```lisp
(defun last-edit-history (buffer)
  (let ((history (buffer-edit-history buffer)))
    (when (plusp (fill-pointer history))
      (aref history (1- (fill-pointer history))))))

(defun second-last-edit-history (buffer)
  (let ((history (buffer-edit-history buffer)))
    (when (>= (fill-pointer history) 2)
      (aref history (- (fill-pointer history) 2)))))
```

**Step 4: Remove :read-only from edit-string**

In `src/buffer/internal/edit.lisp:12-14`, remove `:read-only t` from the `string` slot.

**Step 5: Run tests**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: coalescing test passes, existing undo tests pass.

**Step 6: Commit**

```bash
jj new -m "perf(buffer): coalesce consecutive single-char edits in undo"
jj squash
```

---

### Task 6: Implement history eviction

**Objective:** Evict oldest history when limits are exceeded.

**Files:**
- Modify: `src/buffer/internal/undo.lisp` (add `maybe-evict-history`)

**Step 1: Implement maybe-evict-history**

```lisp
(defun maybe-evict-history (buffer)
  "Evict oldest undo history entries if limits are exceeded.
Never evicts the current (most recent) undo group."
  (let ((history (buffer-edit-history buffer))
        (max-entries *undo-history-max-entries*)
        (max-bytes *undo-history-max-bytes*))
    (when (or (and (plusp max-entries)
                   (> (fill-pointer history) max-entries))
              (and (plusp max-bytes)
                   (> (buffer-history-bytes buffer) max-bytes)))
      (evict-oldest-undo-group buffer))))

(defun evict-oldest-undo-group (buffer)
  "Remove the oldest undo group (up to and including the first :separator).
Uses array replacement (Option B) for simplicity."
  (let ((history (buffer-edit-history buffer)))
    ;; Find the first separator
    (let ((sep-pos (position :separator history)))
      (when (and sep-pos (< (1+ sep-pos) (fill-pointer history)))
        ;; Don't evict if it would remove the only remaining group
        (let ((evicted-bytes 0))
          (loop :for i :from 0 :to sep-pos
                :for elt := (aref history i)
                :do (typecase elt
                      (edit (incf evicted-bytes (length (edit-string elt))))))
          (decf (buffer-history-bytes buffer) evicted-bytes)
          ;; Replace array with remaining elements
          (let ((new-history (make-array (max 16 (- (fill-pointer history) sep-pos 1))
                                         :adjustable t
                                         :fill-pointer (- (fill-pointer history) sep-pos 1))))
            (loop :for i :from (1+ sep-pos)
                  :for j :from 0
                  :while (< i (fill-pointer history))
                  :do (setf (aref new-history j) (aref history i)))
            (setf (buffer-edit-history buffer) new-history)))))))
```

**Step 2: Call maybe-evict-history from push-undo**

Already added in Task 5's `push-undo` rewrite (the `:edit` branch calls `maybe-evict-history` after push).

**Step 3: Run tests**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: entry-limit test passes, all existing tests pass.

**Step 4: Commit**

```bash
jj new -m "feat(buffer): evict oldest undo history when limits exceeded"
jj squash
```

---

### Task 7: Handle redo-stack eviction

**Objective:** The redo stack also stores edits. Apply similar bounding.

**Files:**
- Modify: `src/buffer/internal/undo.lisp`

**Step 1: Track redo-stack bytes**

Add a `redo-bytes` slot to buffer, or compute on demand. The redo stack is a list (not array), so use `(reduce #'+ (redo-stack buffer) :key (lambda (e) (if (edit-p e) (length (edit-string e)) 0)))` when checking.

Alternatively, just cap the redo stack length. The redo stack is typically small (only grows during undo operations, cleared on new edits).

**Step 2: Cap redo stack**

```lisp
(defun push-redo-stack (buffer elt)
  (push elt (buffer-redo-stack buffer))
  (when (> (length (buffer-redo-stack buffer)) *undo-history-max-entries*)
    ;; Drop oldest (last in list)
    (setf (buffer-redo-stack buffer)
          (butlast (buffer-redo-stack buffer)))))
```

**Step 3: Run tests**

**Step 4: Commit**

```bash
jj new -m "feat(buffer): cap redo stack length"
jj squash
```

---

### Task 8: Add user-facing configuration

**Objective:** Expose the limits as user-configurable options.

**Files:**
- Modify: `src/config.lisp` (add config options)

**Step 1: Add config options**

```lisp
(lem:config :undo-history-max-entries 10000)
(lem:config :undo-history-max-bytes 10485760)
```

Bind the special variables from config at buffer creation time, or check config directly in `maybe-evict-history`.

**Step 2: Run tests**

**Step 3: Commit**

```bash
jj new -m "feat(buffer): expose undo history limits as config"
jj squash
```

---

## Verification

After all tasks:
1. `make test` — all tests pass
2. `make lint` — no violations
3. Manual test: open a large file, make many edits, verify undo still works and memory stays bounded (check SBCL's `--dynamic-space-size` or use `(room)` to verify)
4. Manual test: type a long string one char at a time, single undo should revert the whole string
