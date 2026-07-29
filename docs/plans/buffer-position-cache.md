# Buffer Position Cache Implementation Plan

> **For Hermes:** Use subagent-driven-development skill to implement this plan task-by-task.

**Goal:** Eliminate the O(n) `position-at-point` cost that runs on every keystroke by adding an incremental per-line character-offset cache.

**Architecture:** Add a `char-offset` slot to the `line` struct, maintained incrementally on insert/delete. `position-at-point` becomes O(1) by reading the cached offset. A periodic repair mechanism handles lines whose offset was invalidated.

**Tech Stack:** Common Lisp (SBCL), lem-core buffer system

---

## Problem

`position-at-point` (`src/buffer/internal/basic.lisp:382-387`) walks backwards from the current line to the start of the buffer:

```lisp
(defun position-at-point (point)
  (let ((offset (point-charpos point)))
    (do ((line (line:line-previous (point-line point)) (line:line-previous line)))
        ((null line) (1+ offset))
      (incf offset (1+ (line:line-length line))))))
```

This is called from `insert-string/point :around` and `delete-char/point :around` (`src/buffer/internal/buffer-insert.lisp:190, 199, 206, 217`) — i.e., on **every single edit**, because the undo record needs an absolute position.

On a 10,000-line file, editing at line 9,000 costs 9,000 linked-list traversals per keystroke.

`move-to-position` (`basic.lisp:389-398`) is similarly O(n) — it calls `character-offset` from buffer start.

## Design

### D1: Per-line cached character offset

Add a `char-offset` slot to the `line` class (`src/buffer/line.lisp:47`), storing the absolute character offset of the **start** of each line (1-indexed, matching `position-at-point`'s convention).

- Line 1: `char-offset` = 1
- Line 2: `char-offset` = 1 + (length of line 1) + 1 (for the newline)
- Line N: `char-offset` = (line N-1's char-offset) + (length of line N-1) + 1

`position-at-point` becomes:
```lisp
(defun position-at-point (point)
  (+ (line:line-char-offset (point-line point)) (point-charpos point)))
```

O(1). No traversal.

### D2: Incremental maintenance

When a line's text changes length, all subsequent lines' offsets shift. Rather than updating all subsequent lines eagerly (O(n)), use a **lazy invalidation** strategy:

- On insert/delete that changes a line's length, mark that line's `char-offset` as valid (it hasn't changed — only subsequent lines change).
- Subsequent lines' `char-offset` becomes stale. Rather than marking each one, store a "dirty-from" line number on the buffer.
- When `position-at-point` is called on a line with a stale offset, walk forward from the last known-good offset and repair.

Wait — this still has the O(n) worst case. Let me reconsider.

**Better approach: eager update of subsequent lines is too expensive. Lazy repair is the right call, but we need to bound the repair cost.**

### D2 (revised): Lazy repair with finger

Store on the buffer:
- `char-offset-valid-line`: the highest line number whose `char-offset` is known to be correct.
- Lines at or below this number have valid offsets. Lines above are stale.

On `position-at-point(point)`:
1. If the point's line number ≤ `char-offset-valid-line`, return cached offset + charpos. O(1).
2. If the point's line number > `char-offset-valid-line`, repair: walk forward from `char-offset-valid-line` to the point's line, updating each `char-offset`. Update `char-offset-valid-line` to the point's line number. O(distance) — but this distance is usually small (the user is editing near where they last queried), and the repair benefits all future queries up to that line.

On insert/delete:
1. If the edit changes a line's length (insert/delete characters, or insert/delete newlines), set `char-offset-valid-line` to `min(char-offset-valid-line, edited-line-number - 1)`.
2. The edited line's own offset doesn't change (its start position is the same). Only lines after it shift.

This means: after an edit at line L, all offsets ≥ L are potentially stale. The next `position-at-point` for a line > L triggers a repair walk from L to the queried line.

**Why this is fast in practice:** The user edits in a localized region. `position-at-point` is called on the point being edited, which is at or near the edit site. The repair walk is typically 0 lines (same line) or a few lines. The O(n) case only happens when jumping to a distant line after editing near the start — which is rare and already involves an O(n) navigation.

### D3: Initialization

When a buffer is created or a file is loaded, `char-offset-valid-line` starts at 0 (nothing valid). The first `position-at-point` call triggers a full forward walk, populating all offsets up to the queried line. Subsequent calls are O(1) until an edit invalidates them.

For file loading, we could eagerly populate all offsets during `insert-buffer` / file read. This adds O(n) to file-open time (which is already O(n)) but makes the first edit O(1). This is optional — the lazy approach works fine.

### D4: move-to-position optimization

`move-to-position` (`basic.lisp:389-398`) currently calls `character-offset` from buffer start — O(n). With cached offsets, we can binary-search if we had random access to lines, but lines are a linked list. Instead:

- Walk from the nearest known-good line (start, end, or the current point) to the target position.
- Use `char-offset` to know when we've reached the right line.

This is still O(n) in the worst case but benefits from the offset cache to skip line-length computation. A bigger win would require an array index over lines (see "Future work" below).

### D5: Corruption checker update

`check-buffer-corruption` (`src/buffer/internal/check-corruption.lisp:53-75`) walks all lines and points. Add a check that `char-offset` values are consistent (each line's offset = previous line's offset + previous line's length + 1). This catches cache bugs.

### D6: Undo interaction

`recompute-undo-position-offset` (`undo.lisp:116-122`) walks the entire edit history to fixup positions when undo is inhibited. This is a separate O(history) problem (addressed in the undo-history-bounding plan). The position cache doesn't help here directly — undo positions are absolute integers stored in `edit` structs, not line-relative.

However, `position-at-point` is called before `recompute-undo-position-offset` in the `:around` methods (`buffer-insert.lisp:190, 206`), so the cache directly speeds up the common case. The `recompute-undo-position-offset` call only happens inside `with-inhibit-undo` blocks, which are less frequent.

## Future work (out of scope for this plan)

- **Line array index**: A side-array of line pointers indexed by line number would enable O(log n) `move-to-position` via binary search on `char-offset`. This is a bigger change (maintaining the array on insert/delete-newline) and should be a separate plan.
- **Gap buffer / piece table**: Would make both position queries and random access O(log n). Major rewrite — not worth it given the linked-list design is well-tested.

---

## Tasks

### Task 1: Write failing test for cached position-at-point

**Objective:** Test that `position-at-point` returns correct values for a multi-line buffer.

**Files:**
- Modify: `tests/buffer/internal.lisp`

**Step 1: Write the test**

```lisp
(deftest position-at-point-cache
  (let* ((buffer (lem:make-buffer "test" :temporary t))
         (point (lem:buffer-point buffer)))
    (lem:insert-string point "hello\nworld\nfoo")
    ;; Position 1 = start of "hello"
    (lem:buffer-start point)
    (ok (= 1 (lem:position-at-point point)))
    ;; Position 7 = start of "world" (5 + 1 newline + 1)
    (lem:move-to-line point 2)
    (ok (= 7 (lem:position-at-point point)))
    ;; Position 13 = start of "foo" (7 + 5 + 1)
    (lem:move-to-line point 3)
    (ok (= 13 (lem:position-at-point point)))
    ;; After inserting at line 2, positions shift
    (lem:move-to-line point 2)
    (lem:insert-string point "XX")
    (lem:move-to-line point 3)
    (ok (= 15 (lem:position-at-point point)))
    (check-corruption buffer)))
```

**Step 2: Run test to verify it passes (current implementation)**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: PASS (current O(n) implementation is correct, just slow)

**Step 3: Commit**

```bash
jj new -m "test(buffer): add position-at-point cache test"
jj squash
```

---

### Task 2: Add char-offset slot to line

**Objective:** Add the cached offset slot to the line class.

**Files:**
- Modify: `src/buffer/line.lisp:47-79` (add slot)
- Modify: `src/buffer/line.lisp:1-40` (export accessor)

**Step 1: Add the slot**

In the `line` class definition, add:
```lisp
(char-offset
 :initform nil
 :initarg :char-offset
 :accessor line-char-offset)
```

`nil` means "not yet computed". A fixnum means "the absolute character offset of the start of this line".

**Step 2: Export the accessor**

Add `:char-offset` and `:line-char-offset` to the package exports in `line.lisp`.

**Step 3: Verify compile**

Run: `qlot exec ros run --eval '(asdf:load-system "lem/core")'`

**Step 4: Run existing tests**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: All pass (new slot is nil, doesn't affect behavior yet).

**Step 5: Commit**

```bash
jj new -m "feat(buffer): add char-offset slot to line"
jj squash
```

---

### Task 3: Add char-offset-valid-line tracking to buffer

**Objective:** Add the dirty-line tracking to the buffer class.

**Files:**
- Modify: `src/buffer/internal/buffer.lisp:5-94` (add slot)

**Step 1: Add the slot**

In the `buffer` class definition, add:
```lisp
(char-offset-valid-line
 :initform 0
 :accessor buffer-char-offset-valid-line
 :type fixnum)
```

`0` means "no lines have valid offsets". `N` means "lines 1..N have valid offsets".

**Step 2: Verify compile + tests**

**Step 3: Commit**

```bash
jj new -m "feat(buffer): add char-offset-valid-line tracking slot"
jj squash
```

---

### Task 4: Implement cached position-at-point

**Objective:** Rewrite `position-at-point` to use the cache.

**Files:**
- Modify: `src/buffer/internal/basic.lisp:382-387`

**Step 1: Write the repair function**

```lisp
(defun repair-char-offsets (buffer target-linum)
  "Walk forward from the last valid line to TARGET-LINUM, updating char-offsets.
Lines at or below buffer-char-offset-valid-line are already correct."
  (let ((valid-line (buffer-char-offset-valid-line buffer)))
    (when (< valid-line target-linum)
      (let ((start-line
              (if (zerop valid-line)
                  (point-line (buffer-start-point buffer))
                  (line-next-n (point-line (buffer-start-point buffer))
                               (1- valid-line)))))
        ;; Set line 1's offset
        (when (zerop valid-line)
          (setf (line:line-char-offset start-line) 1)
          (setf valid-line 1))
        ;; Walk forward from valid-line to target-linum
        (do ((line (line:line-next start-line) (line:line-next line))
             (linum (1+ valid-line) (1+ linum)))
            ((or (null line) (> linum target-linum))
             (setf (buffer-char-offset-valid-line buffer) (1- linum)))
          (setf (line:line-char-offset line)
                (+ (line:line-char-offset (line:line-previous line))
                   (1+ (line:line-length (line:line-previous line)))))))))
  target-linum)
```

**Step 2: Rewrite position-at-point**

```lisp
(defun position-at-point (point)
  "Return the offset of 'point' from the beginning of the buffer."
  (let ((buffer (point-buffer point))
        (linum (point-linum point)))
    (repair-char-offsets buffer linum)
    (+ (line:line-char-offset (point-line point)) (point-charpos point))))
```

**Step 3: Run tests**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: position-at-point-cache test passes, all other tests pass.

**Step 4: Commit**

```bash
jj new -m "perf(buffer): cache position-at-point with lazy repair"
jj squash
```

---

### Task 5: Invalidate cache on edit

**Objective:** Mark the cache as stale when lines change length.

**Files:**
- Modify: `src/buffer/internal/buffer-insert.lisp` (in `insert-string/point` and `delete-char/point`)

**Step 1: Add invalidation to insert-string/point**

In the `insert-string/point` primary method (`buffer-insert.lisp:99-119`), after the insert loop, invalidate the cache if any line lengths changed:

```lisp
;; After the insert loop, if newlines were inserted (offset-line > 0),
;; or if text was inserted on a line, subsequent offsets are stale.
(when (> (point-linum point) 0)
  (setf (buffer-char-offset-valid-line (point-buffer point))
        (min (buffer-char-offset-valid-line (point-buffer point))
             (1- (point-linum point)))))
```

Actually, the insert could change the current line's length (if no newline is inserted) which shifts all subsequent lines. Or it could insert newlines, which shifts all subsequent lines AND renumbers them. In both cases, lines after the edit point are stale.

The simplest correct invalidation: set `char-offset-valid-line` to `min(current, edited-linum - 1)`. This means "lines before the edit are still valid, lines at or after the edit are stale."

**Step 2: Add invalidation to delete-char/point**

Same pattern in `delete-char/point` (`buffer-insert.lisp:121-150`).

**Step 3: Handle line-number changes from newline insert/delete**

When `insert-newline` or `merge-with-next-line` is called, all subsequent lines' line numbers change. The `char-offset` cache tracks character offsets, not line numbers — but `char-offset-valid-line` is a line number. After a newline insertion at line L, lines > L are renumbered and their offsets are stale. Set `char-offset-valid-line` to `min(current, L-1)`.

The `buffer-nlines` is already incremented/decremented in the insert/delete methods. Add the invalidation there.

**Step 4: Run tests**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: position-at-point-cache test passes (including the "after inserting at line 2, positions shift" assertion).

**Step 5: Commit**

```bash
jj new -m "perf(buffer): invalidate position cache on edit"
jj squash
```

---

### Task 6: Update corruption checker

**Objective:** Add char-offset consistency checks to the corruption checker.

**Files:**
- Modify: `src/buffer/internal/check-corruption.lisp:34-40`

**Step 1: Add offset check to check-lines-corruption**

In the loop that walks lines, add:
```lisp
(when (line:line-char-offset line)
  ;; Verify offset is consistent with previous line
  (when (and prev-line (line:line-char-offset prev-line))
    (debug-assert (= (line:line-char-offset line)
                     (+ (line:line-char-offset prev-line)
                        (1+ (line:line-length prev-line))))
                  "char-offset is inconsistent"
                  line)))
```

**Step 2: Run tests**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: No corruption warnings.

**Step 3: Commit**

```bash
jj new -m "test(buffer): add char-offset consistency to corruption checker"
jj squash
```

---

### Task 7: Write performance benchmark test

**Objective:** Add a test that demonstrates the performance improvement.

**Files:**
- Create: `tests/buffer/perf-position-cache.lisp`

**Step 1: Write the benchmark**

```lisp
(deftest position-at-point-large-file-perf
  ;; Create a 5000-line buffer
  (let* ((buffer (lem:make-buffer "perf-test" :temporary t))
         (point (lem:buffer-point buffer)))
    (dotimes (i 5000)
      (lem:insert-string point (format nil "line ~D~%" i)))
    ;; Move to the last line
    (lem:buffer-end point)
    ;; Time position-at-point (should be fast with cache)
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (lem:position-at-point point))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))
        ;; 100 calls should take < 0.01s with cache (would be ~5s without)
        (ok (< elapsed 0.1)
            (format nil "position-at-point 100x on 5000-line file: ~Fs" elapsed))))))
```

**Step 2: Run the benchmark**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: PASS (< 0.1s for 100 calls)

**Step 3: Commit**

```bash
jj new -m "test(buffer): add position-at-point performance benchmark"
jj squash
```

---

## Verification

After all tasks:
1. `make test` — all tests pass
2. `make lint` — no internal symbol access violations
3. Manual test: open a large file (>5000 lines), scroll to the end, type — should be noticeably faster than before
