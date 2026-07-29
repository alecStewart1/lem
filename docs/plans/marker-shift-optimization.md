# Marker Shift Optimization Implementation Plan

> **For Hermes:** Use subagent-driven-development skill to implement this plan task-by-task.

**Goal:** Reduce the O(n) cost of `shift-markers` by batching marker updates per line and skipping irrelevant markers using a sorted-by-position index.

**Architecture:** Replace the per-line marker list scan with a per-buffer sorted marker index that allows early termination when all affected markers have been found. For single-line edits, use the line's existing point list but sort it by charpos. For multi-line edits, walk the buffer's point list but skip entire line groups whose line number is below the edit.

**Tech Stack:** Common Lisp (SBCL), lem-core buffer/point system

---

## Problem

`shift-markers` (`src/buffer/internal/buffer-insert.lisp:39-97`) is called on every insert/delete. It adjusts the position of all points/markers affected by the edit.

**Single-line insert** (`offset-line = 0, offset-char > 0`):
```lisp
(dolist (p (line:line-points (point-line point)))
  (when (etypecase (point-kind p) ...)
    (incf (point-charpos p) offset-char)))
```
Scans all points on the current line. If the line has many markers (overlays, bookmarks, multiple cursors), this is O(markers-on-line).

**Multi-line insert** (`offset-line > 0`):
```lisp
(dolist (p (buffer-points (point-buffer point)))
  ...)
```
Scans **all points in the entire buffer**. For buffers with many overlays, this is O(total-markers) per edit.

**Delete** has the same structure — O(markers-on-line) for single-line, O(total-markers) for multi-line.

## Design

### D1: The problem is inherent to the marker model

Every marker that's at or after the edit position must be adjusted. There's no way around examining each affected marker. The question is: how do we find the affected markers efficiently?

Current approach: scan all markers on the line (single-line) or all markers in the buffer (multi-line), filtering by position.

**The real cost is the scan itself** — walking a linked list and checking a position predicate for each marker, even markers that don't need adjustment.

### D2: Sorted-by-charpos point list per line

For single-line edits, the key optimization is **early termination**. If points on a line are sorted by charpos, we can stop scanning once we reach a point whose charpos is before the edit position (for `:right-inserting`) or at the edit position (for `:left-inserting`).

Currently, `line-points` is a plain list, pushed to on point creation (`point.lisp:79`: `(push point (line:line-points ...))`). It's unordered.

**Option A: Keep sorted on insert.** When a point is added to a line, insert it in sorted-by-charpos position. Cost: O(n) insertion (list walk to find position). But point creation is less frequent than point shifting, so this is a net win if shifting happens more often.

**Option B: Sort lazily on first shift.** Keep the list unordered. On the first `shift-markers` call since the last modification, sort the list by charpos, then use early termination on subsequent calls. Mark the list as sorted. Any new point addition marks it as unsorted.

**Option C: Don't sort — use a skip list or tree.** Overkill for typical line marker counts (usually < 20).

**Recommendation: Option B** — lazy sort. Most lines have few markers, and the sort cost is negligible. The early termination benefit applies to the common case: editing at the end of a line with markers earlier on the line.

Actually, reconsider: most lines have 1-3 markers (start-point, end-point, current point). The scan is already O(1-3). Sorting adds complexity for minimal gain on typical lines. **The real win is for multi-line edits**, where we scan the entire buffer's point list.

### D3 (revised): Focus on multi-line edit optimization

For multi-line edits (newline insertion/deletion), `shift-markers` scans `buffer-points` — all points in the buffer. The filter is:

```lisp
(when (or (< linum (point-linum p))
          (and (= linum (point-linum p))
               (<= charpos (point-charpos p))))
  ...)
```

Points below the edit line, or at the edit line with charpos ≥ edit charpos, need adjustment. All others are skipped — but still examined.

**Optimization: Group buffer-points by line number.** Instead of a flat list, maintain a per-line structure (already exists as `line-points`). For multi-line edits, we only need to check points on lines ≥ the edit line. Walk the line list forward from the edit line, adjusting points on each.

Wait — the current multi-line code already does something like this indirectly. It scans `buffer-points` (flat list) and filters by `point-linum`. The inefficiency is scanning points on lines below the edit.

**The fix: walk lines, not points.** For multi-line edits:
1. Points on the edit line (at or after charpos): adjust charpos.
2. Points on lines after the edit line: adjust linum (and possibly charpos for the first affected line).
3. Points on lines before the edit line: do nothing.

Instead of scanning `buffer-points` and filtering, walk the line list from the edit line forward, adjusting points on each line. This is O(affected-lines × markers-per-line) instead of O(total-markers).

But this changes the iteration from "all points" to "all lines from edit to end". If the edit is at line 1 of a 10,000-line buffer, we still walk 10,000 lines (even if most have 0 markers). The current approach walks `buffer-points` which might be 5-10 points total.

**The current approach is actually better when total-markers << total-lines.** For typical editing (few markers, many lines), scanning the flat `buffer-points` list is faster than walking all lines.

### D4: The real optimization — skip ineligible markers early

The current code checks every point against the position predicate. The predicate involves `point-linum` and `point-charpos` comparisons. These are already O(1) per point.

The actual overhead is:
1. List traversal (cons cell dereferencing)
2. Predicate evaluation per point
3. `point-change-line` call (which itself does a list scan to remove and re-add the point to the old/new line's point list)

**`point-change-line` is the hidden cost.** Look at `point.lisp:128-143`:

```lisp
(defun point-change-line (point new-linum new-line)
  (unless (point-temporary-p point)
    (let ((old-line (point-line point)))
      (if (line:line-alive-p old-line)
          (do ((scan (line:line-points old-line) (cdr scan))
               (prev nil scan))
              ((eq (car scan) point)
               ...)
            (assert (not (null scan))))
          (push point (line:line-points new-line)))))
  ...)
```

This scans `line-points` to find and remove the point from the old line, then pushes it onto the new line. If a multi-line edit moves many points to new lines, each move is O(markers-on-old-line).

### D5: Practical optimization — fast path for common cases

Most edits are single-character inserts at the cursor position. The typical scenario:
- 1-3 markers on the current line (point, maybe mark, maybe an overlay)
- The edit is at the cursor position
- Most markers are at or before the cursor

For this case, the current code scans 1-3 markers. The overhead is negligible.

**The case that matters: buffers with many overlays.** Syntax highlighting, lint annotations, and LSP overlays can create dozens of markers. Multi-cursor editing creates N cursors, each a point. For these cases, the O(total-markers) multi-line scan is the bottleneck.

### D6: Chosen approach — two-tier optimization

1. **Single-line edits**: No change needed. The per-line scan is already O(markers-on-line), which is typically small. If profiling shows a hot line, the lazy-sort approach (Option B above) can be added later.

2. **Multi-line edits**: Add a fast path that checks if `buffer-points` is small (≤ 32 entries). If so, use the current flat-scan approach (it's faster for small lists). If large, walk the line list from the edit line forward, checking each line's points. This avoids scanning markers on lines below the edit.

3. **`point-change-line`**: Use a doubly-linked list for `line-points` so removal is O(1) instead of O(n). This requires changing `line-points` from a plain list to a structure with prev/next pointers per point.

Actually, `point-change-line` already has a fast path — it scans `line-points` only for non-temporary points, and the scan stops when the point is found. For lines with few markers, this is fine. The O(n) scan only matters when a line has many markers and many points are being moved off it simultaneously (e.g., deleting a large region that spans many marker-bearing lines).

### D7: Final scope — bounded and pragmatic

Given the analysis, the highest-impact, lowest-risk change is:

1. **Fast path for multi-line edits when buffer-points is large**: walk lines instead of scanning the flat point list.
2. **Doubly-linked `line-points` for O(1) removal**: change point's line-membership from a singly-linked list to a doubly-linked list, eliminating the O(n) scan in `point-change-line`.

Changes 1 and 2 are independent. Change 2 is more invasive (touches point creation, deletion, and line-change) but has the broadest benefit.

---

## Tasks

### Task 1: Write benchmark test for shift-markers with many markers

**Objective:** Establish a measurable baseline.

**Files:**
- Create: `tests/buffer/perf-marker-shift.lisp`

**Step 1: Write the benchmark**

```lisp
(deftest shift-markers-many-markers-perf
  ;; Create a buffer with 1000 lines and 100 overlay markers
  (let* ((buffer (lem:make-buffer "perf-test" :temporary t))
         (point (lem:buffer-point buffer)))
    (dotimes (i 1000)
      (lem:insert-string point (format nil "line ~D~%" i)))
    ;; Create 100 markers spread across the buffer
    (lem:buffer-start point)
    (let ((markers '()))
      (dotimes (i 100)
        (lem:move-to-line point (* 10 (1+ i)))
        (push (lem:copy-point point :left-inserting) markers))
      ;; Now insert a newline at line 1 — shifts all 100 markers
      (lem:buffer-start point)
      (let ((start-time (get-internal-real-time)))
        (lem:insert-character point #\newline)
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
          ;; Should be < 0.01s with optimization
          (ok (< elapsed 0.05)
              (format nil "shift 100 markers on newline insert: ~Fs" elapsed))))
      ;; Cleanup
      (mapc #'lem:delete-point markers)
      (check-corruption buffer))))
```

**Step 2: Run benchmark**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: Records baseline time (may pass or fail the threshold).

**Step 3: Commit**

```bash
jj new -m "test(buffer): add shift-markers performance benchmark"
jj squash
```

---

### Task 2: Add prev/next pointers to point for doubly-linked line-points

**Objective:** Enable O(1) removal from line-points.

**Files:**
- Modify: `src/buffer/internal/point.lisp:6-26` (add slots)
- Modify: `src/buffer/line.lisp:47-79` (change line-points to use head pointer)

**Step 1: Add doubly-linked list pointers to point**

Add to the `point` class:
```lisp
(line-prev
 :initform nil
 :accessor point-line-prev)
(line-next
 :initform nil
 :accessor point-line-next)
```

**Step 2: Change line-points to a head pointer**

The `line` class's `points` slot currently holds a list. Change it to hold just the head of a doubly-linked list. The `line-points` accessor returns the head; iteration walks `point-line-next`.

Actually, to minimize API breakage, keep `line-points` returning a list-like structure. The simplest change: keep `line-points` as a list, but also maintain `point-line-prev` / `point-line-next` for O(1) removal. The list is used for iteration; the pointers are used for removal.

This is redundant — maintaining both a list and a DLL is error-prone. **Better: replace the list entirely with a DLL.**

Change `line-points` to return the head of the DLL. Update all iteration sites from `(dolist (p (line:line-points line)))` to a DLL walk:

```lisp
(do ((p (line:line-points line) (point-line-next p)))
    ((null p))
  ...)
```

**Step 3: Update point creation**

In `initialize-point` (`point.lisp:77-80`), push the point onto the line's DLL:

```lisp
(defun initialize-point (point kind)
  (unless (eq :temporary kind)
    (let ((line (point-line point)))
      (setf (point-line-next point) (line:line-points line)
            (point-line-prev point) nil)
      (when (line:line-points line)
        (setf (point-line-prev (line:line-points line)) point))
      (setf (line:line-points line) point))
    (push point (buffer-points (point-buffer point)))))
```

Note: `buffer-points` can remain a plain list — it's only used for iteration and is not performance-critical for removal (points are removed from it via `delete` which is O(n), but this happens only on explicit `delete-point`, not on every line change).

**Step 4: Update point deletion**

In `delete-point` (`point.lisp:113-122`), remove from the DLL:

```lisp
(defun delete-point (point)
  (unless (point-temporary-p point)
    (let ((line (point-line point)))
      (when line
        (let ((prev (point-line-prev point))
              (next (point-line-next point)))
          (if prev
              (setf (point-line-next prev) next)
              (setf (line:line-points line) next))
          (when next
            (setf (point-line-prev next) prev)))))
    (let ((buffer (point-buffer point)))
      (setf (buffer-points buffer)
            (delete point (buffer-points buffer))))
    (values)))
```

**Step 5: Update point-change-line**

In `point-change-line` (`point.lisp:128-143`), remove from old line DLL (O(1)) and add to new line DLL (O(1)):

```lisp
(defun point-change-line (point new-linum new-line)
  (unless (point-temporary-p point)
    (let ((old-line (point-line point)))
      ;; O(1) removal from old line
      (when (and old-line (line:line-alive-p old-line))
        (let ((prev (point-line-prev point))
              (next (point-line-next point)))
          (if prev
              (setf (point-line-next prev) next)
              (setf (line:line-points old-line) next))
          (when next
            (setf (point-line-prev next) prev)))))
    ;; O(1) insertion to new line
    (setf (point-line-next point) (line:line-points new-line)
          (point-line-prev point) nil)
    (when (line:line-points new-line)
      (setf (point-line-prev (line:line-points new-line)) point))
    (setf (line:line-points new-line) point))
  (setf (point-linum point) new-linum)
  (setf (point-line point) new-line))
```

**Step 6: Update all iteration sites**

Find all places that iterate `line-points` as a list and update to DLL walk. Search for `(line:line-points` in the codebase.

Key sites:
- `shift-markers` (`buffer-insert.lisp:39-97`) — multiple `dolist` over `line-points`
- `check-line-corruption` (`check-corruption.lisp:21-32`) — `dolist` over `line-points`
- `check-buffer-points-corruption` (`check-corruption.lisp:42-51`) — `append` of `line-points` across lines (this needs to collect into a list)
- `line-free` (`line.lisp:101-111`) — sets `line-points` to nil

**Step 7: Update line-free**

```lisp
(defun line-free (line)
  ...
  ;; Clear all points' line references
  (do ((p (line-points line) (point-line-next p)))
      ((null p))
    (setf (point-line-prev p) nil
          (point-line-next p) nil))
  (setf (line-points line) nil)
  ...)
```

**Step 8: Verify compile + tests**

Run: `qlot exec ros run --eval '(asdf:load-system "lem/core")'`
Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: All tests pass. The corruption checker is the best validator.

**Step 9: Commit**

```bash
jj new -m "perf(buffer): doubly-linked line-points for O(1) point removal"
jj squash
```

---

### Task 3: Update shift-markers to use DLL iteration

**Objective:** Update the shift-markers function to walk the DLL instead of dolist.

**Files:**
- Modify: `src/buffer/internal/buffer-insert.lisp:39-97`

**Step 1: Update all dolist over line-points**

Replace:
```lisp
(dolist (p (line:line-points (point-line point))) ...)
```
With:
```lisp
(do ((p (line:line-points (point-line point)) (point-line-next p)))
    ((null p))
  ...)
```

There are 4 such loops in `shift-markers` (one per branch: single-line insert, multi-line insert, single-line delete, multi-line delete).

**Step 2: Verify tests**

Run: `qlot exec ros run --eval '(asdf:test-system "lem-tests")'`
Expected: All pass, including corruption checks.

**Step 3: Commit**

```bash
jj new -m "refactor(buffer): use DLL iteration in shift-markers"
jj squash
```

---

### Task 4: Add fast path for multi-line edits with many markers

**Objective:** When buffer-points is large, walk lines instead of scanning the flat list.

**Files:**
- Modify: `src/buffer/internal/buffer-insert.lisp:50-66` (the `< 0 offset-line` branch — multi-line insert)

**Step 1: Add the fast path**

In the multi-line insert branch, check if the number of buffer-points exceeds a threshold. If so, walk the line list from the edit line forward instead of scanning the flat buffer-points list:

```lisp
;; Threshold: if buffer has many points, walk lines instead of scanning flat list
(if (< (length (buffer-points (point-buffer point))) 64)
    ;; Current approach: scan flat buffer-points list
    (dolist (p (buffer-points (point-buffer point))) ...)
    ;; Optimized: walk lines from edit line forward
    (do ((line (line-next-n (point-line point) offset-line) (line:line-next line))
         (linum (+ (point-linum point) offset-line) (1+ linum)))
        ((null line))
      (do ((p (line:line-points line) (point-line-next p)))
          ((null p))
        ;; Adjust points on this line
        ...)))
```

Note: this only helps if the edit is near the end of the buffer (few lines to walk). If the edit is at line 1, walking all lines is worse than scanning a small flat list. The threshold check (`< 64`) ensures we only use the line-walk approach when it's beneficial.

Actually, this optimization is marginal and adds complexity. **Defer it unless the benchmark from Task 1 shows a real problem after the DLL change.** The DLL change (Task 2-3) already eliminates the O(n) removal cost in `point-change-line`, which is the hidden multiplier in the multi-line case.

**Step 2: Re-evaluate after DLL change**

Run the benchmark from Task 1 after Tasks 2-3. If the time is already under threshold, skip this task.

**Step 3: Commit (if implemented)**

```bash
jj new -m "perf(buffer): fast path for multi-line marker shift with many markers"
jj squash
```

---

## Verification

After all tasks:
1. `make test` — all tests pass, especially corruption checks
2. `make lint` — no violations
3. Run the benchmark test — verify time is under threshold
4. Manual test: create many overlays (e.g., open a file with LSP diagnostics), edit near the top of the buffer, verify no lag
