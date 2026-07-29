# Lem Improvement Plans

Implementation plans for three improvement areas discussed in July 2026.
All plans are based on analysis of the actual codebase at commit `492312e4`
(upstream main as of 2026-07-29).

## Branch structure (Jujutsu)

These plans are based on the `alecStewart1/improvements` bookmark, which
rebases to `main@upstream` and serves as the base for implementation branches.

Each plan should be implemented on its own bookmark, created from
`alecStewart1/improvements`:

```
jj new alecStewart1/improvements -m "start of <feature> implementation"
```

## Plans

### 1. SDL3 Frontend (`sdl3-frontend.md`)

Port the SDL2 frontend to SDL3 as a new parallel `frontends/sdl3/` system
using [cl-sdl3](https://github.com/aiffc/cl-sdl3). SDL2 stays until SDL3
is proven. 14 tasks, porting file-by-file in dependency order.

### 2. Buffer Position Cache (`buffer-position-cache.md`)

Eliminate the O(n) `position-at-point` cost that runs on every keystroke
by adding an incremental per-line character-offset cache with lazy repair.
7 tasks. Highest user-visible payoff for large-file editing.

### 3. Undo History Bounding (`undo-history-bounding.md`)

Cap undo history memory by limiting total size and count, with coalescing
of consecutive single-character inserts. 8 tasks. Prevents memory blowup
during long editing sessions.

### 4. Marker Shift Optimization (`marker-shift-optimization.md`)

Reduce the hidden O(n) cost of `point-change-line` by converting
`line-points` from a singly-linked list to a doubly-linked list, enabling
O(1) removal. 4 tasks. Benefits buffers with many overlays/markers and
improves multi-cursor editing.

## Priority

Recommended implementation order (by payoff/effort ratio):

1. **Buffer position cache** — days of work, largest user-visible win for
   large-file editing. No API changes.
2. **Undo history bounding** — straightforward, prevents memory blowup.
   No API changes.
3. **Marker shift optimization** — moderate effort (DLL conversion touches
   point internals), benefits overlay-heavy buffers. Corruption checker
   validates correctness.
4. **SDL3 frontend** — largest effort, longest timeline. Should be done
   last (or in parallel by a different person) since it doesn't depend on
   the buffer improvements.

## Workflow

All plans use the jj squash workflow:

```bash
jj new -m "type(scope): description"   # start a new change
# ... make changes ...
jj squash                               # squash into the change
```

Each task in a plan = one jj change. Commit after every task.
