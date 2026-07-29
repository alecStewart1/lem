# SDL3 Frontend Implementation Plan

> **For Hermes:** Use subagent-driven-development skill to implement this plan task-by-task.

**Goal:** Port the SDL2 frontend to SDL3 as a new parallel `frontends/sdl3/` system, keeping SDL2 alive until SDL3 is proven.

**Architecture:** A new `lem-sdl3` ASDF system implementing the existing `lem-if:*` protocol, using [cl-sdl3](https://github.com/aiffc/cl-sdl3) (which builds against SDL3, SDL3_ttf, SDL3_image, SDL3_mixer). The core editor and all extensions remain untouched — the `lem-if:*` boundary in `src/interface.lisp` is the contract.

**Tech Stack:** Common Lisp (SBCL), cl-sdl3, SDL3_ttf, SDL3_image, trivial-main-thread

---

## Background

The SDL2 frontend (`frontends/sdl2/`, ~3,600 lines of logic across 18 files + 3,259 lines of generated icon data) is functional but carries accumulated workarounds:

- **Threading**: `tmt:with-body-in-main-thread` + `sdl2:make-this-thread-main` + platform-specific `_exit` calls (`main.lisp:226-237`). SDL3 reworked platform event loops and relaxed main-thread constraints on most platforms.
- **High-DPI**: Three separate window-event constants (`+resized+`, `+size-changed+`, `+display-changed+`) each calling `handle-display-changed` (`main.lisp:91-114`). SDL3 unifies display/scale handling.
- **Renderer**: Manual texture management — `will-update-display` sets render target + clears, `update-display` copies to screen (`main.lisp:371-420`). SDL3's GPU API is a generational change.
- **Text input/IME**: Separate `on-textinput` / `on-textediting` / platform-specific `handle-text-input` branches. SDL3 merged and cleaned up text input.

## cl-sdl3 API surface

cl-sdl3 (https://github.com/aiffc/cl-sdl3) builds against SDL3, SDL3_ttf, SDL3_image, and SDL3_mixer. Linux/macOS installation requires the shared libraries in standard paths (e.g. `/usr/lib/x86_64-linux-gnu/`).

The SDL2 frontend uses this API surface (must be verified against cl-sdl3 before porting each file):

**SDL2 core** (`sdl2:` package): `with-init`, `with-window`, `with-renderer`, `with-event-loop`, `make-rect`, `with-rects`, `with-points`, `render-clear`, `render-present`, `render-copy`, `render-copy-ex`, `render-fill-rect`, `render-draw-line`, `render-draw-lines`, `render-draw-point`, `render-draw-points`, `render-draw-rect`, `render-set-viewport`, `set-render-draw-color`, `set-render-target`, `get-renderer-output-size`, `create-texture`, `create-texture-from-surface`, `destroy-texture`, `make-this-thread-main`, `in-main-thread`, `set-hint`, `start-text-input`, `push-quit-event`, `mouse-state`, `surface-width`, `surface-height`, `free-surface`, `free-rect`, `get-window-flags`, `get-window-size`, `get-window-title`, `set-window-title`, `set-window-fullscreen`, `maximize-window`, `minimize-window`, `show-cursor`, `hide-cursor`, `platform`, `mod-value`, `sym-value`, `points*`

**SDL2 FFI** (`sdl2-ffi:`): 39 `+sdlk-*+` key constants, 8 `+kmod-*+` modifier constants, 6 `+sdl-button-*+` / `+sdl-button-*mask+` constants, 7 `+sdl-windowevent-*+` constants, `+sdl-pixelformat-rgba8888+`, `+sdl-textureaccess-target+`, and 9 FFI functions (`sdl-get-clipboard-text`, `sdl-get-render-target`, `sdl-set-clipboard-text`, `sdl-set-text-input-rect`, `sdl-set-window-bordered`, `sdl-set-window-icon`, `ttf-font-ascent`, `ttf-font-descent`, `ttf-font-height`)

**SDL2_ttf** (`sdl2-ttf:`): `init`, `quit`, `open-font`, `close-font`, `render-text-solid`, `render-utf8-blended`

**SDL2_image** (`sdl2-image:`): `init`, `quit`, `load-image`

## File inventory (SDL2 frontend)

| File | Lines | Responsibility | Port difficulty |
|------|-------|----------------|-----------------|
| `sdl2.lisp` | 13 | Implementation class def | Trivial |
| `wm.lisp` | 19 | X11 WM class | Trivial |
| `resource.lisp` | 12 | Resource path lookup | Trivial |
| `mouse.lisp` | 19 | Mouse event struct | Trivial |
| `utils.lisp` | 20 | Texture creation helper | Trivial |
| `icon-font.lisp` | 24 | Icon font registry | Trivial |
| `log.lisp` | 27 | Debug logging macro | Trivial |
| `platform.lisp` | 27 | Platform detection (linux/mac/windows) | Trivial |
| `text-surface-cache.lisp` | 141 | Surface/texture LRU cache | Easy — SDL3 texture API swap |
| `view.lisp` | 158 | Per-window view + texture | Easy |
| `font.lisp` | 176 | Font config + TTF loading | Easy — SDL3_ttf API |
| `image-buffer.lisp` | 187 | Image buffer display | Easy |
| `graphics.lisp` | 194 | Drawable API (lines/rects/points/images) | Easy |
| `keyboard.lisp` | 328 | Key mapping + IME handling | Medium — key constants + event model |
| `color-picker.lisp` | 389 | Color picker UI | Medium |
| `tree.lisp` | 392 | File tree view | Medium |
| `display.lisp` | 405 | Display object, renderer, DPI, font mgmt | Hard — core rendering |
| `drawing.lisp` | 509 | Per-glyph two-pass rendering | Hard — core rendering |
| `main.lisp` | 564 | Event loop, invoke, lem-if methods | Hard — event loop + threading |
| `icon.lisp` | 3259 | Generated icon glyph data | Copy as-is (data, not logic) |

## Design decisions

### D1: Parallel system, not replacement

SDL3 lives in `frontends/sdl3/` as `lem-sdl3`. SDL2 stays in `frontends/sdl2/`. The `get-default-implementation` function (`src/interface.lisp:49-75`) already has a fallback chain — add `:sdl3` to it:

```lisp
(list implementation :webview :ncurses :sdl2 :sdl3)
```

Users select via `--implementation sdl3` or `(setf lem:*default-implementation* :sdl3)`.

### D2: Port order = dependency order

Port bottom-up: trivial files first (resource, platform, utils), then the rendering core (display, font, drawing, view), then event loop + keyboard (main), then UI features (graphics, image-buffer, color-picker, tree). The text-surface-cache ports with display since they're coupled.

### D3: Verify cl-sdl3 API per file

Before porting each file, verify that cl-sdl3 provides the equivalent of every `sdl2:` / `sdl2-ffi:` / `sdl2-ttf:` / `sdl2-image:` symbol used by that file. SDL3 renamed many symbols (e.g. `SDL_CreateTexture` → `SDL_CreateTexture` still exists but the FFI binding name may differ). If cl-sdl3 lacks a binding, either (a) add it to cl-sdl3 upstream, or (b) use `cffi:foreign-funcall` directly as a stopgap. Document any stopgap.

### D4: Keep the two-pass glyph rendering

`drawing.lisp`'s per-character two-pass rendering (background pass → glyph pass) exists to preserve 1-pixel anti-aliasing overhang at attribute boundaries. This is SDL_ttf behavior, not SDL2-specific — port it as-is to SDL3_ttf.

### D5: SDL3 event loop simplification

SDL3's event loop changes:
- `SDL_Event` is now a simpler union — fewer struct offsets
- Text input: `SDL_EVENT_TEXT_INPUT` and `SDL_EVENT_TEXT_EDITING` still exist but the API is cleaner
- Window events: unified `SDL_EVENT_WINDOW_*` with a single `SDL_WindowEvent` struct
- No more `SDL_INIT_*` flags — `SDL_Init()` takes no arguments

Port `event-loop` (`main.lisp:119-144`) to cl-sdl3's equivalent. The threading model (`tmt:with-body-in-main-thread`) may be simplifiable — test whether SDL3 still requires main-thread on each platform.

### D6: SDL3 GPU API — defer

SDL3 introduces a new GPU API (`SDL_GpuStream`). Do NOT use it in the initial port. Use the classic `SDL_Renderer` API (which SDL3 retains) to keep the port 1:1 with the SDL2 frontend. GPU API adoption is a separate future effort.

---

## Tasks

### Task 1: Create lem-sdl3 ASDF system skeleton

**Objective:** Create the system definition and empty package files.

**Files:**
- Create: `frontends/sdl3/lem-sdl3.asd`
- Create: `frontends/sdl3/sdl3.lisp`

**Step 1: Create the ASD file**

Model after `frontends/sdl2/lem-sdl2.asd`. Dependencies: `cl-sdl3`, `lem/core`, `lem/extensions`, `trivial-main-thread`. Check whether cl-sdl3 provides ttf/image bindings under separate system names or unified.

```lisp
(defsystem "lem-sdl3"
  :depends-on ("cl-sdl3"
               "lem/core"
               "lem/extensions"
               "trivial-main-thread")
  :serial t
  :components ((:file "wm")
               (:file "resource")
               (:file "platform")
               (:file "keyboard")
               (:file "font")
               (:file "icon")
               (:file "text-surface-cache")
               (:file "log")
               (:file "sdl3")
               (:file "icon-font")
               (:file "mouse")
               (:file "utils")
               (:file "display")
               (:file "view")
               (:file "main")
               (:file "drawing")
               (:file "graphics")
               (:file "image-buffer")
               (:file "tree")
               (:file "color-picker"))
  :in-order-to ((test-op (test-op "lem-sdl3/tests"))))

(defsystem "lem-sdl3/tests"
  :depends-on ("lem-sdl3" "rove")
  :components ((:module "tests"
                :components ((:file "font")
                             (:file "drawing"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))

(defsystem "lem-sdl3/executable"
  :build-operation program-op
  :build-pathname "../../lem"
  :entry-point "lem:main"
  :depends-on ("lem-sdl3"))
```

**Step 2: Create the implementation class**

```lisp
(defpackage :lem-sdl3/sdl3
  (:use :cl)
  (:export :sdl3))
(in-package :lem-sdl3/sdl3)

(defclass sdl3 (lem:implementation)
  ()
  (:default-initargs
   :name :sdl3
   :redraw-after-modifying-floating-window nil
   :underline-color-support t))

(pushnew :lem-sdl3 *features*)
```

**Step 3: Verify it loads**

Run: `qlot exec ros run --eval '(asdf:load-system "lem-sdl3")'`
Expected: System loads (may fail if cl-sdl3 is not yet in qlfile — see Task 2).

**Step 4: Commit**

```bash
jj new -m "feat(sdl3): create lem-sdl3 ASDF system skeleton"
# files are created...
jj squash
```

---

### Task 2: Add cl-sdl3 to qlfile

**Objective:** Add cl-sdl3 as a qlot dependency.

**Files:**
- Modify: `qlfile`

**Step 1: Add the dependency**

Add to `qlfile`:
```
git cl-sdl3 https://github.com/aiffc/cl-sdl3.git
```

**Step 2: Install**

Run: `qlot install`
Expected: cl-sdl3 and its dependencies install.

**Step 3: Verify cl-sdl3 loads**

Run: `qlot exec ros run --eval '(ql:quickload :cl-sdl3)'`
Expected: Loads without error.

**Step 4: Commit**

```bash
jj new -m "build: add cl-sdl3 to qlfile"
jj squash
```

---

### Task 3: Port trivial files (resource, platform, log, utils, wm, mouse, icon-font)

**Objective:** Port the 7 trivial files that have little to no SDL API dependency.

**Files:**
- Create: `frontends/sdl3/resource.lisp` (from `frontends/sdl2/resource.lisp`)
- Create: `frontends/sdl3/platform.lisp` (from `frontends/sdl2/platform.lisp`)
- Create: `frontends/sdl3/log.lisp` (from `frontends/sdl2/log.lisp`)
- Create: `frontends/sdl3/utils.lisp` (from `frontends/sdl2/utils.lisp`)
- Create: `frontends/sdl3/wm.lisp` (from `frontends/sdl2/wm.lisp`)
- Create: `frontends/sdl3/mouse.lisp` (from `frontends/sdl2/mouse.lisp`)
- Create: `frontends/sdl3/icon-font.lisp` (from `frontends/sdl2/icon-font.lisp`)

**Approach:** These files are almost SDL-agnostic. Copy them, change package names from `lem-sdl2` to `lem-sdl3`, and verify any `sdl2:` references. `utils.lisp` uses `sdl2:create-texture` and `sdl2-ffi:+sdl-pixelformat-rgba8888+` / `sdl2-ffi:+sdl-textureaccess-target+` — verify cl-sdl3 equivalents.

**Step 1: Port each file**

Copy each file, rename packages, update SDL symbol references to cl-sdl3 equivalents.

**Step 2: Verify load**

Run: `qlot exec ros run --eval '(asdf:load-system "lem-sdl3")'`
Expected: All trivial files compile.

**Step 3: Commit**

```bash
jj new -m "feat(sdl3): port trivial frontend files"
jj squash
```

---

### Task 4: Port font.lisp

**Objective:** Port font configuration and TTF loading to SDL3_ttf.

**Files:**
- Create: `frontends/sdl3/font.lisp` (from `frontends/sdl2/font.lisp`)

**Key API changes to verify:**
- `sdl2-ttf:open-font` → cl-sdl3's TTF open function
- `sdl2-ttf:render-text-solid` → cl-sdl3's TTF render function
- `sdl2-ffi.functions:ttf-font-ascent` / `ttf-font-descent` / `ttf-font-height` → cl-sdl3 equivalents
- `sdl2:surface-width` / `sdl2:surface-height` → cl-sdl3 surface accessors

**Step 1: Port the file**

The `font-config` and `font` structs stay as `defstruct`. The `open-font` function calls `sdl2-ttf:open-font` 6 times (latin-normal, latin-bold, cjk-normal, cjk-bold, emoji, braille) — map each to cl-sdl3's equivalent.

**Step 2: Verify TTF loading works**

Write a test that opens the bundled NotoSansMono font and renders a space character, checking width/height are positive.

**Step 3: Commit**

```bash
jj new -m "feat(sdl3): port font loading to SDL3_ttf"
jj squash
```

---

### Task 5: Port text-surface-cache.lisp

**Objective:** Port the surface/texture LRU cache.

**Files:**
- Create: `frontends/sdl3/text-surface-cache.lisp` (from `frontends/sdl2/text-surface-cache.lisp`)

**Step 1: Port the file**

The cache logic (hash tables, `cache-entry` struct, sweep/purge strategy) is SDL-agnostic. Only the SDL calls change:
- `sdl2:destroy-texture` → cl-sdl3 equivalent
- `sdl2:create-texture-from-surface` → cl-sdl3 equivalent
- `sdl2:free-surface` → cl-sdl3 equivalent

**Step 2: Verify load**

Run: `qlot exec ros run --eval '(asdf:load-system "lem-sdl3")'`

**Step 3: Commit**

```bash
jj new -m "feat(sdl3): port text-surface-cache"
jj squash
```

---

### Task 6: Port display.lisp

**Objective:** Port the core display object, renderer wrapper, and DPI handling.

**Files:**
- Create: `frontends/sdl3/display.lisp` (from `frontends/sdl2/display.lisp`)

**Key API changes to verify:**
- `sdl2:get-renderer-output-size` → SDL3 equivalent (may be `SDL_GetCurrentRenderOutputSize`)
- `sdl2:get-window-size` → SDL3 equivalent (SDL_GetWindowSize, may use `SDL_GetWindowSizeInPixels` for DPI)
- `sdl2:set-render-draw-color` → cl-sdl3 equivalent
- `sdl2:render-fill-rect` / `render-draw-line` → cl-sdl3 equivalents
- `sdl2:render-present` → cl-sdl3 equivalent
- `sdl2:make-rect` → cl-sdl3 rect creation (SDL3 uses `SDL_FRect` for rendering — verify)
- `sdl2:in-main-thread` → may not be needed with SDL3's relaxed threading
- `sdl2::sdl-get-render-target` (internal) → cl-sdl3 equivalent or `cffi` call

**Step 1: Port the display class**

Keep the `display` class, all slots, and the `with-display` / `with-renderer` macros. Update SDL calls.

**Step 2: Port DPI handling**

`adapt-high-dpi-display-scale` and `adapt-high-dpi-font-size` use `get-renderer-output-size` vs `get-window-size` to compute scale. SDL3 has `SDL_GetWindowSizeInPixels` which may simplify this — investigate.

**Step 3: Port render helpers**

`render-fill-rect`, `render-line`, `render-border`, `render-margin-line` — all use `with-scratch-rect` + `sdl2:render-*`. Port the scratch-rect pattern (pre-allocated rect reused across calls).

**Step 4: Verify compile**

Run: `qlot exec ros run --eval '(asdf:load-system "lem-sdl3")'`

**Step 5: Commit**

```bash
jj new -m "feat(sdl3): port display object and renderer"
jj squash
```

---

### Task 7: Port view.lisp

**Objective:** Port the per-window view with texture backing.

**Files:**
- Create: `frontends/sdl3/view.lisp` (from `frontends/sdl2/view.lisp`)

**Step 1: Port the file**

The `view` class is SDL-agnostic. SDL calls: `sdl2:destroy-texture`, `sdl2:set-render-target`, `sdl2:render-clear`, `sdl2:render-copy`, `sdl2:with-rects`. Port each to cl-sdl3.

**Step 2: Register the post-display-change hook**

`refresh-all-view-textures` is registered via `display:add-post-display-change-hook` at load time — keep this pattern.

**Step 3: Commit**

```bash
jj new -m "feat(sdl3): port view with texture backing"
jj squash
```

---

### Task 8: Port drawing.lisp

**Objective:** Port the per-glyph two-pass rendering pipeline.

**Files:**
- Create: `frontends/sdl3/drawing.lisp` (from `frontends/sdl2/drawing.lisp`)

**Key concern:** The two-pass rendering (`redraw-physical-line`, `draw-text-glyph-surface`, `draw-text-object-phase`) preserves 1-pixel AA overhang. This is SDL_ttf behavior — port as-is. The `:clip` / `:phase` / `:bg` / `:glyph` mechanism stays.

**Step 1: Port the drawing methods**

All `draw-object` methods, `object-width`, `object-height`, `redraw-physical-line` — port SDL calls. The core calls: `sdl2:render-copy-ex`, `sdl2:render-fill-rect`, `sdl2:surface-width`, `sdl2:surface-height`, `sdl2:destroy-texture`.

**Step 2: Port the `lem-if:render-line` and `lem-if:render-line-on-modeline` methods**

These are the entry points from core.

**Step 3: Verify compile**

**Step 4: Commit**

```bash
jj new -m "feat(sdl3): port two-pass glyph rendering"
jj squash
```

---

### Task 9: Port keyboard.lisp

**Objective:** Port key mapping, modifier handling, and IME.

**Files:**
- Create: `frontends/sdl3/keyboard.lisp` (from `frontends/sdl2/keyboard.lisp`)

**Key API changes:**
- 39 `+sdlk-*+` constants → SDL3 scancode/keycode equivalents (SDL3 renamed some)
- 8 `+kmod-*+` modifier constants → SDL3 modifier masks (SDL3 uses `SDL_KMOD_*`)
- `sdl2:sym-value` / `sdl2:mod-value` → cl-sdl3 accessors for key event fields
- Platform-specific `handle-text-input` / `handle-key-down` / `handle-key-up` dispatch stays

**Step 1: Port the key code table**

Map each `sdl2-ffi:+sdlk-*+` to its cl-sdl3 equivalent. SDL3 may use `SDL_SCANCODE_*` vs `SDL_KEYCODE_*` — verify which cl-sdl3 exposes.

**Step 2: Port modifier handling**

`*modifier-code-table*` and `mod-p` use bitmask AND. SDL3's `SDL_Keymod` is still a bitmask — direct port.

**Step 3: Port text input handling**

The `handle-text-input-*` / `handle-key-down-*` functions are platform-dispatched. SDL3's text input event structure may differ — verify field accessors.

**Step 4: Commit**

```bash
jj new -m "feat(sdl3): port keyboard mapping and IME"
jj squash
```

---

### Task 10: Port main.lisp (event loop + invoke + lem-if methods)

**Objective:** Port the event loop, frontend invocation, and all `lem-if:*` method implementations.

**Files:**
- Create: `frontends/sdl3/main.lisp` (from `frontends/sdl2/main.lisp`)

**Key changes:**
- `sdl2:with-event-loop` → cl-sdl3 event loop (SDL3's `SDL_PollEvent` / `SDL_WaitEvent` — cl-sdl3 may have a different macro)
- `sdl2:with-init` → `SDL_Init()` (no flags in SDL3)
- `sdl2:with-window` / `sdl2:with-renderer` → cl-sdl3 equivalents
- Window event constants → SDL3's `SDL_EVENT_WINDOW_*` (renamed from `SDL_WINDOWEVENT_*`)
- Mouse button constants → SDL3 equivalents
- `sdl2:start-text-input` → SDL3 `SDL_StartTextInput`
- `sdl2:set-hint` → SDL3 `SDL_SetHint` (hint names may have changed)
- Threading: test whether `tmt:with-body-in-main-thread` + `make-this-thread-main` is still needed

**Step 1: Port `create-display`**

The display creation chain: init → TTF init → image init → font config → open font → create window → create renderer → compute scale → create texture → create display object. Port each SDL call.

**Step 2: Port the event loop**

`event-loop` dispatches on: `:quit`, `:textinput`, `:textediting`, `:keydown`, `:keyup`, `:mousebuttondown`, `:mousebuttonup`, `:mousemotion`, `:mousewheel`, `:dropfile`, `:windowevent`. Map each to cl-sdl3's event type.

**Step 3: Port `lem-if:invoke`**

The invoke method sets SDL hints, configures threading, calls `create-display`, and runs the event loop. Port the hint settings (verify SDL3 hint names).

**Step 4: Port all remaining `lem-if:*` methods**

`get-background-color`, `get-foreground-color`, `update-foreground`, `update-background`, `update-cursor-shape`, `display-width`, `display-height`, `display-title`, `set-display-title`, `display-fullscreen-p`, `set-display-fullscreen-p`, `maximize-frame`, `minimize-frame`, `make-view`, `delete-view`, `clear`, `set-view-size`, `set-view-pos`, `redraw-view-before`, `redraw-view-after`, `will-update-display`, `update-display`, `increase-font-size`, `decrease-font-size`, `set-font-size`, `resize-display-before`, `get-font-list`, `get-mouse-position`, `get-char-width`, `get-char-height`, `view-width`, `view-height`, `clipboard-paste`, `clipboard-copy`.

**Step 5: Verify the frontend starts**

Run: `qlot exec ros run --eval '(lem:lem :implementation :sdl3)'`
Expected: Window opens, displays the Lem dashboard.

**Step 6: Commit**

```bash
jj new -m "feat(sdl3): port event loop and lem-if methods"
jj squash
```

---

### Task 11: Port graphics.lisp, image-buffer.lisp, color-picker.lisp, tree.lisp

**Objective:** Port the remaining feature files.

**Files:**
- Create: `frontends/sdl3/graphics.lisp` (from `frontends/sdl2/graphics.lisp`)
- Create: `frontends/sdl3/image-buffer.lisp` (from `frontends/sdl2/image-buffer.lisp`)
- Create: `frontends/sdl3/color-picker.lisp` (from `frontends/sdl2/color-picker.lisp`)
- Create: `frontends/sdl3/tree.lisp` (from `frontends/sdl2/tree.lisp`)

**Step 1: Port graphics.lisp**

The `drawable` class, `with-drawable` macro, and drawing functions. SDL calls: `render-draw-line`, `render-fill-rect`, `render-draw-rect`, `render-draw-point`, `render-draw-points`, `create-texture-from-surface`, `render-copy`. Note: `draw-points` uses `plus-c:c-let` with `sdl2-ffi:sdl-point` — verify cl-sdl3's FFI approach.

**Step 2: Port image-buffer.lisp**

Uses `sdl2-image:load-image` and surface manipulation. Verify cl-sdl3's image loading API.

**Step 3: Port color-picker.lisp and tree.lisp**

These are UI features built on top of graphics/display. Port SDL calls.

**Step 4: Verify full system load**

Run: `qlot exec ros run --eval '(asdf:load-system "lem-sdl3")'`

**Step 5: Commit**

```bash
jj new -m "feat(sdl3): port graphics, image-buffer, color-picker, tree"
jj squash
```

---

### Task 12: Copy icon.lisp

**Objective:** Copy the generated icon glyph data (3,259 lines of data, not logic).

**Files:**
- Create: `frontends/sdl3/icon.lisp` (copy from `frontends/sdl2/icon.lisp`)

**Step 1: Copy and rename package**

Copy the file, change `lem-sdl2` package references to `lem-sdl3`.

**Step 2: Verify load**

**Step 3: Commit**

```bash
jj new -m "feat(sdl3): copy icon glyph data"
jj squash
```

---

### Task 13: Add :sdl3 to implementation fallback chain

**Objective:** Register SDL3 in the default implementation selector.

**Files:**
- Modify: `src/interface.lisp:62`

**Step 1: Update the fallback list**

```lisp
;; Before:
(list implementation :webview :ncurses :sdl2)
;; After:
(list implementation :webview :ncurses :sdl2 :sdl3)
```

**Step 2: Verify selection works**

Run: `qlot exec ros run --eval '(lem:lem :implementation :sdl3)'`

**Step 3: Commit**

```bash
jj new -m "feat: add sdl3 to implementation fallback chain"
jj squash
```

---

### Task 14: Integration testing

**Objective:** Verify the SDL3 frontend works end-to-end.

**Step 1: Manual smoke test**

- Open a file, edit text, verify cursor movement
- Test undo/redo
- Test split windows
- Test font size change (Ctrl+ / Ctrl-)
- Test fullscreen toggle
- Test clipboard copy/paste
- Test IME input (if applicable)
- Test high-DPI (move window between displays)

**Step 2: Run existing test suite**

Run: `make test`
Expected: All existing tests pass (SDL3 frontend should not affect core tests).

**Step 3: Document known issues**

Create `frontends/sdl3/KNOWN-ISSUES.md` listing any SDL3-specific bugs or missing features.

**Step 4: Commit**

```bash
jj new -m "test(sdl3): integration smoke test and known issues"
jj squash
```

---

## Risks

1. **cl-sdl3 maturity**: The library is young. If it lacks bindings for functions the frontend needs (e.g. `SDL_SetTextInputRect`, `SDL_GetRenderWindow`, `TTF_FontAscent`), you'll need to contribute upstream or use `cffi:foreign-funcall` stopgaps. Budget time for this.

2. **SDL3 rect type changes**: SDL3 rendering functions use `SDL_FRect` (float-based) instead of `SDL_Rect` (int-based) for some operations. The scratch-rect optimization in `display.lisp` may need to use `SDL_FRect`. Verify early — this affects every draw call.

3. **Threading model**: SDL3 may not need the `trivial-main-thread` wrapping. Test on Linux first (simplest), then macOS, then Windows. The `_exit` calls on macOS/Windows (`main.lisp:122-123, 237`) may no longer be needed.

4. **SDL3_ttf API changes**: SDL3_ttf may have renamed functions or changed the surface format. The `render-utf8-blended` function is critical — verify it produces compatible surfaces.

5. **Event type renaming**: SDL3 renamed many event types (`SDL_WINDOWEVENT_*` → `SDL_EVENT_WINDOW_*`, `SDL_QUIT` → `SDL_EVENT_QUIT`, etc.). The cl-sdl3 binding names may follow either convention — check before porting the event loop.

## When to revisit / abandon SDL2

- SDL3 frontend passes all integration tests on Linux + macOS
- At least one user has used SDL3 daily for 2+ weeks without regressions
- Then: mark SDL2 frontend as deprecated in README, add migration note, plan removal after one release cycle
