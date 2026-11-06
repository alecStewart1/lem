# SDL2 → SDL3 Frontend Migration Reference

Port status first, then the API mapping tables, then the design notes. Mapping
covers every SDL/SDL2-binding call used in `frontends/sdl2/` and its `cl-sdl3`
counterpart (local checkout: `~/common-lisp/cl-sdl3`). cl-sdl3 exposes C
functions in kebab-case (e.g. `SDL_RenderFillRect` → `sdl3:render-fill-rect`);
`defwrap-fun` variants return multiple values instead of using output pointers.

Legend: ✅ verified against cl-sdl3 · ⚠️ unverified — confirm before use.

Note: SDL2's `in-main-thread` utility has no direct cl-sdl3 equivalent —
SDL3 is main-thread-safe for most calls and offers `sdl3:run-on-main-thread`
and `sdl3:run-app` where needed.

## Port status

| File (sdl2/) | Ported to sdl3? | Notes |
|---|---|---|
| log.lisp | ✅ | |
| platform.lisp | ✅ | BSD platform strings handled by linux fallback — see §1 note |
| resource.lisp | ✅ | |
| utils.lisp | ✅ | |
| sdl3.lisp | ✅ | shared helpers; check before adding more so you don't duplicate |
| main.lisp | ❌ | event loop, init — see §1, §5 |
| display.lisp | ❌ | see §1, §2 |
| drawing.lisp | ❌ | see §2, §4 + FFI-reduction notes (§8) |
| view.lisp | ❌ | see §2 |
| tree.lisp | ❌ | see §2, §4 |
| color-picker.lisp | ❌ | see §1, §2 |
| font.lisp | ❌ | see §4 gotchas |
| text-surface-cache.lisp | ❌ | may shrink or vanish under the atlas/text-engine plan — see §8 |
| icon-font.lisp | ❌ | see §4 |
| keyboard.lisp | ❌ | see §5 |
| mouse.lisp | ❌ | see §6 |
| image-buffer.lisp | ❌ | see §2, §4 |
| graphics.lisp | ❌ | see §2 |
| wm.lisp | ❌ | |
| scratch-frame.lisp | ✅ (new) | REPL frame bench, not a port of sdl2 code — see §9 |

## 1. Core / init / main loop (main.lisp)

| SDL2 | Defined in | cl-sdl3 |
|---|---|---|
| `sdl2:init` / `with-init` | main.lisp | `sdl3:init` ✅ (subsystem flags changed) |
| `sdl2:with-window` | main.lisp | `sdl3:create-window` + `destroy-window`; or `create-window-and-renderer` together ✅ |
| `sdl2:with-renderer` | main.lisp | `sdl3:create-renderer` / `destroy-renderer` ✅ |
| `sdl2:with-event-loop` | main.lisp | **No equivalent.** Hand-roll with `sdl3:poll-event` / `wait-event` / `wait-event-timeout`; or `sdl3:run-app` ⚠️ |
| `sdl2:make-this-thread-main` | main.lisp | `sdl3:enter-app-main-callbacks` or `sdl3:run-app` (`sdl3/main`) ⚠️ |
| `sdl2:in-main-thread` | main, color-picker, display, drawing, graphics | Usually drop; else `sdl3:run-on-main-thread` ✅ |
| `sdl2:push-quit-event` | main.lisp | `sdl3:push-event` with `event.type = :quit` (`SDL_PushQuitEvent` removed) ⚠️ |
| `sdl2:quit` | main.lisp | `sdl3:quit` / `quit-sub-system` ✅ |
| `sdl2:set-hint` | main.lisp | `sdl3:set-hint` ✅ |
| `sdl2:get-window-flags` | main.lisp | `sdl3:get-window-flags` (flags renamed — `WINDOW_HIGH_PIXEL_DENSITY` etc.; `SDL_WINDOW_SHOWN` removed) ⚠️ |
| `sdl2:get-window-size` | display.lisp | `sdl3:get-window-size` ✅ |
| `sdl2:get-window-title` | main.lisp | `sdl3:get-window-title` ✅ |
| `sdl2:set-window-title` | main.lisp | `sdl3:set-window-title` ✅ |
| `sdl2:set-window-fullscreen` | main.lisp | `sdl3:set-window-fullscreen` (bool, not flags) ✅ |
| `sdl2:maximize-window` / `minimize-window` | main.lisp | `sdl3:maximize-window` / `minimize-window` ✅ |
| `sdl2:get-renderer-output-size` | main, display | `sdl3:get-render-output-size` (multi-value) or `get-current-render-output-size` ⚠️ |
| `sdl2:platform` | platform.lisp | `sdl3:platform` ✅ — see BSD note below |

**BSD platforms.** SDL3's `SDL_GetPlatform()` returns `"FreeBSD"`, `"NetBSD"`,
`"OpenBSD"` as distinct strings. `platform.lisp`'s `switch` falls through to the
`linux` class, which is correct for `get-font-list` (fontconfig's `fc-list`
exists on BSDs when fontconfig is installed — not guaranteed on minimal
NetBSD/OpenBSD installs). If a dedicated BSD class is ever added, fall back to
scanning `/usr/local/share/fonts` (FreeBSD), `/usr/X11R7/share/fonts` (NetBSD),
`/usr/X11R6/share/fonts` (OpenBSD) when `fc-list` is missing.

## 2. Rendering (drawing, graphics, display, view, color-picker, tree, image-buffer, utils)

| SDL2 | Defined in | cl-sdl3 |
|---|---|---|
| `sdl2:render-clear` | main, display, view | `sdl3:render-clear` ✅ |
| `sdl2:render-present` | display.lisp | `sdl3:render-present` ✅ |
| `sdl2:render-copy` | main, graphics, view, tree, color-picker | `sdl3:render-texture` (dst rect only; NULL srcrect = whole texture) ✅ |
| `sdl2:render-copy-ex` | drawing, utils | `sdl3:render-texture-rotated` (angle; center+flip args) or `render-texture-affine` ⚠️ |
| `sdl2:render-fill-rect` | most files | `sdl3:render-fill-rect` ✅ |
| `sdl2:render-draw-rect` | graphics, tree | `sdl3:render-rect` ⚠️ |
| `sdl2:render-draw-line` | display, graphics, tree | `sdl3:render-line` ⚠️ |
| `sdl2:render-draw-lines` | display.lisp | `sdl3:render-lines` ⚠️ |
| `sdl2:render-draw-point` | display, graphics, color-picker | `sdl3:render-point` ⚠️ |
| `sdl2:render-draw-points` | graphics.lisp | `sdl3:render-points` ⚠️ |
| `sdl2:set-render-draw-color` | display, tree | `sdl3:set-render-draw-color` ✅ |
| `sdl2:set-render-target` | many files | `sdl3:set-render-target` ⚠️ |
| `sdl2:render-set-viewport` | main.lisp | `sdl3:set-render-viewport` ⚠️ |
| `sdl2:create-texture` | utils, color-picker | `sdl3:create-texture` (wrapped struct object, not raw pointer) ⚠️ |
| `sdl2:create-texture-from-surface` | many files | `sdl3:create-texture-from-surface` ⚠️ |
| `sdl2:destroy-texture` | many files | `sdl3:destroy-texture` ⚠️ |
| `sdl2:current-renderer` | graphics, color-picker, tree | **No equivalent** — thread the renderer through explicitly ✅ |
| `sdl2:+sdl-textureaccess-target+` | utils.lisp | `:target` (`texture-access` type value) ⚠️ |
| `sdl2:+pixelformat-rgba8888+` | utils.lisp | `sdl3:+pixel-format-rgba8888+` (`sdl3/pixels`) ⚠️ |

## 3. Rect / point helpers (sdl2 binding utilities)

| SDL2 | Defined in | cl-sdl3 |
|---|---|---|
| `sdl2:make-rect` | main, display, graphics | `sdl3:make-rect` / foreign alloc via `sdl3/rect` types ⚠️ |
| `sdl2:with-rects` | many files | **No macro** — `cffi:with-foreign-object` + `cffi:mem-struct`, or hand-roll a `with-rects` in sdl3 ⚠️ |
| `sdl2:rect-x/y/w/h` | display.lisp | **No accessors** — `rect` is a `deflsp-type` struct (`sdl3/rect/type.lisp`); use `cffi:foreign-slot-value` ✅ |
| `sdl2:free-rect` | graphics.lisp | Not needed with stack allocation ✅ |
| `sdl2:make-points` / `with-points` / `sdl-point` / `points` | display.lisp | `point` struct type (`sdl3/rect/type.lisp`); hand-roll a `with-points` once in `utils.lisp` ⚠️ |
| `sdl2:sdl-get-render-target` (lem-local) | lem's own code | Port the helper; use `sdl3:get-render-target` ⚠️ |

## 4. Surfaces & fonts (drawing, font, text-surface-cache, main, graphics)

| SDL2 | Defined in | cl-sdl3 |
|---|---|---|
| `sdl2:free-surface` | many files | `sdl3:destroy-surface` ✅ (renamed) |
| `sdl2:surface-width` / `surface-height` | many files | **No wrapper** — plain struct (`sdl3/surface/type.lisp`, slots `w`/`h`); `cffi:foreign-slot-value` ✅ |
| `sdl2-ttf:render-utf8-blended` | drawing, main, tree | `sdl3-ttf:render-text-blended` ✅ (UTF-8 is the only encoding now) |
| `sdl2-ttf:render-text-solid` | font.lisp | `sdl3-ttf:render-text-solid` ✅ |
| `sdl2-ttf:open-font` / `close-font` | font, icon-font, tree | `sdl3-ttf:open-font` / `close-font` ✅ (ptsize is **float**) |
| `sdl2-ttf:init` / `quit` | main.lisp | `sdl3-ttf:init` / `quit` ✅ (idempotent/refcounted; returns bool — check it) |
| `sdl2-image:load-image` | main, drawing, graphics | `sdl3-image:img-load` (or `load-texture` to skip the surface step) ⚠️ |
| `sdl2-image:init` / `quit` | main.lisp | **Removed in SDL3_image** — `IMG_Load` auto-inits; delete these calls ✅ |

**SDL3_ttf gotchas** (verified against `cl-sdl3/src/extr/sdl-ttf/func.lisp` and
the SDL3 migration doc):

- **`length` parameter everywhere.** Every `sdl3-ttf:render-text-*`,
  `get-string-size`, `measure-string` takes a `size_t length` in UTF-8 **bytes**
  (0 = null-terminated). CFFI's `:string` binding can't supply it — wrap these
  so callers never pass raw string + length; compute with
  `babel:string-size-in-octets` or pass 0. Wrong length truncates/misrenders.
- **Bool returns.** Nearly all SDL3_ttf calls return `:bool`; failures are
  silent unless you check and call `sdl3:get-error`.
- **Surface ownership.** Render functions return surfaces you own — free with
  `sdl3:destroy-surface` inside `unwind-protect`. Do **not** port the sdl2
  `get-character-size` trivial-garbage finalizer pattern (sdl2/font.lisp:117).
- **Fallback fonts are not copied** (`TTF_AddFallbackFont` stores the pointer):
  a fallback font must outlive its parent, and `close-font` ordering becomes
  correctness-critical.
- **UTF-8 only.** Ensure `cffi:*default-foreign-encoding*` is `:utf-8` or
  non-Latin-1 strings get mangled at the FFI boundary.
- **ptsize is `:float`** on `open-font` / `set-font-size`; size is in points
  with DPI semantics (`set-font-size-dpi`) — keep points vs pixels straight
  for HiDPI.
- **cl-sdl3 bugs to fix** (in `func.lisp`, not lem): line ~498
  `cffi:defcfun ("(:struct text)WrapWhitespaceVisible" ...)` — bogus C symbol,
  should be `TTF_GetTextWrapWhitespaceVisible`; line ~271
  `(export 'render-text_shaded)` doesn't match the defcfun name
  `render-text-shaded`.

## 5. Events / keyboard (keyboard.lisp, main.lisp)

| SDL2 | Defined in | cl-sdl3 |
|---|---|---|
| `sdl2:sym-value` (keysym) | keyboard.lisp | `SDL_KeyboardEvent` struct fields `scancode`/`key`; `sdl3:get-scancode-from-key` / `get-key-from-scancode` for translation ⚠️ |
| `sdl2:mod-value` | keyboard.lisp | `sdl3:get-mod-state` or the event's `mod` field; keymod constants renamed (`KMOD_LSHIFT` → `+lshift+`-style) ⚠️ |
| `+sdlk-...+` constants | keyboard.lisp | Renamed keycodes in `sdl3/keyboard` (`:keycode` enum) — verify each ⚠️ |
| `sdl2:start-text-input` | main.lisp | `sdl3:start-text-input` ⚠️ |
| window event constants `+sdl-windowevent-exposed+` etc. | main.lisp | `sdl3/events` enums (`:window-event-exposed` style — events split into `SDL_EVENT_*`) ⚠️ |
| `sdl2:with-event-loop` | main.lisp | Loop over `sdl3:poll-event`; event structs polled by value (see §1) ⚠️ |

## 6. Mouse / cursor (mouse.lisp, main.lisp)

| SDL2 | Defined in | cl-sdl3 |
|---|---|---|
| `sdl2:hide-cursor` / `show-cursor` | mouse.lisp | `sdl3:hide-cursor` / `show-cursor` ⚠️ |
| `sdl2:mouse-state` | main.lisp | `sdl3:get-mouse-state` (button bitmasks → `+button-lmask+`-style in `sdl3/mouse`) ⚠️ |
| `+sdl-button-left+` etc. | main.lisp | `sdl3/mouse` button constants ⚠️ |

## 7. Things with NO SDL3 counterpart — don't create equivalents

| SDL2 frontend item | Why it's not needed |
|---|---|
| `sdl2:in-main-thread` | SDL3 moved to main-callback model (`SDL_RunApp`); event/render calls are safe. Only `sdl3:run-on-main-thread` for genuinely main-only cases |
| `sdl2:current-renderer` | SDL3 has no renderer-global state; renderer is an explicit argument everywhere |
| `sdl2-image:init` / `quit` | SDL3_image initializes implicitly on first load |
| `sdl2:with-window` / `with-renderer` / `with-rects` / `with-points` | Binding-library conveniences, not SDL APIs. Recreate small local macros in `utils.lisp` once |
| `sdl2:rect-x/y/w/h`, `surface-width/height` | Just foreign struct slot readers in cl-sdl3; trivial local helpers at most |
| `sdl2-ttf` UTF-8 vs Latin-1 distinction | SDL3_ttf only speaks UTF-8; one function per style |
| `sdl2:push-quit-event` | Push a generic quit `SDL_Event` via `sdl3:push-event` |
| `SDL_WINDOW_SHOWN` handling | Flag removed in SDL3 — windows are shown via `sdl3:show-window` |
| `sdl2:make-this-thread-main` | `SDL_RunApp` / `enter-app-main-callbacks` replaces the main-thread dance |

## 8. FFI-call reduction plan (sdl3 port design notes)

The per-call FFI boundary cost is small (~100ns); the win is eliminating the
*work behind* the calls — rasterization, texture uploads, cache churn. Ranked
by expected payoff:

### 8.1 Glyph atlas instead of per-string surfaces (biggest win)

Current sdl2 model (drawing.lisp + text-surface-cache.lisp): per unique
`(string, attribute)` pay `render-utf8-blended` (rasterize whole string) +
`create-texture-from-surface` (upload) on cache miss, then one `render-copy`
per draw per frame. A per-glyph atlas rasterizes each *codepoint* once into one
shared texture; drawing a string becomes N `render-copy` calls from the same
texture — which SDL3's renderer batches nearly free. Cache pressure drops from
per-string to per-glyph, and most of the two-tier sweep machinery
(`purge-stale-textures`, `sweep-if-oversize`) becomes unnecessary.

### 8.2 Or go further: the SDL3_ttf renderer text engine

`TTF_CreateRendererTextEngine` + `TTF_DrawRendererText` draws text directly
through the renderer with no surface or texture at all — one FFI call per text
run, HarfBuzz shaping included. This deletes the entire surface/texture cache
path. Trade-off: per-span color goes through text properties, so
mixed-attribute lines need multiple `TTF_Text` objects — measure attribute
density before committing. The atlas (8.1) is the incremental option; this is
the structural one.

### 8.3 Kill the `get-character-size` surface dance

sdl2 font.lisp:113-119 renders a `" "` surface just to measure a cell. SDL3_ttf
offers `TTF_GetFontHeight` and `TTF_GetGlyphMetrics` / `TTF_GetStringSize` —
zero allocations, and it removes the one place the finalizer-cancel hack is
load-bearing.

### 8.4 Batch flat-color primitives

Per-frame there are many `render-fill-rect` / `render-line` calls (modeline,
region highlight, cursor), each an FFI round-trip. Building one vertex array
and issuing a single `sdl3:render-geometry` call per color group collapses
dozens of calls into one. Do this only if profiling shows the loop matters —
text dwarfs rects.

### 8.5 Merge the six opened fonts into a fallback chain

`TTF_AddFallbackFont` lets one font serve latin + CJK + emoji + braille.
Fewer `open-font` calls at startup (minor), but more importantly it unlocks
8.1 and 8.2 — a single atlas or text-engine font covers every script, instead
of font-switching per string. Lifetime caveat from §4 applies: fallback fonts
are not copied and must outlive the parent font.

### What's not worth it

- `declaim`-ing the font-management code (`open-font`, `make-font-config`,
  `merge-font-config`) — one-time cost, no per-frame effect. Type declarations
  only pay off in per-glyph/per-frame paths (draw loops, coordinate math), and
  only when the declaration is compiled before and visible to callers.
- Micro-optimizing the FFI call boundary itself.

## 9. REPL-driven exploration workflow (how to find out what a function *does*)

**The harness exists: `frontends/sdl3/scratch-frame.lisp`.** It implements this
workflow end-to-end and was verified working headless (SDL_VIDEODRIVER=dummy,
software renderer): `open-scratch-frame :headless t` → `draw-demo-frame` →
`capture-frame "/tmp/frame.bmp"` (BMP on disk via `SDL_RenderReadPixels` +
`SDL_SaveBMP`) → keep drawing → capture again. API: `clear-frame`, `fill-rect`,
`draw-text`, `present`, `close-scratch-frame`, `with-scratch-frame`; the
defstruct accessors (`scratch-window` etc.) expose the live objects to the
REPL. Run it from the REPL with Sly connected and the same calls redraw the
live window instead of writing a file.

"Does something" isn't clear until a frame is actually on your screen. The REPL
makes the running frontend an experiment bench instead of a black box:

1. **Run the frontend from the REPL, not a dumped binary.** Start `ros run` (or
   `sbcl`) with Sly/SLIME connected, `ql:quickload :lem-sdl3`, then call its
   entry point as a separate thread. Now the live editor window and the REPL
   share one image: every `C-M-x` in Emacs takes effect in the running GUI, and
   the running GUI's state is queryable from the REPL.

2. **Probe live state while the window is open.** From the REPL, evaluate
   against the actual objects the frame loop is using: `sdl3:get-window-size`,
   renderer info, the display's font metrics, `hash-table-count` on the texture
   cache, `sdl3:get-error` after a suspected failure. Reading the FFI wrapper's
   return values here replaces guessing about what a function "does".

3. **Force a redraw from the REPL.** Call the display/view redraw entry
   directly after any redefinition and watch the window change immediately —
   no restart between experiments. That tightens the loop to seconds.

4. **Capture the frame.** `sdl3:render-read-pixels` → `sdl3:surface` → save PNG
   via sdl3-image, then look at the file. This verifies what was *actually*
   rendered, which is how you catch silent bool-return failures (§4) and
   off-by-one baseline anchoring without eyeballing the live window.

5. **Count and time FFI calls before optimizing (§8 targets).**
   - Per-call counters: define thin wrappers in `lem-sdl3/utils.lisp` around the
     hot sdl3-ttf/render entry points that bump a hash-table counter, or use
     `(defmethod ... :around)`/advice where generic functions exist. Frame N's
     counts tell you whether the atlas plan (§8.1) is actually needed.
   - Sampling: `(sb-sprof:with-profiling (:max-samples 1000 :report :graph))`
     around a forced redraw of a large buffer shows whether time goes to
     rasterization, texture upload, or the draw loop.
   - Coarse timing: wrap one redraw in `(get-internal-run-time)` deltas and
     diff between experiments. Real numbers, one `C-M-x` apart.

6. **Build a throwaway scratch window first.** A ~50-line `frontends/sdl3/`
   experiment file that opens a window/renderer, opens a font, and renders
   sample strings — evaluated form by form — is where you learn SDL3_ttf's
   actual behavior (length parameter semantics, baseline anchoring, fallback
   rendering) *before* entangling it with lem's view machinery. Keep it; it
   becomes the regression probe when cl-sdl3 changes.

7. **Treat every experiment as a redefinition, not a rebuild.** If a hypothesis
   fails, `defun`/`defmethod` the old body back and try the next. Nothing needs
   a restart unless CFFI type definitions change (and those restart too rarely
   to fear).

### cl-sdl3 calling conventions (learned the hard way, verified)

These cost real debugging time — don't rediscover them:

- **Struct initargs are `:%x`-style** (double percent): `deflsp-type` prefixes
  slots with `%`, and `defclass-std` keeps that in the initarg. So
  `(make-instance 'sdl3:frect :%x 1.0 :%y 2.0 :%w 3.0 :%h 4.0)`. Same for
  `sdl3:color` (`:%r :%g :%b :%a`).
- **Foreign slot names are `sdl3:%w`-style** and must be *package-qualified*:
  `(cffi:foreign-slot-value surface '(:struct sdl3:surface) 'sdl3:%w)`. An
  unqualified `%w` interns in *your* package and fails with "Undefined slot".
- **Flags are keywords**: `'(:video)` for `sdl3:init`, `'(:hidden)` for window
  flags. CFFI bitfields reject unquoted/plain symbols.
- **You can pass CLOS instances** (`sdl3:frect`, `sdl3:color`) directly where a
  C struct or struct-pointer argument is expected — the `deflsp-type` CFFI
  translation handles it. `NIL` means NULL for optional rect/pointer args.
- **`render-read-pixels` exists** in cl-sdl3 as `sdl3:render-read-pixels`
  (renderer, rect-or-nil) → surface, and `sdl3:save-bmp` writes it to disk.
- Check every bool return with `sdl3:get-error`; an empty error string with a
  NULL pointer usually means the *previous* call poisoned the error state —
  probe incrementally in the REPL (this inverted-check bug class is exactly
  what step 5 of this workflow is for).
