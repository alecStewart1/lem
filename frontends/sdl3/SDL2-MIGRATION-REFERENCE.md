# SDL2 → SDL3 Frontend Migration Reference

Mapping of every SDL/SDL2-binding call used in `frontends/sdl2/` to its
`cl-sdl3` counterpart (local checkout: `~/common-lisp/cl-sdl3`). cl-sdl3
exposes C functions in kebab-case (e.g. `SDL_RenderFillRect` →
`sdl3:render-fill-rect`); `defwrap-fun` variants return multiple values
instead of using output pointers.

Note: SDL2's `in-main-thread` utility has no direct cl-sdl3 equivalent —
SDL3 is main-thread-safe for most calls and offers `sdl3:run-on-main-thread`
and `sdl3:run-app` where needed.

## 1. Core / init / main loop (main.lisp)

| SDL2 function | Defined in | cl-sdl3 counterpart |
|---|---|---|
| `sdl2:init` `sdl2:with-init` | `main.lisp` (binding) | `sdl3:init` (init subsystem flags changed in SDL3) |
| `sdl2:with-window` | `main.lisp` | `sdl3:create-window` + `sdl3:destroy-window` (no macro; window + renderer can be made together via `sdl3:create-window-and-renderer`) |
| `sdl2:with-renderer` | `main.lisp` | `sdl3:create-renderer` `sdl3:destroy-renderer` |
| `sdl2:with-event-loop` | `main.lisp` | **No direct equivalent.** Hand-roll a loop with `sdl3:poll-event` (or `sdl3:wait-event` `wait-event-timeout`); whole program may use `sdl3:run-app` |
| `sdl2:make-this-thread-main` | `main.lisp` | `sdl3:enter-app-main-callbacks` or `sdl3:run-app` (`sdl3/main`) |
| `sdl2:in-main-thread` | `main.lisp`, `color-picker.lisp`, `display.lisp`, `drawing.lisp`, `graphics.lisp` | Usually drop; else `sdl3:run-on-main-thread` |
| `sdl2:push-quit-event` | `main.lisp` | `sdl3:push-event` with a quit event struct (SDL3 removed `SDL_PushQuitEvent`; set `event.type = :quit` and push) |
| `sdl2:quit` | `main.lisp` | `sdl3:quit`  `sdl3:quit-sub-system` |
| `sdl2:set-hint` | `main.lisp` | `sdl3:set-hint` |
| `sdl2:get-window-flags` | `main.lisp` | `sdl3:get-window-flags` (flag constants renamed — `WINDOW_HIGH_PIXEL_DENSITY` etc.; `SDL_WINDOW_SHOWN` removed) |
| `sdl2:get-window-size` | `display.lisp` | `sdl3:get-window-size` (video) |
| `sdl2:get-window-title` | `main.lisp` | `sdl3:get-window-title` |
| `sdl2:set-window-title` | `main.lisp` | `sdl3:set-window-title` |
| `sdl2:set-window-fullscreen` | `main.lisp` | `sdl3:set-window-fullscreen` (now takes a bool, not flags) |
| `sdl2:maximize-window` `minimize-window` | `main.lisp` | `sdl3:maximize-window` `sdl3:minimize-window` |
| `sdl2:get-renderer-output-size` | `main.lisp`, `display.lisp` | `sdl3:get-render-output-size` (multiple values) or `sdl3:get-current-render-output-size` |
| `sdl2:platform` | `platform.lisp` | `sdl3:get-platform` |

## 2. Rendering (drawing.lisp, graphics.lisp, display.lisp, view.lisp, color-picker.lisp, tree.lisp, image-buffer.lisp, utils.lisp)

| SDL2 function | Defined in | cl-sdl3 counterpart |
|---|---|---|
| `sdl2:render-clear` | `main.lisp`, `display.lisp`, `view.lisp` | `sdl3:render-clear` |
| `sdl2:render-present` | `display.lisp` | `sdl3:render-present` |
| `sdl2:render-copy` | `main.lisp`, `graphics.lisp`, `view.lisp`, `tree.lisp`, `color-picker.lisp` | `sdl3:render-texture` (SDL3 replaces copy/rect-pair with dst rect; srcrect NULL = whole texture) |
| `sdl2:render-copy-ex` | `drawing.lisp`, `utils.lisp` | `sdl3:render-texture-rotated` (angle only; center + flip args) or `sdl3:render-texture-affine` |
| `sdl2:render-fill-rect` | most files | `sdl3:render-fill-rect` |
| `sdl2:render-draw-rect` | `graphics.lisp`, `tree.lisp` | `sdl3:render-rect` |
| `sdl2:render-draw-line` | `display.lisp`, `graphics.lisp`, `tree.lisp` | `sdl3:render-line` |
| `sdl2:render-draw-lines` | `display.lisp` | `sdl3:render-lines` |
| `sdl2:render-draw-point` | `display.lisp`, `graphics.lisp`, `color-picker.lisp` | `sdl3:render-point` |
| `sdl2:render-draw-points` | `graphics.lisp` | `sdl3:render-points` |
| `sdl2:set-render-draw-color` | `display.lisp`, `tree.lisp` | `sdl3:set-render-draw-color` |
| `sdl2:set-render-target` | many files | `sdl3:set-render-target` |
| `sdl2:render-set-viewport` | `main.lisp` | `sdl3:set-render-viewport` |
| `sdl2:create-texture` | `utils.lisp`, `color-picker.lisp` | `sdl3:create-texture` (texture is a wrapped struct object in cl-sdl3, not a raw pointer) |
| `sdl2:create-texture-from-surface` | many files | `sdl3:create-texture-from-surface` |
| `sdl2:destroy-texture` | many files | `sdl3:destroy-texture` |
| `sdl2:current-renderer` | `graphics.lisp`, `color-picker.lisp`, `tree.lisp` | **No equivalent** — SDL3 has no global current renderer; thread the renderer through explicitly |
| `sdl2:+sdl-textureaccess-target+` | `utils.lisp` | `:target` / cl-sdl3 `texture-access` type value |
| `sdl2:+pixelformat-rgba8888+` | `utils.lisp` | `sdl3:+pixel-format-rgba8888+` (`sdl3/pixels`) |

## 3. Rect / point helpers (sdl2 binding utilities)

| SDL2 function | Defined in | cl-sdl3 counterpart |
|---|---|---|
| `sdl2:make-rect` | `main.lisp`, `display.lisp`, `graphics.lisp` | `sdl3:make-rect` / foreign alloc via `sdl3/rect` types |
| `sdl2:with-rects` | many files | **No macro** — allocate with `cffi:with-foreign-object` + `cffi:mem-struct`, or wrap your own `with-rects` in the sdl3 frontend |
| `sdl2:rect-x/y/w/h` | `display.lisp` | **No accessors** — `rect` is a plain `deflsp-type` struct in `sdl3/rect/type.lisp`; read slots with `cffi:foreign-slot-value` |
| `sdl2:free-rect` | `graphics.lisp` | not needed with stack allocation |
| `sdl2:make-points`/`with-points`/`sdl-point`/`points` | `display.lisp` | cl-sdl3 `point` struct type (`sdl3/rect/type.lisp`); hand-roll a `with-points` helper — helper macros like this are worth recreating once in `frontends/sdl3/utils.lisp` |
| `sdl2:sdl-get-render-target` (lem local fn) | lem's own code | n/a — port the lem-side helper, use `sdl3:get-render-target` |

## 4. Surfaces (drawing, font, text-surface-cache, main, graphics)

| SDL2 function | Defined in | cl-sdl3 counterpart |
|---|---|---|
| `sdl2:free-surface` | many files | `sdl3:destroy-surface` (renamed!) |
| `sdl2:surface-width` / `surface-height` | many files | **No wrapper** — `surface` is a plain struct type (`sdl3/surface/type.lisp`, slots `w`/`h`); read with `cffi:foreign-slot-value` or write local helpers |
| `sdl2-ttf:render-utf8-blended` | `drawing.lisp`, `main.lisp`, `tree.lisp` | `sdl3-ttf:render-text-blended` (SDL3_ttf: UTF-8 is the only encoding now) |
| `sdl2-ttf:render-text-solid` | `font.lisp` | `sdl3-ttf:render-text-solid` |
| `sdl2-ttf:open-font` / `close-font` | `font.lisp`, `icon-font.lisp`, `tree.lisp` | `sdl3-ttf:open-font` / `sdl3-ttf:close-font` (ptsize is a **float** in SDL3) |
| `sdl2-ttf:init` / `quit` | `main.lisp` | `sdl3-ttf:init` / `sdl3-ttf:quit` (idempotent in SDL3) |
| `sdl2-image:load-image` | `main.lisp`, `drawing.lisp`, `graphics.lisp` | `sdl3-image:img-load` (or `sdl3-image:load-texture` to skip the surface step) |
| `sdl2-image:init` / `quit` | `main.lisp` | **Removed in SDL3_image** — `IMG_Load` auto-inits; just delete these calls |

## 5. Events / keyboard (keyboard.lisp, main.lisp)

| SDL2 function | Defined in | cl-sdl3 counterpart |
|---|---|---|
| `sdl2:sym-value` (keysym sym) | `keyboard.lisp` | `SDL_KeyboardEvent` struct field `scancode`/`key` — read from the cl-sdl3 event struct; `sdl3:get-scancode-from-key` / `sdl3:get-key-from-scancode` for translation |
| `sdl2:mod-value` | `keyboard.lisp` | `sdl3:get-mod-state` or the event struct's `mod` field (keymod constants renamed: `LSHIFT` → `LShift` style constants — check `sdl3/keyboard` exports) |
| `+sdlk-...+` constants | `keyboard.lisp` | renamed keycodes in `sdl3/keyboard` (`: keycode` enum; e.g. `sdlk-return` may become `sdlk-return` still — verify each) |
| `+kmod-...+` constants | `keyboard.lisp` | `kmod` constants in `sdl3/keyboard` (renamed: `KMOD_LSHIFT` etc. → e.g. `+lshift+`) |
| `sdl2:start-text-input` | `main.lisp` | `sdl3:start-text-input` |
| window event constants `+sdl-windowevent-exposed+` etc. | `main.lisp` | event enums in `sdl3/events` (`:window-event-exposed` style — SDL3 split events into `SDL_EVENT_*`) |
| `sdl2:with-event-loop` (see §1) | `main.lisp` | loop over `sdl3:poll-event`; SDL3 event structs are polled by value |

## 6. Mouse / cursor (mouse.lisp, main.lisp)

| SDL2 function | Defined in | cl-sdl3 counterpart |
|---|---|---|
| `sdl2:hide-cursor` / `show-cursor` | `mouse.lisp` | `sdl3:hide-cursor` / `sdl3:show-cursor` |
| `sdl2:mouse-state` | `main.lisp` | `sdl3:get-mouse-state` (button bitmasks: `SDL_BUTTON_LMASK` → cl-sdl3 `+button-lmask+`-style constant in `sdl3/mouse`) |
| `+sdl-button-left+` etc. | `main.lisp` | `sdl3/mouse` button constants |

## 7. Things with NO SDL3 counterpart — don't create equivalents

| SDL2 frontend item | Why it's not needed |
|---|---|
| `sdl2:in-main-thread` | SDL3 moved to main-callback model (`SDL_RunApp`); event/render calls are safe. Only `sdl3:run-on-main-thread` for genuinely main-only cases |
| `sdl2:current-renderer` | SDL3 has no renderer-global state; renderer is an explicit argument everywhere |
| `sdl2-image:init` / `sdl2-image:quit` | SDL3_image initializes implicitly on first load |
| `sdl2:with-window` / `with-renderer` / `with-rects` / `with-points` macros | Binding-library conveniences, not SDL APIs. Recreate small local macros in `frontends/sdl3/utils.lisp` once, rather than per-call |
| `sdl2:rect-x/y/w/h`, `surface-width/height` accessors | Just foreign struct slot readers in cl-sdl3; trivial local helpers at most |
| `sdl2-ttf` UTF-8 vs Latin-1 distinction (`render-text-solid` vs `render-utf8-solid`) | SDL3_ttf only speaks UTF-8; one function per style |
| `sdl2:push-quit-event` as a special API | SDL3: push a generic quit `SDL_Event` via `sdl3:push-event` |
| Window "shown" flag handling (`SDL_WINDOW_SHOWN`) | Flag removed in SDL3 — windows are shown via `sdl3:show-window` |
| `sdl2:make-this-thread-main` | SDL3's `SDL_RunApp` / `enter-app-main-callbacks` replaces the main-thread dance |

## 8. Already ported in frontends/sdl3/

`log.lisp`, `platform.lisp`, `resource.lisp`, `utils.lisp`, `sdl3.lisp`
exist — check `sdl3.lisp` before adding helpers so you don't duplicate.
Not yet ported from the list above: main loop/event plumbing, drawing,
display, view, tree, color-picker, font, text-surface-cache, icon,
keyboard, mouse, image-buffer, graphics, wm.
