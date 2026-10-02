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
