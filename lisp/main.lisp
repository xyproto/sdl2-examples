(in-package :sdl2-examples)

(require :sdl2)

(defun renderer-test ()
  "Display an image of a grumpy cat"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Hello World!" :x 100 :y 100 :w 620 :h 387 :flags '(:shown))
      (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
        (let* (
          (bmp (sdl2:load-bmp "../img/grumpy-cat.bmp"))
          (tex (sdl2:create-texture-from-surface ren bmp)))
            (loop repeat 20 do
              (sdl2:render-clear ren)
              (sdl2:render-copy ren tex)
              (sdl2:render-present ren)
              (sdl2:delay 100)))))))
