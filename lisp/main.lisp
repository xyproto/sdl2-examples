(in-package :sdl2-examples)

;; Based on this example file:
;; https://github.com/lispgames/cl-sdl2/blob/master/examples/basic.lisp

(require :sdl2)
(require :cl-opengl)

(defun basic-test ()
  "The kitchen sink."
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (let ((controllers ())
              (haptic ()))

          ;; basic window/gl setup
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl2:gl-make-current win gl-context)
          (gl:viewport 0 0 800 600)
          (gl:matrix-mode :projection)
          (gl:ortho -2 2 -2 2 -2 2)
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:clear-color 0.0 0.0 1.0 1.0)
          (gl:clear :color-buffer)

          (format t "Opening game controllers.~%")
          (finish-output)
          ;; open any game controllers
          (loop :for i :upto (- (sdl2:joystick-count) 1)
                :do (when (sdl2:game-controller-p i)
                      (format t "Found gamecontroller: ~a~%"
                              (sdl2:game-controller-name-for-index i))
                      (let* ((gc (sdl2:game-controller-open i))
                             (joy (sdl2:game-controller-get-joystick gc)))
                        (setf controllers (acons i gc controllers))
                        (when (sdl2:joystick-is-haptic-p joy)
                          (let ((h (sdl2:haptic-open-from-joystick joy)))
                            (setf haptic (acons i h haptic))
                            (sdl2:rumble-init h))))))

          ;; main loop
          (format t "Beginning main loop.~%")
          (finish-output)
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
                      (let ((scancode (sdl2:scancode-value keysym))
                            (sym (sdl2:sym-value keysym))
                            (mod-value (sdl2:mod-value keysym)))
                        (cond
                          ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
                          ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
                          ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
                        (format t "Key sym: ~a, code: ~a, mod: ~a~%"
                                sym
                                scancode
                                mod-value)))

            (:keyup (:keysym keysym)
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit)))

            (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                          (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                                  x xrel y yrel state))

            (:controlleraxismotion
             (:which controller-id :axis axis-id :value value)
             (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
                     controller-id axis-id value))

            (:controllerbuttondown (:which controller-id)
                                   (let ((h (cdr (assoc controller-id haptic))))
                                     (when h
                                       (sdl2:rumble-play h 1.0 100))))

            (:idle ()
                   (gl:clear :color-buffer)
                   (gl:begin :triangles)
                   (gl:color 1.0 0.0 0.0)
                   (gl:vertex 0.0 1.0)
                   (gl:vertex -1.0 -1.0)
                   (gl:vertex 1.0 -1.0)
                   (gl:end)
                   (gl:flush)
                   (sdl2:gl-swap-window win))

            (:quit () t))

          (format t "Closing opened game controllers.~%")
          (finish-output)
          ;; close any game controllers that were opened as well as any haptics
          (loop :for (i . controller) :in controllers
                :do (sdl2:game-controller-close controller)
                    (sdl2:haptic-close (cdr (assoc i haptic)))))))))
