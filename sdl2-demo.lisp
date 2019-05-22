
(load-file "std.lisp")

(require 'sdl2)

(defmacro with-sdl-init (flags . body)
  `(unwind-protect (progn
                     (SDL_Init ,flags)
                     ,@body)
     (SDL_Quit)))

(with-sdl-init SDL_INIT_VIDEO
  (let ((window (SDL_CreateWindow "foo" 0 0 640 400 0)))
    (println window)

    (let ((renderer (SDL_CreateRenderer window -1 (+ SDL_RENDERER_ACCELERATED
                                                     SDL_RENDERER_PRESENTVSYNC))))
      (println renderer)
      (SDL_DestroyRenderer renderer))))
