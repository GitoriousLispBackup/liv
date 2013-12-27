;; name: liv - leth image viewer
;; use skippy for gif?
;; use cl-devil for everything?

(ql:quickload "cl-devil") ;; this need ilut, arch package is without.
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-image")
(ql:quickload "cl-opengl")

;; TODO: fix a list of files taken in from command line arguments
;; TODO: fix scaling, zooming and moving around
;; TODO: fix gif animations

;; http://content.gpwiki.org/index.php/DevIL:Tutorials:Basics

(defparameter *image-file-name* "/home/leth/projects/cl-image-viewer/test.png")
(defparameter *window* nil)
(defparameter *texture* nil)

(defun arguments ()
  (or 
   #+SBCL *posix-argv*
   #+CLISP *args*
   #+CMU extensions:*command-line-words*
   nil))

(defun display-image (image-file-name)
  (il:load-image (namestring image-file-name))
  (il:convert-image :rgba :unsigned-byte)
  (ilu:scale (sdl:width *window*) (sdl:height *window*) 32)
  (gl:tex-image-2d :texture-2d
  		   0
		   :rgba
  		   (il:get-integer :image-width)
  		   (il:get-integer :image-height)
  		   0
		   :rgba
  		   :unsigned-byte
  		   (il:get-data))
  (il:delete-images)
)

(defun draw-scene ()
  (gl:color 1 1 1)
  (gl:with-primitive :quads
    (gl:tex-coord 0 0)
    (gl:vertex 0 0 0)
    (gl:tex-coord 1 0)
    (gl:vertex (sdl:width *window*) 0 0) ;; size
    (gl:tex-coord 1 1)
    (gl:vertex (sdl:width *window*) (sdl:height *window*) 0) ;; size
    (gl:tex-coord 0 1)
    (gl:vertex 0 (sdl:height *window*) 0)) ;; size
  (gl:flush)
  (sdl:update-display))


(defun start-displayer ()
  "Starts displayer"
  (sdl:with-init 
    ()
    (setf *window*
	  (sdl:window 0 0
		      :title-caption "liv"
		      :icon-caption "liv"
		      :double-buffer t
		      :sw t
		      :opengl t
		      :resizable t
		      :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
		      :fps (make-instance 'sdl:fps-timestep)))
    (when *window* 
    (gl:enable :texture-2d)
    (gl:matrix-mode :projection)
    (gl:clear :depth-buffer-bit)
    (gl:load-identity)
    (gl:ortho 0 (sdl:width *window*) (sdl:height *window*) 0 0 1)
    (gl:matrix-mode :modelview)
    (gl:clear-depth 1.0)
    (gl:clear-color 0 0 0 0)
    (gl:color 0 0 0)
    (sdl:enable-key-repeat nil nil)
    (setf *texture* (car (gl:gen-textures 1)))
    (gl:bind-texture :texture-2d *texture*)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (ilut:init) ;; i think so.. ? including v↓
    (il:init)
    (ilut:renderer :opengl)
    (ilut:enable :opengl-conv)
    (display-image *image-file-name*)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event
       ()
       (cond
	 ((sdl:key-down-p :sdl-key-escape) (sdl:push-quit-event)) ;; Keep this!
	 ((sdl:key-down-p :sdl-key-q) (sdl:push-quit-event))
;;	 ((sdl:key-down-p :sdl-key-n) (next-image))
;;	 ((sdl:key-down-p :sdl-key-p) (previous-image))
	 ))
      (:video-expose-event () (sdl:update-display))
      (:idle
       ()
       (sdl:with-timestep ()) ;; not sure it's needed also see *window*
       (draw-scene)
       )))))

  
(start-displayer)
