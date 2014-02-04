;; liv - leth image viewer (simple image viewer written in common lisp)
;; © Copyright 2014 Viktor "leth" Lindberg <leth@fripost.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ql:quickload "cl-devil") ;; this need ilut, arch package is without.
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-image")
(ql:quickload "cl-opengl")

;; TODO: fix a list of files taken in from command line arguments
;; TODO: check if it can do svg and if not then fix it
;; TODO: fix gif animations
;; TODO: fix scaling, zooming and moving around
;; TODO: fix aspect ratio

;; http://content.gpwiki.org/index.php/DevIL:Tutorials:Basics

(defparameter *image-file-name* "/home/leth/projects/cl-image-viewer/test.png")
(defparameter *window* nil)
(defparameter *window-width* nil)
(defparameter *window-height* nil)
(defparameter *window-past-width* 0)
(defparameter *window-past-height* 0)
(defparameter *window-fullscreen* nil)
(defparameter *texture* nil)
(defparameter *image* nil)
(defparameter *image-width* nil)
(defparameter *image-height* nil)
(defparameter *image-scale* nil)
(defparameter *display-image* nil)
(defparameter *display-x* 0)
(defparameter *display-y* 0)
(defparameter *display-width* nil)
(defparameter *display-height* nil)


(defparameter *scaling-methods* (list :scale-bell 
				      :scale-mitchell 
				      :scale-lanczos3
				      :scale-bspline
				      :scale-triangle
				      :scale-box))
(defparameter *scaling-method-number* 0)
(defparameter *scaling-method* :scale-bell)

(defun set-scaling-method (number)
  (setf *scaling-method-number* number 
	*scaling-method* (nth number *scaling-methods*)))

(defun arguments ()
  (or
   #+SBCL *posix-argv*
   #+CLISP *args*
   #+CMU extensions:*command-line-words*
   nil))

(defun display-image ()
  (il:with-bound-image *display-image*
    (il:tex-image *image-width* *image-height* 0 4 :rgba :unsigned-byte 
		  (cffi:null-pointer))
    (il:clear-image)
    (il:blit *image* 0 0 0 0 0 0 *image-width* *image-height* 1)
    (il:flip-image) ;why is this needed with blit but not with copy?
    (ilu:image-parameter :filter *scaling-method*)

    ;; aspect ratio: X/Y   12,3 = 4  12,4 = 3 (wider == less)
    ;; if (image aspect ratio > screen aspect ration) (image is narrower)
    ;;   then → image height should match screen height (scale according to height)
    ;;   else → image widht should match screen width (scale according to width)
    (let* ((image-aspect-ratio (/ *image-width* *image-height*))
	   (window-aspect-ratio (/ *window-width* *image*)))
      (if (> image-aspect-ratio window-aspect-ratio)
	  (setf *display-width* *window-width*
		*display-height* (floor (* *image-height* 
					   (/ *window-width* *image-width*))))
	  (setf *display-width* (floor (* *image-width* 
					  (/ *window-height* *image-height*)))
		*display-height* *window-height*))
      (format 't "i-w:~a i-h:~a i-r:~a w-w:~a w-h:~a w-r:~a d-w:~a d-h:~a" 
	      *image-width* 
	      *image-height* 
	      image-aspect-ratio
	      *window-width*
	      *window-height*
	      window-aspect-ratio
	      *display-width* 
	      *display-height*)
      )
    (ilu:scale *display-width* *display-height* 0)
    (gl:color 1 1 1)
    (gl:tex-image-2d :texture-2d
		     0
		     :rgba
		     *display-width* ; (il:get-integer :image-width)
		     *display-height*; (il:get-integer :image-height)
		     0
		     :rgba
		     :unsigned-byte
		     (il:get-data))     
    )
  )


(defun load-image-file (image-file-name)
  (il:with-bound-image *image*
    (il:load-image (namestring image-file-name))
    (setf *image-width* (il:image-width)
	  *image-height* (il:image-height))
    (il:convert-image :rgba :unsigned-byte)))

(defun draw-scene ()
  (gl:clear)
  (gl:color 1 1 1)
  (gl:with-primitive :quads
    (gl:tex-coord 0 0)
    (gl:vertex 0 0 0)
    (gl:tex-coord 1 0)
    (gl:vertex *display-width* 0 0) ;; size
    (gl:tex-coord 1 1)
    (gl:vertex *display-width* *display-height* 0) ;; size
    (gl:tex-coord 0 1)
    (gl:vertex 0 *display-height* 0)) ;; size
  (gl:flush)
  (sdl:update-display))

(defun change-scale ()
  (if (eq (length *scaling-methods*) (+ 1 *scaling-method-number*))
      (set-scaling-method 0)
      (set-scaling-method 
       (+ 1 *scaling-method-number*)))
  (display-image)
  (draw-scene))

(defun set-window (width height &optional (fullscreen nil))
  (if (not (equal fullscreen *window-fullscreen*))
      (setf *window-past-width* (sdl:width *window*)
	    *window-past-height* (sdl:height *window*)))
  (setf *window-fullscreen* (if fullscreen t nil)
	*window* (sdl:window width height
			     :title-caption "liv"
			     :icon-caption "liv"
			     :double-buffer t
			     :sw t
			     :opengl t
			     :resizable t
			     :fullscreen (if fullscreen t nil)
			     :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
			     :fps (make-instance 'sdl:fps-timestep)))
  (gl:matrix-mode :projection)
  (gl:clear :depth-buffer-bit)
  (gl:clear :color-buffer-bit)
  (gl:load-identity)
;  (gl:ortho 0 width height 0 0 1)
  (gl:ortho 0 (sdl:width *window*) (sdl:height *window*) 0 0 1)
  (gl:matrix-mode :modelview)
  (setq *window-width* (sdl:width *window*)
	*window-height* (sdl:height *window*))
  (display-image))

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
		      :fullscreen nil
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
      (ilut:init)
      (il:init)
      (ilut:renderer :opengl)
      (ilut:enable :opengl-conv)
      (setq *window-width* (sdl:width *window*)
	    *window-height* (sdl:height *window*)
	    *window-past-width* (sdl:width *window*)
	    *window-past-height* (sdl:height *window*)
	    *image* (il:gen-image)
	    *display-image* (il:gen-image))
      (il:with-bound-image *display-image*
	(ilu:image-parameter :filter *scaling-method*))
      (load-image-file *image-file-name*)
      (display-image)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event
	 ()
	 (cond
	   ((sdl:key-down-p :sdl-key-escape) (sdl:push-quit-event)) ;; Keep this!
	   ((sdl:key-down-p :sdl-key-q) (sdl:push-quit-event))
	   ((sdl:key-down-p :sdl-key-r) (display-image))
	   ((sdl:key-down-p :sdl-key-s) (change-scale))
	   ((sdl:key-down-p :sdl-key-f) (set-window *window-past-width* *window-past-height* (not *window-fullscreen*)))
	   ;;	 ((sdl:key-down-p :sdl-key-n) (next-image))
	   ;;	 ((sdl:key-down-p :sdl-key-p) (previous-image))
	   ))
	(:video-expose-event () (sdl:update-display))
	(:video-resize-event 
	 (:w new-width :h new-height)
	 (set-window new-width new-height))
	(:idle
	 ()
	 (sdl:with-timestep ()) ;; not sure it's needed also see *window*
	 (draw-scene)
	 ))))
  (il:delete-images *image* *display-image*))

(start-displayer)
