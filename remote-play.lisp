(in-package #:remote-play)

(defvar *stream* nil)

(defun-g vert ((vert g-pnt))
  (pos vert))

(defun-g frag ()
  (v! 0 0 0 0))

(defpipeline-g draw () (vert g-pnt) (frag))

(defun loop-step ()
  (update-repl-link)
  (step-host)
  (step-remote-controls)
  (as-frame
    (map-g #'draw *stream*)))

(defun init ()
  (init-remote-control-server)
  (unless *stream*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *stream* (make-buffer-stream v :index-array i)))))

(def-simple-main-loop play (:on-start #'init)
  (loop-step))
