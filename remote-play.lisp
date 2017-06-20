(in-package #:remote-play)

(defvar *stream* nil)
(defvar *camera* nil)
(defvar *sphere* nil)

;;------------------------------------------------------------
;; Camera

(defclass camera ()
  ((pos :initform (v! 0 0 0) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defun world->clip (camera)
  (let ((vp (resolution (current-viewport))))
    (m4:* (rtg-math.projection:perspective (x vp) (y vp) 0.1 40f0 60f0)
          (m4:translation (v3:negate (pos camera)))
          (q:to-mat4 (q:inverse (rot camera))))))

;;------------------------------------------------------------
;; Objects

(defclass thing ()
  ((pos :initform (v! 0 0 -10) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defun model->world (thing)
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

;;------------------------------------------------------------
;; Pipeline

(defun-g vert ((vert g-pnt)
               &uniform (c0 :vec4) (c1 :vec3)
               (model->world :mat4) (world->clip :mat4))
  (let* ((pos (pos vert))
         (pos (+ pos (v! 0 0 0)))
         (pos (+ pos (v! (s~ c0 :xy) 0)))
         (pos (v! pos 1)))
    (values (* world->clip model->world pos)
            c1)))

(defun-g frag ((col :vec3))
  col)

(defpipeline-g draw () (vert g-pnt) (frag :vec3))

;;------------------------------------------------------------
;; The rest

(defun loop-step ()
  (setf (viewport-resolution (current-viewport))
        (surface-resolution (current-surface *cepl-context*)))
  (update-repl-link)
  (step-host)
  (step-remote-controls)
  (as-frame
    (map-g #'draw *stream*
           :model->world (model->world *sphere*)
           :world->clip (world->clip *camera*)
           :c0 (print (remote-control-val (remote 1)))
           :c1 (v! (x (remote-control-val (remote 2)))
                   (x (remote-control-val (remote 3)))
                   (x (remote-control-val (remote 4)))))))

(defun init ()
  (init-remote-control-server)
  (unless *stream*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *stream* (make-buffer-stream v :index-array i))))
  (setf *camera* (make-instance 'camera))
  (setf *sphere* (make-instance 'thing)))

(def-simple-main-loop play (:on-start #'init)
  (loop-step))
