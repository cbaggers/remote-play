(in-package #:remote-play)

(defvar *stream* nil)
(defvar *camera* nil)
(defvar *sphere* nil)
(defparameter *light-pos* (v! 0 3 -8))

;;------------------------------------------------------------
;; Camera

(defclass camera ()
  ((pos :initform (v! 0 0 0) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defun world->clip (camera)
  (let ((vp (resolution (current-viewport))))
    (m4:* (rtg-math.projection:perspective (x vp) (y vp) 0.1 90f0 60f0)
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
         (pos (+ pos (v! (s~ c0 :xy) 0)))
         ;;
         (world-pos (*  model->world (v! pos 1)))
         (world-norm (* (m4:to-mat3 model->world) (norm vert)))
         ;;
         (clip-pos (* world->clip world-pos)))
    (values clip-pos
            c1
            (s~ world-pos :xyz)
            world-norm)))

(defun-g frag ((col :vec3) (pos :vec3) (norm :vec3)
               &uniform (lpos :vec3) (cpos :vec3)
               (spec-power :float))
  (let* ((norm (normalize norm))
         ;;
         (obj-col  col
           ;;(v! 1 0 0 0)
           )
         (spec-power spec-power)
         (ldir (normalize (- lpos pos)))
         (refl (reflect (- ldir) norm))
         (vdir (normalize (- cpos pos)))
         ;;
         (amb 0.1)
         (dif (saturate (dot norm ldir)))
         (spec (* (pow (saturate (dot vdir refl)) 32)
                  spec-power))
         (phong (+ amb dif spec)))
    (* obj-col phong)))

(defpipeline-g draw () (vert g-pnt) (frag :vec3 :vec3 :vec3))

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
           :lpos *light-pos*
           :cpos (pos *camera*)
           :c0 (v4:*s (remote-control-val (remote 1)) 10f0)
           :c1 (v! (x (remote-control-val (remote 2)))
                   (x (remote-control-val (remote 3)))
                   (x (remote-control-val (remote 4))))
           :spec-power (x (remote-control-val (remote 5))))))

(defun init ()
  (setf (clear-color *cepl-context*) (v! 0.13 0.13 0.15 0))
  (init-remote-control-server)
  (unless *stream*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *stream* (make-buffer-stream v :index-array i))))
  (setf *camera* (make-instance 'camera))
  (setf *sphere* (make-instance 'thing)))

(def-simple-main-loop play (:on-start #'init)
  (loop-step))
