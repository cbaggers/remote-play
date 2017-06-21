(in-package #:remote-play)

(defvar *stream* nil)
(defvar *cone-stream* nil)
(defvar *sphere-stream* nil)
(defvar *cube-stream* nil)
(defvar *camera* nil)
(defvar *sphere* nil)
(defvar *fbo* nil)
(defvar *fbo-sampler* nil)
(defvar *screen-listener* nil)
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
;; Things

(defclass thing ()
  ((pos :initform (v! 0 0 -10) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defun model->world (thing)
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

;;------------------------------------------------------------
;; Draw the thing pass

(defun-g vert ((vert g-pnt)
               &uniform (model->world :mat4) (world->clip :mat4) (boost :vec4))

  (let* ((pos (* (pos vert) (+ (s~ boost :xyz) (vec3 1))))
         (world-pos (*  model->world (v! pos 1)))
         (world-norm (* (m4:to-mat3 model->world) (norm vert)))
         (clip-pos (* world->clip world-pos)))

    (values clip-pos
            (s~ world-pos :xyz)
            world-norm)))

(defun-g frag ((pos :vec3) (norm :vec3)
               &uniform (lpos :vec3) (cpos :vec3)
               (obj-color :vec3)
               (spec-power :float))
  (let* ((norm (normalize norm))
         ;;
         (spec-power spec-power)
         (ldir (normalize (- lpos pos)))
         (refl (reflect (- ldir) norm))
         (vdir (normalize (- cpos pos)))
         ;;
         (amb 0.1)
         (dif (saturate (dot norm ldir)))
         (spec (* (pow (saturate (dot vdir refl)) 32) spec-power))
         (phong (+ amb dif spec)))
    (* obj-color phong)))

(defpipeline-g draw () (vert g-pnt) (frag :vec3 :vec3))

;;------------------------------------------------------------
;; Negate Pass

(defun-g negate-vert ((vert :vec2))
  (values (v! vert 0 1)
          (+ (vec2 0.5) (* vert 0.5))))

(defun-g negate-frag ((uv :vec2) &uniform (sam :sampler-2d))
  (- (vec4 1) (texture sam uv)))

(defpipeline-g negate-colors ()
  (negate-vert :vec2)
  (negate-frag :vec2))

;;------------------------------------------------------------
;; Step

(defun loop-step ()
  (setf (viewport-resolution (current-viewport))
        (surface-resolution (current-surface *cepl-context*)))
  (update-repl-link)
  (step-host)
  (step-remote-controls)
  (when (> (remote-x 6) 0f0)
    (setf *stream* *sphere-stream*))
  (when (> (remote-x 7) 0f0)
    (setf *stream* *cone-stream*))
  (when (> (remote-x 9) 0f0)
    (setf *stream* *cube-stream*))

  (if (> (remote-x 8) 0f0)
      (setf (rot *sphere*)
            (q:normalize
             (q:from-fixed-angles-v3
              (v3:*s (s~ (remote 1) :xzy) pi-f))))
      (setf (pos *sphere*)
            (v3:+ (v! 0 0 -10)
                  (v3:*s (s~ (remote 1) :xyz) 10f0))))

  ;;   (setf *stream* *sphere-stream*))
  (with-fbo-bound (*fbo*)
    (clear)
    (map-g #'draw *stream*
           :model->world (model->world *sphere*)
           :world->clip (world->clip *camera*)
           :lpos *light-pos*
           :cpos (pos *camera*)
           :obj-color (v! (remote-x 2) (remote-x 3) (remote-x 4))
           :spec-power (remote-x 5)
           :boost (remote 10)))
  (as-frame
    (map-g #'negate-colors (get-quad-stream-v2)
           :sam *fbo-sampler*)))

;;------------------------------------------------------------
;; Screen Size

(defun on-screen-size-change (size)
  (print size)
  (setf (viewport-resolution (current-viewport))
        (surface-resolution (current-surface *cepl-context*)))
  (reshape))

(defun reshape ()
  (when *fbo*
    (free (attachment-tex *fbo* 0))
    (free (attachment-tex *fbo* :d)))
  (setf *fbo* (make-fbo 0 :d))
  (setf *fbo-sampler* (sample (attachment-tex *fbo* 0))))

;;------------------------------------------------------------
;; Initialize

(defun init ()
  (init-remote-control-server)

  (setf (clear-color *cepl-context*) (v! 0.13 0.13 0.15 0))

  ;;
  ;; listen for screen events
  (unless *screen-listener*
    (setf *screen-listener*
          (skitter:listen-to (lambda (d &rest ignored)
                               (declare (ignore ignored))
                               (on-screen-size-change d))
                             (skitter:window 0) :size)))
  ;;
  ;; make the sphere
  (unless *sphere-stream*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *sphere-stream* (make-buffer-stream v :index-array i))))
  ;;
  ;; make the cone
  (unless *cone-stream*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:cone-gpu-arrays)
      (setf *cone-stream* (make-buffer-stream v :index-array i))))
  ;;
  ;; make the cube
  (unless *cube-stream*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *cube-stream* (make-buffer-stream v :index-array i))))
  ;;
  ;; set the default stream
  (setf *stream* *sphere-stream*)
  (unless *fbo*
    (reshape))
  (setf *camera* (make-instance 'camera))
  (setf *sphere* (make-instance 'thing)))

;;------------------------------------------------------------
;; Main Loop

(def-simple-main-loop play (:on-start #'init)
  (loop-step))
