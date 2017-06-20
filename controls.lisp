(in-package #:remote-play.controls)

(defvar *control-server* nil)

(define-control norm4 (:static t) rtg-math.types:vec4 (v! 0 0 0 0))

(define-input-source remote-control ()
  (val norm4))

(defvar *controls* (make-array 1 :element-type '(or remote-control null)
                               :adjustable t :fill-pointer 0))

(defun remote (&optional (n 0))
  (when (>= n (length *controls*))
    (adjust-array *controls* (1+ n) :fill-pointer (1+ n) :initial-element nil)
    (setf (aref *controls* n) (make-remote-control)))
  (aref *controls* n))

(defun init-remote-control-server ()
  (unless *control-server*
    (setf *control-server* (live-remote:make-server 1234))))

(defun step-remote-controls ()
  (unless *control-server*
    (break "Ya didnt start the server"))
  (loop :for msg :in (live-remote:read-all-remote-messages) :do
     (when (listp msg)
       (destructuring-bind (source-id group-id data) msg
         (declare (ignore group-id))
         (let ((remote (remote source-id)))
           (set-remote-control-val remote (get-internal-real-time) data))))))
