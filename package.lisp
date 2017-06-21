;;;; package.lisp

(uiop:define-package #:remote-play.controls
    (:use #:cl #:skitter #:skitter.internals :rtg-math)
  (:export :remote :init-remote-control-server :step-remote-controls
           :remote-control-val))

(uiop:define-package #:remote-play
    (:use #:cl #:cepl #:nineveh #:livesupport #:remote-play.controls :rtg-math
          :vari))
