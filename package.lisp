;;;; package.lisp

(uiop:define-package #:remote-play.controls
    (:use #:cl #:skitter #:skitter.internals :rtg-math)
  (:export :remote))

(uiop:define-package #:remote-play
    (:use #:cl #:cepl #:nineveh #:livesupport #:remote-play.controls))
