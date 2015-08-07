(defpackage :icfpc-2015
  (:use :cl
        :alexandria
        :anaphora
        :serapeum
        :cl-annot
        :annot.std
        ))
(in-package :icfpc-2015)
(cl-syntax:use-syntax :annot)

@export
(defun rand/make (seed)
  "From rosetta code"
  (let ((modulus (expt 2 31)))
    (lambda ()
      (aprog1 (ash seed -16)
        (setf seed (mod (+ (* seed 1103515245) 12345)
                        modulus))))))
@export
(defvar *rand/state* (rand/make 0))
@export
(defun rand/seed (seed)
  (setf *rand/state* (rand/make seed)))
@export
(defun rand/next ()
  (funcall *rand/state*))


