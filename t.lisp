(defpackage :icfpc-2015-test
  (:use :common-lisp
        :icfpc-2015
        :prove
        :anaphora))
(in-package :icfpc-2015-test)

(setf *enable-colors* t)
(plan nil)
(subtest "random generator"
  (let ((*rand/state*)
        (nums '(0 24107 16552 12125 9427
                13152 21440 3383 6873 16117)))
    (rand/seed 17)
    (dotimes (i (length nums))
      (is (rand/next) (nth i nums) :test #'=))))

(finalize)

