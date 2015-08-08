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

(subtest "points"
  (subtest "equality"
    (is (make-point 42 15) (make-point 42 15) :test #'point=)
    (isnt (make-point 234 15) (make-point 42 6) :test #'point=)
    (is (make-point3 9 42 1) (make-point3 9 42 1) :test #'point3=)
    (isnt (make-point3 9 213 1) (make-point3 12 42 1) :test #'point3=))
  (subtest "+"
    (is (point+ (make-point 5 42) (make-point -3 19))
        (make-point 2 61) :test #'point=)
    (is (point3+ (make-point3 5 42 2) (make-point3 -3 19 4))
        (make-point3 2 61 6) :test #'point3=))
  (subtest "-"
    (is (point- (make-point 0 -42) (make-point 5 12))
        (make-point -5 -54) :test #'point=)
    (is (point3- (make-point3 0 -42 165) (make-point3 5 12 1))
        (make-point3 -5 -54 164) :test #'point3=))
  (subtest "conversion"
    (subtest "zero"
      (is (point->point3 (make-point 0 0))
          (make-point3 0 0 0) :test #'point3=)
      (is (point3->point (make-point3 0 0 0))
          (make-point 0 0) :test #'point=))
    (subtest "horizontal"
      (subtest "(1 . 0)"
        (is (point->point3 (make-point 1 0))
            (make-point3 1 -1 0) :test #'point3=)
        (is (point3->point (make-point3 1 -1 0))
            (make-point 1 0) :test #'point=))
      (subtest "(4 . 0)"
        (is (point->point3 (make-point 4 0))
            (make-point3 4 -4 0) :test #'point3=)
        (is (point3->point (make-point3 4 -4 0))
            (make-point 4 0) :test #'point=))
      (subtest "(-1 . 0)"
        (is (point->point3 (make-point -1 0))
            (make-point3 -1 1 0) :test #'point3=)
        (is (point3->point (make-point3 -1 1 0))
            (make-point -1 0) :test #'point=))
      (subtest "(-4 . 0)"
        (is (point->point3 (make-point -4 0))
            (make-point3 -4 4 0) :test #'point3=)
        (is (point3->point (make-point3 -4 4 0))
            (make-point -4 0) :test #'point=)))
    (subtest "vertical"
      (subtest "(0 . 1)"
        (is (point->point3 (make-point 0 1))
            (make-point3 0 -1 1) :test #'point3=)
        (is (point3->point (make-point3 0 -1 1))
            (make-point 0 1) :test #'point=))
      (subtest "(0 . 4)"
        (is (point->point3 (make-point 0 4))
            (make-point3 -2 -2 4) :test #'point3=)
        (is (point3->point (make-point3 -2 -2 4))
            (make-point 0 4) :test #'point=))
      (subtest "(0 . -1)"
        (is (point->point3 (make-point 0 -1))
            (make-point3 1 0 -1) :test #'point3=)
        (is (point3->point (make-point3 1 0 -1))
            (make-point 0 -1) :test #'point=))
      (subtest "(0 . -4)"
        (is (point->point3 (make-point 0 -4))
            (make-point3 2 2 -4) :test #'point3=)
        (is (point3->point (make-point3 2 2 -4))
            (make-point 0 -4) :test #'point=)))
    (subtest "other"
      (subtest "(1 . 2)"
        (is (point->point3 (make-point 1 2))
            (make-point3 0 -2 2) :test #'point3=)
        (is (point3->point (make-point3 0 -2 2))
            (make-point 1 2) :test #'point=))
      (subtest "(-4 . 1)"
        (is (point->point3 (make-point -4 1))
            (make-point3 -4 3 1) :test #'point3=)
        (is (point3->point (make-point3 -4 3 1))
            (make-point -4 1) :test #'point=))
      (subtest "(-3 . -5)"
        (is (point->point3 (make-point -3 -5))
            (make-point3 0 5 -5) :test #'point3=)
        (is (point3->point (make-point3 0 5 -5))
            (make-point -3 -5) :test #'point=))
      (subtest "(1 . -2)"
        (is (point->point3 (make-point 1 -2))
            (make-point3 2 0 -2) :test #'point3=)
        (is (point3->point (make-point3 2 0 -2))
            (make-point 1 -2) :test #'point=))))
  (subtest "rotation"
    (is (point/rotate-ccw (make-point 0 0))
        (make-point 0 0) :test #'point=)
    (is (point/rotate-cw (make-point 0 0))
        (make-point 0 0) :test #'point=)
    (let ((map
           (list
            ;; dist = 1
            (cons (make-point -1 0) (make-point -1 -1))
            (cons (make-point -1 -1) (make-point 0 -1))
            (cons (make-point 0 -1) (make-point 1 0))
            (cons (make-point 1 0) (make-point 0 1))
            (cons (make-point 0 1) (make-point -1 1))
            (cons (make-point -1 1) (make-point -1 0))
            ;; dist = 2
            (cons (make-point -2 0) (make-point -1 -2)) ;; 1
            (cons (make-point -2 -1) (make-point 0 -2)) ;; 2
            (cons (make-point -1 -2) (make-point 1 -2)) ;; 3
            (cons (make-point 0 -2) (make-point 1 -1))  ;; 4
            (cons (make-point 2 0) (make-point 1 2))    ;; 7
            (cons (make-point 0 2) (make-point -2 1))  ;; 10
            ;; dist = 3
            (cons (make-point -3 0) (make-point -2 -3)) ;; 1
            (cons (make-point 2 -2) (make-point 2 1)) ;; 8
            )))
      (dolist (pair map)
        (is (point/rotate-cw (car pair)) (cdr pair)
            :test #'point=)
        (is (point/rotate-ccw (cdr pair)) (car pair)
            :test #'point=)))))

(subtest "unit"
  (subtest "equality"
    (is (make-instance
                 'unit
                 :cells (list
                         (make-point 0 0)
                         (make-point 2 0)
                         (make-point 1 2))
                 :pivot (make-point 1 1))
        (make-instance
                 'unit
                 :cells (list
                         (make-point 0 0)
                         (make-point 2 0)
                         (make-point 1 2))
                 :pivot (make-point 1 1))
        :test #'unit=)
    (isnt (make-instance
           'unit
           :cells (list
                   (make-point 0 0)
                   (make-point 2 0)
                   (make-point 1 2))
           :pivot (make-point 1 1))
          (make-instance
           'unit
           :cells (list
                   (make-point 0 0)
                   (make-point 2 0)
                   (make-point 1 2))
           :pivot (make-point 42 42))
          :test #'unit=))
  (subtest "rotation"
    (is (unit/rotate-cw
         (make-instance
          'unit
          :cells (list
                  (make-point 0 0)
                  (make-point 2 0)
                  (make-point 1 2))
          :pivot (make-point 1 1)))
        (make-instance
         'unit
         :cells (list
                 (make-point 1 -1)
                 (make-point 2 1)
                 (make-point 0 1))
         :pivot (make-point 1 1))
        :test #'unit=)))

(finalize)
