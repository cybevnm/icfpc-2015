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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
(defun make-point (x y)
  "Makes point in odd-row coordinate system"
  (cons x y))
@export
(defun point/x (point)
  (car point))
@export
(defun point/y (point)
  (cdr point))
@export
(defun point= (a b)
  (and
   (= (point/x a) (point/x b))
   (= (point/y a) (point/y b))))
@export
(defun point+ (a b)
  (make-point
   (+ (point/x a) (point/x b))
   (+ (point/y a) (point/y b))))
@export
(defun point- (a b)
  (make-point
   (- (point/x a) (point/x b))
   (- (point/y a) (point/y b))))
@export
(defclass point3 ()
  ((x :initarg :x :accessor point3/x)
   (y :initarg :y :accessor point3/y)
   (z :initarg :z :accessor point3/z)))
(defmethod print-object ((point3 point3) stream)
  (print-unreadable-object (point3 stream :type t)
    (format
     stream "x=~s y=~s z=~s"
     (point3/x point3) (point3/y point3) (point3/z point3))))
@export
(defun make-point3 (x y z)
  "Makes point in cubic coordinate system"
  (make-instance 'point3 :x x :y y :z z))
(defun point3/clone (point3)
  (make-point3
   (point3/x point3)
   (point3/y point3)
   (point3/z point3)))
@export
(defun point3+ (a b)
  (make-point3
   (+ (point3/x a) (point3/x b))
   (+ (point3/y a) (point3/y b))
   (+ (point3/z a) (point3/z b))))
@export
(defun point3- (a b)
  (make-point3
   (- (point3/x a) (point3/x b))
   (- (point3/y a) (point3/y b))
   (- (point3/z a) (point3/z b))))
@export
(defun point->point3 (point)
  ;; # convert odd-r offset to cube
  ;; x = col - (row - (row&1)) / 2
  ;; z = row
  ;; y = -x-z
  (let* ((col (point/x point))
         (row (point/y point))
         (x (- col (/ (- row (if (oddp row) 1 0)) 2)))
         (z row)
         (y (+ (- x) (- z))))
    (make-point3 x y z)))
@export
(defun point3->point (point3)
  ;; # convert cube to odd-r offset
  ;; col = x + (z - (z&1)) / 2
  ;; row = z
  (let ((x (point3/x point3))
        (z (point3/z point3)))
    (make-point
     (+ x (/ (- z (if (oddp z) 1 0)) 2))
     z)))
@export
(defun point3= (a b)
  (and
   (= (point3/x a) (point3/x b))
   (= (point3/y a) (point3/y b))
   (= (point3/z a) (point3/z b))))
@export
(defun point3/rotate-ccw (point3)
  (lret ((result (point3/clone point3)))
    (with-slots (x y z) result
      (psetf x (- y)
             y (- z)
             z (- x)))))
@export
(defun point3/rotate-cw (point3)
  (lret ((result (point3/clone point3)))
    (with-slots (x y z) result
      (psetf x (- z)
             y (- x)
             z (- y)))))
;; TODO: optimization memoize ?
@export
(defun point/rotate-cw (point)
  (point3->point
   (point3/rotate-cw
    (point->point3 point))))
;; TODO: optimization memoize ?
@export
(defun point/rotate-ccw (point)
  (point3->point
   (point3/rotate-ccw
    (point->point3 point))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
(defclass unit ()
  ((cells :initarg :cells :accessor unit/cells
          :documentation "List of points")
   (pivot :initarg :pivot :accessor unit/pivot
          :documentation "Point")
   (pos :initarg :pos :initform (make-point 0 0)
        :accessor unit/pos
        :documentation "Position of top-left cell on
                        the board")))
(defun unit/clone (unit)
  (with-slots (cells pivot pos) unit
    (make-instance
     'unit :cells cells
     :pivot pivot :pos pos)))
(defun unit/width (unit)
  (let ((min-x most-positive-fixnum)
        (max-x most-negative-fixnum))
    (dolist (c (unit/cells unit))
      (let ((x (point/x c)))
        (setf min-x (min x min-x)
              max-x (max x max-x))))
    (- max-x min-x)))
(defun unit/rotate (unit rotator)
  (with-slots (cells pivot) unit
    (make-instance
     'unit
     :cells (mapcar
             (lambda (p)
               (let ((pivot3 (point->point3 pivot)))
                 (point3->point
                  (point3+
                   (funcall rotator
                            (point3-
                             (point->point3 p)
                             pivot3))
                   pivot3))))
             cells)
     :pivot pivot)))
@export 
(defun unit/rotate-ccw (unit)
  (unit/rotate unit #'point3/rotate-ccw))
@export
(defun unit/rotate-cw (unit)
  (unit/rotate unit #'point3/rotate-cw))
(defun unit/valid-dx-dy-p (dx dy)
  (or (and (= dx 1) (= dy 0))
      (and (= dx -1) (= dy 0))
      (and (= dx 1) (= dy 1))
      (and (= dx -1) (= dy 1))))
(defun unit/move (unit dx dy)
  (assert (unit/valid-dx-dy-p dx dy))
  (lret ((result (unit/clone unit)))
    (setf (unit/pos result)
          (point+ (unit/pos result) (make-point dx dy)))))
(defun unit/move-w (unit)
  (unit/move unit -1 0))
(defun unit/move-e (unit)
  (unit/move unit 1 0))
(defun unit/move-sw (unit)
  (let ((y (point/y (unit/pos unit))))
    (unit/move unit -1 (if (evenp y) -1 0))))
(defun unit/move-se (unit)
  (let ((y (point/y (unit/pos unit))))
    (unit/move unit -1 (if (evenp y) 0 1))))
@export
(defun unit= (a b)
  (let ((a-cells (unit/cells a))
        (b-cells (unit/cells b)))
    (or (eq a b)
        (and (= (length a-cells) (length b-cells))
             (every
              (lambda (pair)
                (point= (first pair) (second pair)))
              (rutils:zip a-cells b-cells))
             (point= (unit/pivot a) (unit/pivot b))))))
(defun unit/abs-pos (unit point)
  "POS is in unit's coordinates. Return point's 
   POS in absolute coordinates"
  (point3->point
   (point3+
    (point->point3 point)
    (point->point3 (unit/pos unit)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
(defclass board ()
  ((cells :accessor board/cells
          :documentation "Possible states are :empty
                          and :full")))
(defun board/clone (board)
  (let ((width (board/width board))
        (height (board/height board)))
    (aprog1 (make-instance
             'board
             :width width 
             :height height)
      (dotimes (y height)
        (dotimes (x width)
          (setf (board/cell it x y)
                (board/cell board x y)))))))
(defun board/dimension (board i)
  (array-dimension (board/cells board) i))
(defun board/width (board)
  (board/dimension board 0))
(defun board/height (board)
  (board/dimension board 1))
(defmethod initialize-instance
    :after ((board board) &key width height)
  (setf (board/cells board)
        (make-array `(,width ,height)
                    :initial-element :empty)))
(defun cell->symbol (cell)
  (ecase cell
    (:empty #\.)
    (:full #\*)))
(defun board/cell (board x y)
  (aref (board/cells board) x y))
(defun (setf board/cell) (cell board x y)
  (setf (aref (board/cells board) x y) cell))
(defun board/cell* (board point)
  (board/cell
   board (point/x point) (point/y point)))
(defun (setf board/cell*) (cell board point)
  (setf (board/cell
         board (point/x point) (point/y point))
        cell))
;; TODO: colors ?
(defun board/dump (board &key unit)
  (dotimes (y (board/height board))
    (dotimes (x (board/width board))
      (let ((cell-is-here
             (when unit
               (dolist (c (unit/cells unit))
                 (when (point=
                        (unit/abs-pos unit c)
                        (make-point x y))
                   (return t))))))
        (if cell-is-here
            (format
             t (if (evenp y) "@ " " @"))
            (format
             t (if (evenp y) "~A " " ~A")
             (cell->symbol (board/cell board x y))))))
    (format t "~%")))
(defun board/valid-unit-p (board unit)
  "Return t if unit is in valid location"
  (dolist (c (unit/cells unit) t)
    (let* ((abs-pos (unit/abs-pos unit c))
           (abs-x (point/x abs-pos))
           (abs-y (point/y abs-pos)))
      (unless (and (>= abs-x 0)
                   (< abs-x (board/width board))
                   (>= abs-y 0)
                   (< abs-y (board/height board))
                   (eq (board/cell* board abs-pos) :empty))
        (return-from board/valid-unit-p nil)))))
(defun board/full-row (board y)
  (dotimes (x (board/width board) t)
    (if (eq (board/cell board x y) :empty)
        (return))))
(defun board/clear-lines (board)
  "Return number of lines cleared"
  (let* ((width (board/width board))
         (height (board/height board))
         (filled-rows
          (loop for y from 0 to (1- height)
             when (board/full-row board y) 
             collect y into lines
             finally (return lines))))
    (dolist (y filled-rows)
      (dotimes (x width)
        (setf (board/cell board x y) :empty))
      (loop for y from (1- y) downto 0
         do (dolist (x width)
              (setf (board/cell board x (1+ y))
                    (board/cell board x y))
              (setf (board/cell board x y) :empty))))
    (length filled-rows)))
(defun board/integrate-unit (board unit)
  "Return number of lines cleared"
  (dolist (c (unit/cells unit))
    (let ((abs-pos (unit/abs-pos unit c)))
      (setf (board/cell* board abs-pos) :full)))
  (board/clear-lines board))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
(defclass game ()
  ((id :initarg :id)
   (board :initarg :board :accessor game/board)
   (units :initarg :units :accessor game/units)
   (source-len :initarg :source-len
               :accessor game/source-len)
   (seeds :initarg :seeds :accessor game/seeds)
   (curr-unit :initarg :curr-unit :initform nil
              :accessor game/curr-unit)
   (points :initarg :points :initform 0
           :accessor game/points)
   (prev-lines-cleared :initarg :prev-lines-cleared :initform 0
                       :accessor game/prev-lines-cleared)))
(defun game/clone (game)
  (with-slots (id board units source-len
                  seeds curr-unit points
                  prev-lines-cleared) game
    (make-instance
     'game
     :id id :board (board/clone board)
     :units units :source-len source-len
     :seeds seeds :curr-unit curr-unit
     :points points
     :prev-lines-cleared prev-lines-cleared)))
(defmethod initialize-instance
    :after ((game game) &key width height filled)
  (when (and width height)
    (setf (game/board game)
          (lret ((board (make-instance
                         'board
                         :width width :height height)))
            (dolist (f filled)
              (setf (board/cell* board f) :full))))))
(defun mand-assocdr (foo alist)
  "Mandatory assoc, returns foo's value from
   alist or signals error if no such value"
  (cdr (or (assoc foo alist)
           (error "Can't find ~A in alist" foo))))
(defun alist->game (alist)
  (labels ((raw-point->point (raw-point)
             (make-point
              (mand-assocdr :x raw-point)
              (mand-assocdr :y raw-point))))
    (make-instance
     'game :id (mand-assocdr :id alist)
     :source-len (mand-assocdr :source-length alist)
     :seeds (mand-assocdr :source-seeds alist)
     :width (mand-assocdr :width alist)
     :height (mand-assocdr :height alist)
     :filled (mapcar
              #'raw-point->point
              (mand-assocdr :filled alist))
     :units (let ((raw-units (mand-assocdr :units alist)))
              (mapcar
               (lambda (raw-unit)
                 (make-instance
                  'unit
                  :cells (mapcar
                          #'raw-point->point
                          (mand-assocdr :members raw-unit))
                  :pivot (raw-point->point
                          (mand-assocdr :pivot raw-unit))))
               raw-units)))))
(defun stream->game (stream)
  (alist->game (json:decode-json stream)))
(defun file->game (path)
  (with-input-from-file (stream path)
    (stream->game stream)))

(defun game/start (game seed-index)
  (rand/seed (nth seed-index (game/seeds game))))
(defun game/spawn-unit (game)
  (let* ((index (rand/next))
         (units (game/units game))
         (unit (nth (mod index (length units)) units))
         (unit-width (unit/width unit))
         (board-width (board/width (game/board game))))
    (setf (game/curr-unit game)
          (lret ((new-unit (unit/clone unit)))
            (setf (unit/pos new-unit)
                  (make-point
                   (floor (/ (- board-width unit-width) 2))
                   (point/y (unit/pos new-unit))))))))
;; (defclass move ()
;;   ((type :initarg :type
;;          :documentation "One of :move-w :move-e
;;                                 :move-sw :move-se
;;                                 :rotate-cw :rotate-ccw")))
(defun move-type->func (type)
  (ecase type
    (:move-w #'unit/move-w)
    (:move-e #'unit/move-e)
    (:move-sw #'unit/move-sw)
    (:move-se #'unit/move-se)
    (:rotate-cw #'unit/rotate-cw)
    (:rotate-ccww #'unit/rotate-ccw)))
;; (defun game/enum-moves (game)
;;   )
(defun game/integrate-curr-unit (game)
  (with-slots (curr-unit points board prev-lines-cleared) game
    (incf points (length (unit/cells curr-unit)))
    (let* ((lines-cleared
           (board/integrate-unit board curr-unit))
           (points
            (* 100
               (1+ lines-cleared)
               (/ lines-cleared 2)))
           (bonus
            (if (> prev-lines-cleared 1)
                (floor (* (1- prev-lines-cleared)
                          points
                          0.1))
                0)))
      (incf points (+ points bonus))
      (setf prev-lines-cleared lines-cleared)
      (setf curr-unit nil))))
(defun game/step (game)
  "Return :no-units-left-end
          :no-place-for-next-unit-end"
  (with-slots (source-len curr-unit board) game
    (when (zerop (game/source-len game))
      (return-from game/step :no-units-left-end))
    (unless curr-unit
      (game/spawn-unit game))
    (unless (board/valid-unit-p board curr-unit)
      (return-from game/step :no-place-for-next-unit-end))
    (dolist (type '(:move-w :move-e :move-se :rotate-cw :rotate-ccw))
      (let ((moved-unit
             (funcall (move-type->func type) curr-unit)))
        (assert moved-unit)
        (if (board/valid-unit-p board moved-unit)
            (progn
              (setf curr-unit moved-unit)
              (return-from game/step :todo-implement-end))
            (progn
              (game/integrate-curr-unit game)
              (game/points game)))))))
(defun game/dump-board (game)
  (board/dump (game/board game)
              :unit (game/curr-unit game)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; {p, ', !, ., 0, 3}	move W
;; {b, c, e, f, y, 2}	move E
;; {a, g, h, i, j, 4}	move SW
;; {l, m, n, o, space, 5}    	move SE
;; {d, q, r, v, z, 1}	rotate clockwise
;; {k, s, t, u, w, x}	rotate counter-clockwise
;; \t, \n, \r	(ignored)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ai/play-seed (game index)
  "INDEX should be less than number of seeds"
  )
