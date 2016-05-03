(defpackage :icfpc-2015
  (:use :cl
        :alexandria
        :anaphora
        :serapeum
        :cl-annot
        :annot.std
        :cl-fad)
  (:shadowing-import-from
   :alexandria
   :copy-stream
   :copy-file))
(in-package :icfpc-2015)
(cl-syntax:use-syntax :annot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *verbose* nil)
(defun message (item)
  (when *verbose*
    (print item)))
(defun mand-assocdr (foo alist)
  "Mandatory assoc, returns foo's value from
   alist or signals error if no such value"
  (cdr (or (assoc foo alist)
           (error "Can't find ~A in alist" foo))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
(defun rand/make (seed)
  "The implementation is inspired by rosetta code, 
   the constants are from the ICFPC task IIRC."
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
    (1+ (- max-x min-x))))
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
(defun unit/move (unit dx dy)
  (lret ((result (unit/clone unit)))
    (setf (unit/pos result)
          (point+ (unit/pos result) (make-point dx dy)))))
(defun unit/move-w (unit)
  (unit/move unit -1 0))
(defun unit/move-e (unit)
  (unit/move unit 1 0))
(defun unit/move-sw (unit)
  (let ((y (point/y (unit/pos unit))))
    (unit/move unit (if (evenp y) -1 0) 1)))
(defun unit/move-se (unit)
  (let ((y (point/y (unit/pos unit))))
    (unit/move unit (if (evenp y) 0 1) 1)))
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
      (let ((unit-cell-is-here
             (when unit
               (dolist (c (unit/cells unit))
                 (when (point=
                        (unit/abs-pos unit c)
                        (make-point x y))
                   (return t))))))
        (if unit-cell-is-here
            (if (eq (board/cell board x y) :full)
                (format t (if (evenp y) "X " " X"))
                (format t (if (evenp y) "@ " " @")))
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
         do (dotimes (x width)
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
  ((id :initarg :id :reader game/id)
   (board :initarg :board :accessor game/board)
   (units :initarg :units :accessor game/units)
   (source-len :initarg :source-len
               :accessor game/source-len)
   (seeds :initarg :seeds :accessor game/seeds)
   (curr-seed :initarg :curr-seed :initform nil
              :accessor game/curr-seed
              :documentation "Contains seed which in use
                              for current simulation")
   (curr-chain :initarg :curr-chain :initform nil
               :accessor game/curr-chain
               :documentation "Contains chain of moves for
                               current simulation")
   (curr-unit :initarg :curr-unit :initform nil
              :accessor game/curr-unit)
   (curr-unit-visited-points
    :initarg :curr-unit-visited-points
    :initform nil
    :accessor game/curr-unit-visited-points
    :documentation "List of points which were visited 
                    by curr-unit. It's an error to visit
                    same point twice")
   (points :initarg :points :initform 0
           :accessor game/points)

   (prev-lines-cleared :initarg :prev-lines-cleared :initform 0
                       :accessor game/prev-lines-cleared)))
(defun game/clone (game)
  (with-slots (id board units source-len
                  seeds curr-seed curr-chain
                  curr-unit
                  curr-unit-visited-points
                  points
                  prev-lines-cleared) game
    (make-instance
     'game
     :id id :board (board/clone board)
     :units units :source-len source-len
     :seeds seeds :curr-seed curr-seed
     :curr-chain curr-chain
     :curr-unit curr-unit
     :curr-unit-visited-points curr-unit-visited-points
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
(defun game/dump-board (game)
  (board/dump (game/board game)
              :unit (game/curr-unit game)))
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
  (let ((seeds (game/seeds game)))
    (assert (< seed-index (length seeds)))
    (let ((seed (nth seed-index seeds)))
      (setf (game/curr-seed game) seed)
      (rand/seed seed))))
(defun game/spawn-unit (game)
  (let* ((index (rand/next))
         (units (game/units game))
         (unit (nth (mod index (length units)) units))
         (unit-width (unit/width unit))
         (board-width (board/width (game/board game))))
    (decf (game/source-len game))
    (when (game/source-len game)
      (setf (game/curr-unit game)
            (lret ((new-unit (unit/clone unit)))
              (setf (unit/pos new-unit)
                    (make-point
                     (floor (/ (- board-width unit-width) 2))
                     (point/y (unit/pos new-unit))))))
      t)))
(defun game/valid-unit-p (game unit)
  (and 
       (not )))
(defun game/move-curr-unit (game type)
  "Return :good-move or :bad-move"
  (with-slots (board
               curr-unit
               curr-unit-visited-points) game
    (let ((moved-unit
           (funcall (move-type->func type) curr-unit)))
      (cond
        ((member (unit/pos moved-unit)
                 (game/curr-unit-visited-points game)
                 :test #'point=)
         :bad-move)
        ((board/valid-unit-p board moved-unit)
         (setf curr-unit moved-unit)
         (append curr-unit-visited-points
                 (list (unit/pos moved-unit)))
         :good-move)
        (t (game/integrate-curr-unit game)
           :good-move)))))
(defun move-type->func (type)
  (ecase type
    (:move-w #'unit/move-w)
    (:move-e #'unit/move-e)
    (:move-sw #'unit/move-sw)
    (:move-se #'unit/move-se)
    (:rotate-cw #'unit/rotate-cw)
    (:rotate-ccw #'unit/rotate-ccw)))
(defun game/integrate-curr-unit (game)
  (with-slots (curr-unit points board prev-lines-cleared) game
    (when curr-unit
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
        (setf curr-unit nil)))))
(defparameter *max-depth* 100)
(defun game/step-recurs (game &key (depth 0) allow-hor-movement)
  "Return :no-units-left
          :no-place-for-next-unit
          :max-depth-reached"
  (with-slots (source-len curr-unit board) game
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (unless curr-unit
      (unless (game/spawn-unit game)
        (return-from game/step-recurs :no-units-left)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; (game/dump-board game)
    ;; (terpri)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (unless (board/valid-unit-p board curr-unit)
      (return-from game/step-recurs :no-place-for-next-unit))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (let ((type
           (random-elt (if allow-hor-movement
                           '(:move-w :move-e
                             :move-se :move-sw
                             ;;:rotate-cw :rotate-ccw
                             )
                           '(:move-se :move-sw)))))
      (ecase (game/move-curr-unit game type)
        (:good-move
         (if (>= depth *max-depth*)
             :max-depth-reached
             (progn
               (setf (game/curr-chain game)
                     (append (game/curr-chain game) (list type)))
               (game/step-recurs game :depth (1+ depth)))))
        (:bad-move :bad-move-encountered)))))
(defun game/play (game seed-index
                  &key max-depth allow-hor-movement)
  (let ((*max-depth* max-depth))
    (game/start game seed-index)
    (game/step-recurs
     game :allow-hor-movement allow-hor-movement)))
(defun game/play-few-times (game seed-index &key (times 150))
  "Game will not be modified"
  (let ((max-points most-negative-fixnum)
        (best-chain nil)
        (best-game nil))
    (labels ((one-pass (&key allow-hor-movement)
               (dotimes (x times)
                 (let ((game (game/clone game)))
                   (game/play game seed-index :max-depth 200
                              :allow-hor-movement allow-hor-movement)
                   (let ((points (game/points game)))
                     (when (> points max-points)
                       (setf max-points (game/points game))
                       (setf best-chain (game/curr-chain game))
                       (setf best-game game)))))))
      (one-pass :allow-hor-movement nil)
      (one-pass :allow-hor-movement t)
      best-game)))
(defun game/play-problems-dir (dir-path action)
  (let ((games nil))
    (dolist (path (list-directory dir-path))
      (push (file->game path) games))
    (nreversef games)
    (let ((decomposed-games nil))
      (dolist (game games)
        (dotimes (seed-index (length (game/seeds game)))
          (push (game/play-few-times game seed-index)
                decomposed-games)))
      (nreversef decomposed-games)
      (values (funcall action decomposed-games)
              decomposed-games))))
(defun game/play-problems-dir-and-send (dir-path)
  (game/play-problems-dir
   dir-path (lambda (games)
              (game/send-results
               (games->results-json games)))))
(defun game/play-problems-dir-and-show-points (dir-path)
  (game/play-problems-dir
   dir-path (lambda (games)
              (mapcar #'game/points games))))
;; (defun game/play-few-times-and-send-results (game seed-index)
;;   (let* ((game (game/clone *game*)))
;;     (multiple-value-bind (pts chain game)
;;         (game/play-few-times game 0 :times 15000)
;;       (let ((json (game->results-json game chain)))
;;         ;(game/send-results json)
;;         ))))
(defun move-type->code (type)
  (ecase type
    (:move-w #\p)
    (:move-e #\b)
    (:move-sw #\a)
    (:move-se #\l)
    (:rotate-cw #\d)
    (:rotate-ccw #\k)))
(defun chain->code-string (chain)
  (let ((string
         (map 'string #'move-type->code chain)))
    (awhen (search "pab" string)
      (setf string (replace string "Ei!" :start1 it)))
    string))
(defvar *default-tag* "default-tag")
(defun games->results-alist (games
                             &key (tag *default-tag*))
  @ignore tag
  `#(,@(mapcar
        (lambda (game)
          `(("problemId" . ,(game/id game))
            ("seed" . ,(game/curr-seed game))
                                        ;("tag" . ,tag)
            ("solution" . ,(chain->code-string
                            (game/curr-chain game)))))
        games)))
;(defun game/encode-results-to-json (games))
(defun games->results-json (games &key (tag *default-tag*))
  (json:encode-json-to-string
   (game->results-alist games :tag tag)))
(defvar *team-id* 0)
(defvar *api-key* "no-key-specified")
(defun game/send-results (json
                          &key (tag *default-tag*)
                            (team-id *team-id*)
                            (api-key *api-key*))
  (assert team-id)
  (drakma:http-request
   (format nil "https://davar.icfpcontest.org/teams/~A/solutions"
           team-id)
   :method :post
   :content-type "application/json"
   :content json
   :basic-authorization (list "" api-key)))
(defun game/send-results2 (game chain
                          &key (tag *default-tag*)
                            (team-id *team-id*)
                            (api-key *api-key*))
  (game/send-results (game->results-json game chain :tag tag)
                     :tag tag :team-id team-id :api-key api-key))

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
