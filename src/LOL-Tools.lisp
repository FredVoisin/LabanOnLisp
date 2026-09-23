(in-package :lol)



(defun split-list-modulo (list mod)
  (mapcar #'(lambda (m) (subseq list m (+ m mod)))
          (+serie 0 mod (floor (/ (length list) mod)))))

#|
(split-list-modulo '(1 2 3 4 5 6 7 8 9) 3)
|#

(defun +serie (start step n)
  (let ((r '()))
    (dotimes (i n (reverse r))
      (push (+ start (* i step)) r))))

#|
(+serie 0 3 3)
|#

(defun fflat (liste)
  (cond
   ((null liste) nil)
   ((atom liste) (list liste))
   (t (append (fflat (car liste))
              (fflat (cdr liste))))))


(defmethod mat-trans ((matrix list))
"The matrix is represented by a list of rows. Each row is a list of items.
Rows and columns are interchanged. from OM/PW without remove nil."
  (let ((maxl (1- (loop for elt in matrix maximize (length elt))))
        result)
    (loop for i from 0 to maxl do
         (push (mapcar #'(lambda (list) (nth i list)) matrix) result))
    (nreverse result)))



(defun characters (string)
  (let ((c '()))
    (dotimes (n (length string) (reverse c))
      (push (elt string n) c))))


(defun seq-member (s1 s2)
  (let ((a nil) (l1 (length s1)) (l2 (length s2)))
    (loop for s from 0 to (1- l2)
          until (setf a (equalp s1
                                (subseq s2 s
                                        (funcall #'min l2 (+ s l1)))))
          finally (return a))))

;(seq-member '(1 2 3) '(4 3 3 2 3 5 6 7 1 2 3))



(defmethod reorder0 ((list list) (old-position integer) (new-position integer))
  (assert (and (< old-position (length list))
               (< new-position (length list))))
  (if (= old-position new-position)
    list
    (let ((e (nth old-position list))
          (nlist (append (subseq list 0 old-position)
                         (subseq list (1+ old-position)))))
      (append (subseq nlist 0 new-position)
              (list e)
              (subseq nlist new-position)))))

(defmethod reorder0 ((list list) (old-position list) (new-position list))
  (assert (= (length old-position) (length new-position)))
  (let ((nlist (make-list (length list))))
    (dotimes (n (length list))
      (setf (nth (nth n new-position) nlist)
            (nth (nth n old-position) list)))
    nlist))


;(reorder0 '(1 2 3 4 5 6) 0 1)
;(reorder0 '(1 2 3 4 5 6) '(0 1 2 3 4 5) '(5 0 2 4 3 1))
;(reorder0 '(a b c d e f) '(0 1 2 3 4 5) '(5 0 2 4 3 1))


(defun reorder (list order)
  (let ((serie (loop for n from 0 to (1- (length list))
                     collect n)))
    (reorder0 list serie order)))

(reorder '(a b c d e f) '(5 0 2 4 3 1))


(defun order (list &key key)
  (let ((ordered (sort (copy-tree list) '< :key key))
        )
    (loop for l in list
         collect (position l ordered))))


;(order '(2746 4875 4968504 45696 4566894767 465358 496583 3958 35985 9))



(defmethod get-position ((in situation) (elt bodypart))
  (position elt (body in) :test #'equalp))

(defmethod get-position ((in bodypart) (elt dimension))
  (position elt in :test #'equalp))


(defmethod get-position ((in situation) (elt dimension))
  (position elt in :test #'equalp))

(defmethod get-position ((in situation) (elt list))
  (mapcar #'(lambda (x) (position x in :test #'equalp)) elt))





