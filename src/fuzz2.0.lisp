;; special symbols :
;; inf -> infinite (class)
;; inf- -> infinite- (class)
;; inf+ -> infinite+ (class)



(defclass INFINITE ()
  ()
  (:documentation "Class of infinite."))

(defclass INFINITE- (INFINITE)
  ()
  (:documentation "Class of infinite."))

(defclass INFINITE+ (INFINITE)
  ()
  (:documentation "Class of infinite."))

(defmethod print-object ((self INFINITE) stream)
  (format stream "inf±")
  (values))

(defmethod print-object ((self INFINITE-) stream)
  (format stream "inf-")
  (values))

(defmethod print-object ((self INFINITE+) stream)
  (format stream "inf+")
  (values))

(defun inf ()
  (make-instance 'infinite))

(defun inf- ()
  (make-instance 'infinite-))

(defun inf+ ()
  (make-instance 'infinite+))

(defvar inf (funcall #'inf))
(defvar inf- (funcall #'inf-))
(defvar inf+ (funcall #'inf+))

#|
(inf+)
inf+
|#

(defclass FUZZSET ()
  ((name
    :initarg :name
    :reader name
    :initform ""
    :accessor name
    :type string))
  (:documentation "Class of fuzz sets."))

(defclass FUZZSET-SET (FUZZSET)
  ((support
    :initarg :support
    :reader support
    :initform '()
    :accessor support
    :type list)
   (membership
    :initarg :membership
    :reader membership
    :initform '()
    :accessor membership
    :type list))
  (:documentation "Sub-Class of discrete fuzzy sets."))

(defmethod print-object ((fuzzset fuzzset-set) stream)
  (format stream
         "~S:(" (name fuzzset))
  (dotimes (n (length (support fuzzset))
              (format stream
                      ")" (name fuzzset)))
    (format stream
            "~S/~S " (nth n (support fuzzset))
            (nth n (membership fuzzset))))
  (values))

#|
(setf a (make-instance 'fuzzset-fct
          :name (read-from-string (symbol-name  (gensym "f")))
          :support '(1 2 3)
          :membership '(0 1 0)))
|#

(defclass FUZZSET-FCT (FUZZSET)
  ((func
    :initarg :func
    :reader func
    :initform '()
    :accessor func
    :type list)
   (domain
    :initarg :domain
    :reader domain
    :initform nil
    :accessor domain
    :type list))
  (:documentation "Sub-Class of continuous fuzzy sets
defined by a list of functions ans their respective domains."))

(defmethod support ((fuzzset fuzzset-fct))
  (domain fuzzset))

(defmethod print-object ((fuzzset fuzzset-fct) stream)
  (format stream
         "~S:function" (name fuzzset))
  
  (values))

#|
(setf f (make-instance 'fuzzset-fct
          :name (read-from-string (symbol-name  (gensym "f")))
          :func (list (lambda (x) (clip (log x))))
          :domain '((0 nil))))

(describe f)
|#

(defmethod kernel ((fuzzset fuzzset-set))
  ""
  (mapcar #'(lambda (n)
              (nth n (support fuzzset)))
          (posn 1 (membership fuzzset))))


#|
(kernel a)
|#

(defmethod fuzzy-numberp ((object fuzzset-set))
  (if (= 1 (length (kernel object)))
    t nil))

(defun posn (item list)
  (let ((r '()))
    (dotimes (n (length list) (reverse r))
      (when (equalp (nth n list) item)
        (push n r)))))


(defun extract-bornes (x support)
  (loop for i from 0 to (- (length support) 2)
        until (and (<= (nth i support) x)
                   (>= (nth (1+ i) support) x))
        finally (return i)))

#|
(extract-bornes 4 '(0 1 2 3 4 5))
(extract-bornes 6 '(0 1 2 3 4 5))
(nth 8 '(1 2 3))
|#

(defun line-eq (x1 y1 x2 y2)
  "Donne les coefficients a b de la droite : y = ax + b
passant par les points (x1 y1) et (x2 y2)."
  (if (= x1 x2) (values (funcall #'inf) 0)
      (let ((a (/ (- y2 y1) (- x2 x1)))
            (b))
        (setf b (- y1 (* a x1)))
        (values a b))))


(defmethod mu ((fuzzset fuzzset-set) (x number))
   (let* ((supp (support fuzzset))
          (membersh (membership fuzzset))
          (i (extract-bornes x supp))
          )
     (cond ((< x (apply #'min supp))
            (car membersh))
           ((> x (apply #'max supp))
            (car (last membersh)))
           (t
            (multiple-value-bind (a b)
                            (line-eq (nth i supp)
                                     (nth i membersh)
                                     (nth (1+ i) supp)
                                     (nth (1+ i) membersh))
          (+ b (* a x)))))
     ))

(defmethod mu ((fuzzset fuzzset-fct) (x number))
   (let* ((supp (support fuzzset))
          (dom (pos-int-dom supp x))
          (supinf-lim (list (caar supp) (cadar (last supp))))
          )
     (cond ((not (member nil supinf-lim))  ;; domain = [a b])
            (if (and (>= x (car supinf-lim))
                (<= x (cadr supinf-lim)))
              (funcall (nth dom (func fuzzset)) x)
              0))
           ((and (null (car supinf-lim))  ;;domain = ]-inf +inf[
                 (null (cadr supinf-lim)))
            (funcall (nth dom (func fuzzset)) x))
           ((and (null (car supinf-lim))  ;;domain = ]-inf b]
                 (cadr supinf-lim))
            (if (>= x (cadr supinf-lim))
              0
              (funcall (nth dom (func fuzzset)) x)))
           ((and (car supinf-lim))        ;;domain = [a +inf[
            (null (cadr supinf-lim))
            (if (< x (car supinf-lim))
              0
              (funcall (nth dom (func fuzzset)) x)))
            (t
             (LOL-ERROR-MSG "MU function : error of domains in fuzzy set"
                            :end-text "aborted")))))


#|
(describe f)
(mu f 2)
|#


(defun clip (x)
  (cond ((> x 1)
         1)
        ((minusp x)
         0)
        (t
         x)))


#|
(defgeneric < (number &rest more-numbers)
  (:documentation
   "Tests if x < y. Accepts nil, inf, inf+ and inf- as args."))

(defmethod < ((number number) &rest more-numbers)
  (if (member nil (mapcar #'plusp (lambda (x) (- number x)) more-numbers))
    nil t))



(defmethod < ((x number) (y number))
  (if (plusp (- x y))
    t nil))
|#
    
#|
(zerop 0)
(minusp 0)
(plusp 1)
|#

(defmethod <i ((x number) (n null))
  t)

(defmethod <i ((x t) (n t))
  nil)

(defmethod <=i ((x number) (n number))
  (<= x n))

(defmethod <=i ((x number) (n null))
  t)

(defmethod <=i ((x t) (n t))
  nil)

(defmethod >i ((x number) (n number))
  (> x n))

(defmethod >i ((x number) (n null))
  t)

(defmethod >i ((x t) (n t))
  nil)

(defmethod >=i ((x number) (n number))
  (>= x n))

(defmethod >=i ((x number) (n null))
  t)

(defmethod >=i ((x t) (n t))
  nil)

(defun min-dom (list)
  (first list))

(defun max-dom (list)
  (second list))

(defun pos-int-dom (list-dom val)
  (flet ((match (x) (and (<=i val (max-dom x)) (>=i val (min-dom x)))))
    (position-if #'match list-dom)))

     
     
#|
(mu a -2)
(mu a 2.5)
(mu a 8)

(setf b (make-instance 'fuzzset-set
          :name (read-from-string (symbol-name  (gensym "f")))
          :support '(1 2 3 4 5)
          :membership '(0 1 0.8 0.2 0)))
b
(fuzzy-numberp b)
(mu b -1)
(mu b 2.1)
(mu b 8)
|#

#|
(defmethod print-object ((self DOUBLE-FLOAT) stream)
  (format stream "~,6F" self)
  (values))
|#
