(defun +! (a b)
  (append a b))

#|
(+! '(1) '(1 1))
(+! '(a) '(b c))
|#


;;; ADDITION AVEC ERREUR PROBABILISTE

(defun random-pond (p)
  (if (= 0 p)
    nil
    (if (> (random 1.0) p)
      nil t)))

#|
(let ((r '()))
  (dotimes (n 1000 (float (/ (count 't r) 1000)))
    (push (random-pond 0.1) r)))
|#

#|
(defun append! (A B prob)
  "append probabiliste"
  (dotimes (n (length A) B)
    (let ((val (pop A)))
    (if (random-pond prob) 
      (setf B (push val B))    ;; si t pas d'erreur
      (when (random-pond 0.5)  ;; sinon erreur (en trop ou en moins)
        (setf B (append (list val val) B)))))))
|#

(defun append! (A B prob)
  "append probabiliste recursif"
  (if (null A)
      B
      (let ((val (pop A)))
        (append!
         A
         (if (random-pond prob) 
           (push val B)          ;; si t pas d'erreur
           (if (random-pond 0.5)  ;; sinon erreur (en trop ou en moins)
             (append (list val val) B)
             B))
         prob))))


#|
(append! '(a b c d e) '(1 2 3 4 5 6) 1.0)
|#

#|
(defmethod f+ ((A list) (B list) (p float))
  "somme avec erreurs"
  (append! A B p))

(defmethod f+ ((A integer) (B integer) (p float))
  (let ((x (make-list A :initial-element 't))
        (y (make-list B :initial-element 't)))
    (length (append! X Y p))))
|#


#|
(f+ '(a a a) '(b) 0.6)
(let ((r '()))
  (dotimes (n 1000 (sort r '<))
    (push (f+ 5 3 0.99) r)))
|#

(defun clip (x)
  (cond ((minusp x)
         0)
        ((> x 1.0)
         1)
        (t x)))


(defmethod f+ ((A integer) (B integer) &optional (base 10000))
"somme dont l'erreur est proportionnelle au plus petit nombre d'elements ajoutes"
  (let ((x (make-list A :initial-element 't))
        (y (make-list B :initial-element 't)))
    (length (append! X Y (/ 1 (expt (float (1+ (/ 1 base))) (min A B)))))))

(defmethod f+ ((A list) (B list) &optional (base 10000))
"somme dont l'erreur est proportionnelle au plus petit nombre d'elements ajoutes"
    (append! A B (/ 1 (expt (float (1+ (/ 1 base))) (min (length A) (length B))))))

#|
(f+ '(a b c d e) '(1 2 3 4 5 6))
|#

(defun test-f+ (x y &optional (base 1000))
  (let ((r '()))
    (dotimes (n 1000 (and (print (sort r '<)) (float (/ (count (+ x y) r :test #'eq) 1000))))
      (push (f+ x y base) r))))

(defun analyse-f+ (x y &optional (base 1000))
  (let ((r '()))
    (dotimes (n 1000)
      (push (f+ x y base) r))
    (mapcar #'(lambda (x)
                (list (count x r :test #'equal) x))
            (sort (remove-duplicates r :test #'equal) '<))))

#|
(test-f+ 10 10 5)
(analyse-f+ 10 10 5)
(analyse-f+ 10 10 10000)
(analyse-f+ 5 15 10000)
(analyse-f+ 1 19 10000)
(f+ 10 10000 100000)
(time (f+ 1 1))
(time (+ 1 1))
(time (f+ 10 10))
(time (+ 10 10))
(time (f+ 100 100))
(time (+ 100 100))
(time (f+ 1000 1000))
(time (+ 1000 1000))
(time (f+ 10000 10000))
(time (+ 10000 10000))
(time (f+ 100000 100000))
(time (+ 100000 100000))
|#



;; ADDITION AVEC CRITERE d'EGALITE





(defun nombre (A)
  (mapcar #'(lambda (x)
              (list (count x A) x))
          (remove-duplicates A :from-end t)))

#|
(nombre '(a a a))
(nombre '(a a b a))
(nombre (+! '(a b c) '(a)))
|#