;;;;============================================================================
;;;;               GESTES.lisp
;;;;
;;;; author: Frederic Voisin
;;;; date: 23/04/1999 
;;;;============================================================================ 



;;;;                        Frédéric Voisin, 1999



(in-package :om)

;;************ maths *************

(defun fact1 (n var)
  (if (= var 1)
    n
    (fact1 (* n (- var 1)) (- var 1))))

(defun fact (x)
  "f(x) = x!"
  (fact1 x x))

(defun permut (list)
  "Returns all permutations of elements in list."
  (if (null (rest list))
    (list list)
    (mapcan #'(lambda (perm)
                (insert (first list) perm))
            (permut (rest list)))))

(defun insert (elt list)
  (let ((length (length list)))
  (loop for i from 0 to  length
        collect (append (subseq list 0 i) 
                        (list elt)
                        (subseq list i length)))))

(defun cartesian2 (A B)
  "Returns the cartesian products of A and B."
  (loop for e1 in A
        append (loop for e2 in B
                 collect (list e1 e2))))

(defun cartesian (A &rest rest)
  "Generalized cartesian product."
  (mapcar 'flat (reduce 'cartesian2 (cons A rest))))

#|
(cartesian '(0 1 2 3) '(a b c) '(10 20 30 40))
(combine-transpo '(60 70 80) 2)
|#


(defun dx (list)
  "Delta values of list."
  (let ((r '()))
    (dotimes (i (1- (length list)) (reverse r))
      (push (- (nth (1+ i) list)
               (nth i list))
            r))))

(defun dx2 (list)
  "Delta values of list."
  (let ((r '()))
    (dotimes (i (length list) (sort (remove 0 r) '<))
      (dotimes (j (- (length list) i))
        (push (- (nth (+ j i) list) (nth i list)) r)))))

(defun serie-lineaire (start stop n)
  "Returns a linear serie from start to end with n samples."
  (let ((r '()))
    (dotimes (i n (reverse r))
      (push (+ start (* i (/ (- stop start ) (1- n)))) r))))


(defun echant (list n)
  "Does a resampling of list by n samples."
  (let ((l (length list))
        (r '()))
      (if (>= n l) 
        (dotimes (i n (reverse r))
          (push (nth (floor (* i (/ l n))) list) r))
        (let ((newn (- n 2)))
          (dotimes (i newn (append (list (car list)) (reverse r) (last list)))
            (push (nth (round (* (1+ i) (/ (- l 2) (if (evenp newn)
                                                     (1+ newn)
                                                     (if (= 1 newn)
                                                       2 newn))))) list) r))))))

#|
(length (echant '(1 2 3 4 5 6 7 8 9 10 11 12) 8))
|#


(defmethod scale-Xs ((minout number) (maxout number) (inX list))
  (let* ((min (apply #'min inx))
         (max (apply #'max inx))
         (delta-ratio (/ (- maxout minout) (- max min)))
         (deltaxs (dx inX))
         (dxout (mapcar #'(lambda (x) (* delta-ratio x)) deltaxs))
         (r (list minout)))
    (dolist (l dxout (reverse r))
      (push (float (+ (car r) l)) r))
    ))

(defmethod scale-Xs ((minout number) (maxout number) (inX om::bpf))
  (let* ((xs (x-points inX))
         (ys (y-points inX))
         (min (apply #'min xs))
         (max (apply #'max xs))
         (delta-ratio (/ (- maxout minout) (- max min)))
         (deltaxs (dx xs))
         (dxout (mapcar #'(lambda (x) (* delta-ratio x)) deltaxs))
         (r (list minout)))
    (dolist (l dxout (setf r (reverse r)))
      (push (float (+ (car r) l)) r))
    (make-one-instance 'om::bpf  (mapcar #'round r) ys)))

(defmethod scale-Ys ((minout number) (maxout number) (inY list))
  (let* ((min (apply #'min inY))
         (max (apply #'max inY))
         (delta-ratio (/ (- maxout minout) (- max min)))
         (deltaxs (dx inY))
         (dxout (mapcar #'(lambda (x) (* delta-ratio x)) deltaxs))
         (r (list minout)))
    (dolist (l dxout (reverse r))
      (push (float (+ (car r) l)) r))
    ))

(defmethod scale-Ys ((minout number) (maxout number) (inY om::bpf))
  (let* ((xs (x-points inY))
         (ys (y-points inY))
         (min (apply #'min ys))
         (max (apply #'max ys))
         (delta-ratio (/ (- maxout minout) (- max min)))
         (deltaxs (dx ys))
         (dxout (mapcar #'(lambda (x) (* delta-ratio x)) deltaxs))
         (r (list minout)))
    (dolist (l dxout (setf r (reverse r)))
      (push (float (+ (car r) l)) r))
    (make-one-instance 'om::bpf  xs (mapcar #'round r))))

#|
(scale-xs -1 6 '(1 2 3))
|#


(defun droite (x1 y1 x2 y2)
  "Donne les coefficients a b de la droite : y = ax + b
passant par les points (x1 y1) et (x2 y2)."
  (if (= x1 x2) (values nil 0)
      (let ((a (/ (- y2 y1) (- x2 x1)))
            (b))
        (setf b (- y1 (* a x1)))
        (values a b))))

#|
(droite 5 7 9 -12)
|#


(defmethod interpo-lineaire ((x number) &key xs ys)
   "Return the linear interpolation of value x according to x series Xs and y series Ys."
  (cond ((and xs ys)
         (if (member x xs :test #'eq)
           (nth (position x xs :test #'eq) ys)
           (let* ((xa (position-if #'(lambda (a) (<= a x)) xs :from-end 't))
                  (xb (position-if #'(lambda (a) (>= a x)) xs))
                  (ya (nth xa ys))
                  (yb (nth xb ys)))
             (multiple-value-bind (a b) (droite (nth xa xs) ya
                                                (nth xb xs) yb)
               (float (+ b (* a x)))))))
        (t (print-error "obscure interpo-lineaire error 1, it's my fault !"))))

#|
(interpo-lineaire 11 :xs '(0 2 4 6 8 10 12 14 20) :ys (reverse '(0 2 4 6 8 10 12 14 20)))
|#

(defmethod extrapo-lineaire ((x number) &key xs ys)
   "Return the extrapolation of value x according to a 2 points segment defined by Xs and Ys."
  (cond ((and xs ys)
         (multiple-value-bind (a b) (droite (car xs) (car ys)
                                            (cadr xs) (cadr ys))
           (float (+ b (* a x)))))
        (t (print-error "extrapo-lineaire error 1"))))

#|
(extrapo-lineaire 20 :xs '(0 2 ) :ys '(0 1))
|#

(defmethod intextrapo ((x number) (xs list) &optional ys)
   "Return the interpolation or extrapolation of value x (or list of values) according to
the om::bpf (break-ponit-function) or to the series Xs and Ys (lists of values)."
  (assert (listp ys))
  (when (not (= (length xs) (length ys)))
    (print-error "You didn't get it : Xs and Ys have not the same length in intextrapo !"))
  (cond ((and (<= x (apply #'max xs))
              (>= x (apply #'min xs)))
         (interpo-lineaire x 
                           :xs xs
                           :ys ys))
        ((> x (apply #'max xs))
         (extrapo-lineaire x
                           :xs (subseq xs (- (length xs) 2) (length xs))
                           :ys (subseq ys (- (length ys) 2) (length ys))))
        ((< x (apply #'min xs))
         (extrapo-lineaire x
                           :xs (subseq xs 0 2)
                           :ys (subseq ys 0 2)))
        (t (print-error "intextrapo error 1"))))

(defmethod intextrapo ((x number) (xs om::bpf) &optional ys)
  (when ys (format t "intextrapo : bpf as 2nd arg, ys ignored.~%"))
  (let ((newxs (x-points xs))
        (newys (y-points xs)))
    (intextrapo x newxs newys)))

(defmethod intextrapo ((x list) (xs om::bpf) &optional ys)
  (when ys (format t "intextrapo : bpf as 2nd arg, ignoring ys arg.~%"))
  (let ((newxs (x-points xs))
        (newys (y-points xs)))
    (mapcar #'(lambda (a) (intextrapo a newxs newys)) x)))

(defmethod intextrapo ((x list) (xs list) &optional ys)
   (mapcar #'(lambda (a) (intextrapo a xs ys)) x))

#|
(intextrapo 2 (make-instance 'bpf :x-points '(0 10) :y-points '(0 100)))
(intextrapo '(-1 12) (make-instance 'bpf))
(intextrapo 2 '(0 2 4 6 8 10 12 14 20) '(0 8 4 6 8 10 12 14 25))
|#




;;************ utilities **********

(defun fflat (liste)
  (cond
   ((null liste) nil)
   ((atom liste) (list liste))
   (t (append (fflat (car liste))
              (fflat (cdr liste))))))

(defun ldlp (l)
  "Test if every element of l is a list of lists."
  (not (member nil (mapcar #'listp l))))

#|
(ldlp '(() () 8))
|#

(defun min-dom (list)
  (first list))

(defun max-dom (list)
  (second list))

(defun pos-int-dom (list-dom val)
  "Returns the domain number if val match a domain in list-dom."
  (flet ((match (x) (and (<= val (max-dom x)) (>= val (min-dom x)))))
    (position-if #'match list-dom)))

#|
;exemple :
(pos-int-dom '((0 2) (5 10) (12 24)) 1.1)
|#

(defun print-error (string)
  "error msg with MCL dialog box"
  (message-dialog (format nil "Error : ~S" string)
                  :size #@(400 150))
  (abort))

(defun foo ()
  nil)

(defmethod numtostring ((num number))
  "Converts a number into a string."
  (format nil "~S" num))

(defmethod numtostring ((num list))
  "Converts a number into a string."
  (mapcar #'numtostring num))

#|
(numtostring '(1 2 2 33.335))
|#

(defun mapnth (elts list)
  "Returns the each # of elts in list."
  (mapcar #'(lambda (n) (nth n list)) elts))

;;**************** Time features ************************

(defvar *tempo* '((0 60)))

(defun change-tempo (offset tempo)
  "Changes tempo in maquette. For temporal object tempo."
  (push (list offset tempo) *tempo*)
  (sort (remove-duplicates *tempo* :test #'equal) '< :key #'car))

(defun find-good-tempo (temp-obj-offset)
  "Looks for the last previous tempo change in om::maquette."
  (let ((good-tempo))
    (loop for e in *tempo*
          while (>= temp-obj-offset (car e))
          do
          (setf good-tempo (car e)))
    good-tempo))


(defmethod at-tempo ((offset number) (time-val number))
  (let ((tempo (find-good-tempo offset)))
    (* time-val (/ 60 tempo))))

(defmethod at-tempo ((offset number) (time-val list))
  (mapcar #'(lambda (x) (at-tempo offset x)) time-val))

(defun to-tempo (value tempo)
  (when (null tempo) (setf tempo 60))
  (round (* value (/ 60 tempo))))

(defmethod find-value ((x number) (values list) (tempi list))
  (let ((set (mapcar #'(lambda (x)
                         (mapcar #'(lambda (y)
                                     (to-tempo y x)) values)) tempi))
        ratios
        r
        (temp 100000))
    (setf ratios (mapcar #'(lambda (a) (special-ratio x a)) set))
    (dotimes (n (length ratios))
      (let ((min (apply #'min (nth n ratios))))
        (when (< min temp) (setf temp min))))
    (dotimes (n (length ratios) (reverse r))
      (when (member temp (nth n ratios))
        (push (list (nth (position temp (nth n ratios)) values)
                    (nth n tempi)) r)))
    ))

(defun find-tempovalues (x values tempi)
  "Cherche les valeurs et tempi les plus proches de x exprime en ms."
  (find-value x values tempi))

#|
(find-value 1000 '(2000 1000 500 250 125) '(60 120 80 72 180))
|#

(defmethod find-value2 ((x number) (values list) (tempi list) (mult list))
  (let ((set (mapcar #'(lambda (x)
                         (mapcar #'(lambda (y)
                                     (to-tempo y x)) values)) tempi))
        ratios
        r)
    (setf set (mapcar #'(lambda (a)
                          (mapcar #'(lambda (b)
                                      (mapcar #'(lambda (c)
                                                  (* a c)) b)) set)) mult)
          ratios (mapcar #'(lambda (a)
                             (mapcar #'(lambda (b)
                                         (special-ratio x b)) a))
                         set))
    (let ((min (apply #'min (fflat ratios))))
      (dotimes (n (length ratios) (reverse r))
        (dotimes (o (length (nth n ratios)))
          (when (member min (nth o (nth n ratios)))
            ;(print (list n o))
            (push (list (nth (position min (nth o (nth n ratios))) values)
                        (nth o tempi)
                        (nth n mult)) r)))))))


#|
(find-value2 666 '(1000 500 333 250 125) '(60 120 80 72 180) '(1 2 3))
|#


(defun special-ratio (val set)
  (mapcar #'(lambda (x) (abs (1- (/ val x)))) set))


#|
(special-ratio 2 '(1 2 3 4 5 6))
|#


(defun contrainte-1 (list_pitches champs)
  (if (listp champs)
    list_pitches
    foo))

(defun permut-acc (chord &rest lim)
  (setf chord (sort chord '<))
  (let ((intervalles '()))
    (dotimes (n (- (length chord) 1))
      (let ((i (abs (- (nth (1+ n) chord)
                       (nth n chord)))))
        (when (pos-int-dom (list lim) i)
          (push i intervalles))))
    (setf intervalles (permut (reverse intervalles)))
    (remove-duplicates
     (mapcar #'(lambda (int) (root-ints (car chord) int)) intervalles)
     :test #'equalp)))

(defun find-ints (chord opt-int)
  (setf chord (sort chord '<))
  (let ((intervalles '()))
    (if (= 0 opt-int)
      (dotimes (n (- (length chord) 1) (reverse intervalles))
        (push (abs (- (nth (1+ n) chord)
                      (nth n chord))) intervalles))
      (setf intervalles (dx2 chord)))))

#|
(permut-acc '(60 62 65 69) 48 72)
|#
    
(defun root-ints (root ints)
  "Returns a chord with its the root note and notes
according to intervals from root."
  (setf root (list root))
  (dotimes (n (length ints) (reverse root))
    (setf root (push (+ (car root) (nth n ints)) root))))

#|
(root-ints 6000 '(0 100 200 300))
|#


;;********** filters *************


(defun filtre (set1 set2)
  (dolist (s2 set2 set1)
    (remove-if #'(lambda (x) (equalp x s2))
               set1)))


(defun filtre (set1 set2)
  (dolist (s2 set2 set1)
    (setf set1 (remove s2 set1))))


#|
(filtre '(1 2 3 0 2 3 5 8 9 4 1 0 1 0 1 0 2 3) '(1 2 3 0))
|#

(defun filtre1 (accord intervals)
  (let ((ints (dx2 accord))
        (r '()))
    (setf r (filtre ints intervals))))

(defun filtre-accord (accord intervals)
  (setf accord (mapcar 'round accord)
        intervals (mapcar 'round intervals))
  (let ((ints (mapcar 'abs (dx2 accord))))
    (if (member 'nil (mapcar #'(lambda (x) (not (member x intervals))) ints) :test #'eq)
      nil accord)))

(defun filtre-accord2 (accord intervals approx)
  (setf accord (mapcar 'round accord)
        intervals (mapcar #'(lambda (x) (make-rg x approx)) intervals))
  (let ((ints (mapcar 'abs (dx2 accord))))
    (if (not (remove 'nil (mapcar #'(lambda (a) (pos-int-dom intervals a)) ints)))
       accord 'nil)))


(defun make-rg (x 1/2range)
  (list (round (- x 1/2range)) (round (+ x 1/2range))))


#|
(dx2 '(60 62 68 69))
|#

(om::defmethod* filt-acc2 ((chord list) (set list) &optional approx)
    :menuins ( (2 (("no" 0)
                   ("1/2" 50)
                   ("1/4" 25)
                   ("1/8" 12.5)
                   ("1/10" 10)
                   ("1/16" 6.25))))
  :initvals '((6000 6700) (0 400) 0)
  :icon 127
  :doc "Passes the chord if it does not contains any intervals in set."
  (if (not (member 't (mapcar 'listp chord)))
    (filtre-accord2 (sort chord '<) set approx)
    (remove 'nil (mapcar #'(lambda (x) (filtre-accord2 (sort x '<) set approx)) chord))))

(om::defmethod* filt-acc2 ((chord list) (set number) &optional approx)
  :menuins ( (2 (("no" 0)
                 ("1/2" 50)
                 ("1/4" 25)
                 ("1/8" 12.5)
                 ("1/10" 10)
                 ("1/16" 6.25))))
  :initvals '((6000 6700) (0 400) 0)
  :icon 127
  :doc "Passes the chord if it does not contains any intervals in set."
  (setf set (list set))
  (filt-acc2 chord set approx))

(defun combine-transpo (accord transp)
  (let ((temp (mapcar #'(lambda (note)
                          (list note
                                (+ note transp)
                                (- note transp)))
                      accord)))
    (remove-duplicates (apply 'cartesian temp) :test #'equalp)))


;;;;; ******************    MIDI features


(defmethod splitmidi ((chord number))
  "Returns the midi channels according to the height or quarter-tone
tuning of a synthesizer (1 = whole-tone, 3 = 1/4 higher."
  (let ((p (- (/ chord 100) (floor (/ chord 100)))))
    (if (or (<= p .25)
            (>= p .75))
      1 3)))

(defmethod splitmidi ((chord list))
  "Returns the midi channels according to the height or quarter-tone
tuning of a synthesizer (1 = whole-tone, 3 = 1/4 higher."
  (mapcar #'splitmidi chord))

(defmethod splitmidi2 ((chord number))
  "Splits the notes of chord into two lists (whole-tone & quarter-tone)."
  (let ((p (- (/ chord 100) (floor (/ chord 100))))
        (l1 '()) (l2 '()))
    (if (or (<= p .25)
            (>= p .75))
      (push (round chord) l1) (push (round chord) l2))
    (list (reverse l1) (reverse l2))))

(defmethod splitmidi2 ((chord list))
  (mapcar #'splitmidi2 chord))

(defmethod quartp ((chord number))
  "Tests if each note in chord is a quarter-tone of the nearest tempered pitch."
  (let ((p (- (/ chord 100) (floor (/ chord 100)))))
    (if (or (<= p .25)
            (>= p .75))
      nil t)))

(defmethod quartp ((chord list))
  "Tests if each note in chord is a quarter-tone of the nearest tempered pitch."
  (mapcar #'quartp chord))

#|
(splitmidi2 '(6000 6050 7200 7250))
|#

#|
(quartp 6910)
|#

(defun filtre-1/4 (chord percent)
  (let ((notes (mapcar #'1/4? chord))
        (n 0))
    (dolist (l notes)
      (when l (setf n (1+ n))))
    (when (< (* 100.0 (/ n (length chord))) percent)
      chord)))

#|
(filtre1/4 '((6900 6950 7200 6050) (6900 6950 7200 6050) (6900 6800 7200 6050)) 50)
|#

(defmethod filtre1/4 ((chord list) (percent number))
  (cond ((not (member 't (mapcar #'listp chord)))
         (remove 'nil (filtre-1/4 chord percent)))
        (t
         (remove 'nil (mapcar #'(lambda (n) (filtre1/4 n percent)) chord)))))

(defmethod filtre1/4 ((chord list) (percent list))
  (remove 'nil  (mapcar #'(lambda (n p) (filtre1/4 n p)) chord percent)))

#|
(filtre1/4 '((6900 6950 7200 6050) (6900 6950 7200 6050) (6900 6800 7200 6050))
           '(100 51 20))
|#


(defun dist-seuil (distances seuil)
  (remove-if #'(lambda (x) (< (car (last x)) seuil)) distances))

(defmethod rem-parfait ((chord list))
  "Returns chord if the first three lowest or highest notes do not a perfect chord."
  (let ((length (length chord)))
    (setf chord (sort chord '<))
    (when (not (or (parfait? (subseq chord 0 3))
                   (parfait? (subseq chord (- length 2) length))))
      chord)))
                 
(defun parfait? (chord)
  "Tests if the first 3rd notes in chord do a perfect chord."
  (when (= 700 (- (nth 2 chord) (nth 0 chord)))
    (let ((tierce (- (nth 1 chord) (nth 0 chord))))
      (if (or (= 300 tierce) (= 400 tierce))
        t nil))))

#|
(parfait? '(6000 6300 6700))
|#


(defun notes-communes (chord1 chord2)
  (let ((r '()))
    (mapcar #'(lambda (x)
                (mapcar #'(lambda (y)
                            (when (=? y x 1/4)
                              (push y r))) chord1)) chord2)
    (reverse r)))

(defun =? (x y approx)
  (let ((a (1- (* 100 approx))))
    (or (not (null (pos-int-dom (list (list (- x a)
                                            (+ x a))
                                      )
                                y))))))

#|
(=? 6000 6024 1/4)
|#
    
#|
(notes-communes '(6000 6300 6700 6800 6900) '(6000 6300 6700 6920))
|#

(defmethod 1/4? ((chord1 number) (chord2 number))
  "Test for each pitch in chord2 if it is a quarter-tone higher than each
pitch in chord1."
  (not (null (pos-int-dom (list (list (- chord1 50)
                                      (- chord1 25))
                                (list (+ chord1 25)
                                      (+ chord1 50)))
                          chord2))))

(defmethod 1/4? ((chord1 number) (chord2 list))
  "Test for each pitch in chord2 if it is a quarter-tone higher than each
pitch in chord1."
  (mapcar #'(lambda (x) (1/4? chord1 x)) chord2))

(defmethod 1/4? ((chord1 list) (chord2 list))
  "Test for each pitch in chord2 if it is a quarter-tone higher than each
pitch in chord1."
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (1/4? x y)) chord2)) chord1))

#|
(1/4? '(6000 3000) '(3000 6050 6024 6100))
|#
           

(defun ress-chord (chord1 chord2 pc p1/4)
"Mesure de ressemblance entre deux accords.
pc = poids des notes communes;
p1/4 = poids des notes en quart."
  (let ((notes-communes (notes-communes chord1 chord2))
        (notes-1/4 (remove 'nil (mapcar #'(lambda (x) (not (null (member t x))))
                                        (1/4? chord1 chord2)))))
    (float (/ (/ (+ (* pc (length notes-communes))
                    (* p1/4 (length notes-1/4)))
                 (/ (length (append chord1 chord2)) 2))
              pc))))


#|
;exemples :
(ress-chord '(5000 6200 6400) '(6000 6300 6700) 2 1)
(ress-chord '(6000 6300 6700) '(6000 6300 6700) 2 1)
(ress-chord '(6000 6300 6700) '(6000 6300 6750) 2 1)
(ress-chord '(6700 6300 6000 6800) '(6000 6300 6700 6850) 2 1)
(ress-chord '(6000 6300 6700 6800) '(6000 6300 6750) 2 1)
|#

(defmethod ressemblance ((chord1 list) (chord2 list) (pc number) (p1/4 number))
  "Gives a measure of proximity of chord1 and chord2 according to the
weigth pc for common pitches and weigth p1/4 for pitches a quarter-tone close
to corresponding pitches. Chord1 can be a list of chords if chord2 is empty."
  (cond ((and (not (ldlp chord1)) (not (ldlp chord2)))
         (- 1 (ress-chord chord1 chord2 pc p1/4)))
        ((and (ldlp chord1) (not (ldlp chord2)))
         (mapcar #'(lambda (x) (- 1 (ress-chord x chord2 pc p1/4))) chord1))
        ((and (not (ldlp chord1)) (ldlp chord2))
         (mapcar #'(lambda (x) (- 1 (ress-chord chord1 x pc p1/4))) chord2))

        ((and (ldlp chord1) (ldlp chord2))
        (mapcar #'(lambda (x)
                     (mapcar #'(lambda (y)
                                 (- 1 (ress-chord x y pc p1/4)))
                             chord2))
                 chord1))))

(defmethod ressemblance ((chord1 list) (chord2 null) (pc number) (p1/4 number))
  "Gives a measure of proximity of chord1 and chord2 according to the
weigth pc for common pitches and weigth p1/4 for pitches a quarter-tone close
to corresponding pitches. Chord1 can be a list of chords if chord2 is empty."
  (let ((r '()))
    (dotimes (l1 (length chord1) (reverse r))
      (dotimes (l2 (length chord1))
        (when (> (- l2 l1) 0)
          (push (list (nth l1 chord1)
                      (nth l2 chord1)
                      (ressemblance (nth l1 chord1)
                                    (nth l2 chord1)
                                    pc p1/4)) r))))))

#|
(defun 1/2matrice (l-seq)
  (let ((r '()))
    (dotimes (l1 (length l-seq) (reverse r))
      (dotimes (l2 (length l-seq))
        (cond ((> (- l2 l1) 0)
               (push (list (nth l1 l-seq)
                           (nth l2 l-seq)
                           (morph::dist-2 (cdr (nth l1 l-seq))
                                   (cdr (nth l2 l-seq))
                                   1 1 0 1))
                     r)))))))
|#

;;; GESTES


(defun if2 (input1 input2)
  (if input1
    (append (list input1)
            (cdr input2))
    (cond 
     ((not input2)
      (list 6000))
     ((and (not (ldlp input2)) (listp input2))
      input2)
     (t (print-error "if2 -> Not yet implemented !!")
        (abort)))))

#|
(if2 nil '(6000 7200))
(if2 nil '((6000 7200) (6000 7200)))
(if2 nil nil)
(if2 6100 '(6000 7200 7300))
|#

(defun make-offsets (extend pdate dur)
  (butlast (om::om+ (if (null (car pdate))
                      0
                      (- dur (cadadr pdate)))
                    (om::arithm-ser 0
                                    (* dur (om::om-round extend 0 dur))
                                    dur))))

(defmethod make-rythme ((extend null) (rythm list) (dur list) (nuances list))
   (let ((rythm-p (if rythm t nil)))
     (cond ((null rythm)
            (setf rythm (list 1000)))
           ((member 't (mapcar #'symbolp rythm))
            (setf rythm (funcall #'meaning rythm))))
     (let ((nb  (1+ (length rythm)))
           (r '(0))
           (d '())
           (nu '()))
       
       (cond ((null dur)
              (setf dur rythm))
             ((member 't (mapcar #'symbolp dur))
              (if (equalp 'motif (car dur))
                (setf dur (funcall #'meaning (cdr dur)))
                (setf dur (echant (funcall #'meaning dur) nb)))))
       
       (cond ((null nuances)
              (setf nuances (list 120)))
             ((member 't (mapcar #'symbolp nuances))
              (if (equalp 'motif (car nuances))
                (setf nuances (funcall #'meaning (cdr nuances)))
                (setf nuances (echant (funcall #'meaning nuances) nb)))))
       
       (if rythm-p
         (loop for n from 0 to (1- nb)
               do
               (when (< n (1- nb))
                 (push (+ (nth (mod n (length rythm)) rythm) (car r)) r))
               (push  (nth (mod n (length dur)) dur) d)
               (push  (nth (mod n (length nuances)) nuances) nu)
               )
         (setf r (list 0)
               d (list (car dur))
               nu (list (car nuances))))
       (values (reverse r) (reverse d) (reverse nu)))))

#|
(make-rythme nil  nil nil nil)
(make-rythme nil  nil nil '(100 110))
(make-rythme nil  nil '(100 110) nil)
(make-rythme nil  '(500) nil nil)
(make-rythme nil  '(200 300 100) '(50  100 300) '(20 60))
(make-rythme nil  '(200 300 100) '(50  100 300) '(p cresc f))
(make-rythme nil  '(200 300 100) '(50  << 300 step 100) '(p cresc f))
(make-rythme nil  '(100 to 300 step 50) '(50 100) '(p cresc f))
(make-rythme nil  '(100 to 300 step 10) '(50 to 100 step 10) '(p cresc f))
(make-rythme 2000  '(200 300 100) '(50  100 300) '(20 60))
(make-rythme 2000  '(200 300 100) '(50  100 300) '(20 60))
|#


(defmethod make-rythme ((extend number) (rythm list) (dur list) (nuances list))
; !! quant rules extend final peut etre superieur a extend initial
  (assert (> extend 0))
  (let ((rythm-p (if rythm t nil)))
  (cond ((null rythm)
         (setf rythm (list extend)))
        ((member 't (mapcar #'symbolp rythm))
         (setf rythm (funcall #'meaning rythm))))

  (let ((r '(0))
        (d '())
        (nu '())
        (test-dur (member 't (mapcar #'symbolp dur)))
        (test-nuances (member 't (mapcar #'symbolp nuances)))
        (i 0))

    (when (null dur)
           (setf dur rythm))
    (when (null nuances)
           (setf nuances (list 120)))

    (loop for n from 0 
          until (> (+ (if test-dur 0 (nth (mod n (length dur)) dur))
                      (nth (mod n (length rythm)) rythm) 
                      (car r))
                   extend)
          do
          (push (+ (nth (mod n (length rythm)) rythm) (car r)) r)
          (when (null test-dur)
            (push  (nth (mod n (length dur)) dur) d))
          (when (null test-nuances)
            (push  (nth (mod n (length nuances)) nuances) nu))
          (setf i n)
          )
    (if rythm-p
      (let ((nb (length r)))
        (if test-dur
          (if (equalp 'motif (car dur))
            (setf d (funcall #'meaning (cdr dur)))
            (setf d (echant (funcall #'meaning dur) nb)))
          (setf d (reverse (push (nth (mod (1+ i) (length dur)) dur) d))))
        (if test-nuances
          (if (equalp 'motif (car nuances))
            (setf nu (funcall #'meaning (cdr nuances)))
            (setf nu (echant (funcall #'meaning nuances) nb)))
          (setf nu (reverse (push  (nth (mod (1+ i) (length nuances)) nuances) nu)))))
      (setf r (list 0)
            d (list (car dur))
            nu (list (car nuances))))
    (values (reverse r) d nu))))

#|
(make-rythme 2000  nil nil nil)
(make-rythme 2000  nil nil '(40 60))
(make-rythme 2000  nil '(100 200 150) '(40 60))
(make-rythme 2000  '(100 300 200) '(100 200 150) '(40 60))
(make-rythme 2000  '(100 300 200) '(short then long) '(40 60))
(make-rythme 2000  '(100 300 200) '(100 200 150) '(p cresc f))
(make-rythme nil  '(100 300 200) '(100 200 150) '(p cresc f))
(make-rythme nil  '(100 to 300 step 50) '(50 100) '(p cresc f))
(make-rythme nil  '(100 to 300 step 10) '(50 to 100 step 10) '(p cresc f))
|#

(defmethod make-pitches ((pitches list) (harm list) (npitches integer) &key previous-pitch)
  (assert (> npitches 0))
  (when (null pitches) (setf pitches (make-list npitches :initial-element 6000)))
  (when (member 't (mapcar #'symbolp pitches))
    (setf pitches (funcall #'meaning pitches)))
  (let ((newpitches (if2 previous-pitch pitches))
        (notes '())
        (np (length pitches))
        (nldlpp (not (ldlp pitches))))
    (if (= 1 npitches)
      (setf notes (car newpitches))
      (cond
       ((and nldlpp (= 2 np))             ;;pitches = ambitus = '(start stop)
        (if (= (first newpitches) (cadr newpitches))      ;; intervalle ambitus unisson ou non
          (setf notes (make-list npitches :initial-element (first newpitches)))
          (setf notes (apply #'serie-lineaire (append newpitches (list npitches))))))
       
       ((and nldlpp (= 1 np))             ;;pitches = valeurs = une note
        (setf notes newpitches))
       
       ((and nldlpp (> np 2 ))            ;;pitches = list de val > 2 = 
        (if (< np npitches)
          (setf notes (append newpitches (make-list (- npitches np) :initial-element (car (last newpitches)))))
          (setf notes (subseq newpitches 0 npitches))))
       
       (t 
        (print-error (format nil "make-pitches - 1  :~% value ~S~% not-yet-implemented." (list pitches np))))))
    (when harm
      (if (not (member 'nil (mapcar #'numberp harm)))
        (setf notes (harmofiltre notes harm 200))
        (if (symbolp (car harm))
          (setf notes (funcall (car harm) notes (cdr harm)))
          (print-error "make-pitches - 2 : Not yet implemented"))))
    (splitmidi2
     (if previous-pitch
       (cdr notes)
       notes))
    ))

(defmethod make-pitches ( (pitches symbol) (harm list) (npitches null) &key previous-pitch)
   (make-pitches (if (null pitches) 6000 (car (meaning (list pitches)))) harm 1 :previous-pitch previous-pitch))

(defmethod make-pitches ( (pitches symbol) (harm list) (npitches integer) &key previous-pitch)
   (make-pitches (if (null pitches) 6000 (car (meaning (list pitches)))) harm npitches :previous-pitch previous-pitch))

(defmethod make-pitches ( (pitches list) (harm list) (npitches null) &key previous-pitch)
  (make-pitches pitches harm 1 :previous-pitch previous-pitch))

(defmethod make-pitches ( (pitches number) (harm list) (npitches null) &key previous-pitch)
  (make-pitches (list pitches) harm 1 :previous-pitch previous-pitch))

(defmethod make-pitches ( (pitches number) (harm list) (npitches integer) &key previous-pitch)
  (make-pitches (make-list npitches :initial-element pitches) harm npitches :previous-pitch previous-pitch))

(defmethod make-pitches ( (pitches null) (harm list) (npitches null) &key previous-pitch)
  (make-pitches (make-list 1 :initial-element 6000) harm 1))


#|
(make-pitches  6000 '(6000 8000) 4 :previous-pitch 7200)
(make-pitches  6000 '(6000 8000) 4)
(make-pitches  '(6000 8000) '(6000 8000) 4 :previous-pitch 7200)
(make-pitches  '(6000 8000) '(6000 8000) 4)
(make-pitches  '(6000 8000) '(6000 8000) nil)
(make-pitches  '(6000 8000) '(6000 8000) nil :previous-pitch 7200)
(make-pitches  '(6000 8000) nil 4 :previous-pitch 7200)
(make-pitches  '(6000 8000) nil 4)
(make-pitches  '(6000 8000) nil 2)
(make-pitches  '(6000 6000) nil 2 :previous-pitch 7200)
(make-pitches  '(6000 8000) nil 3)
(make-pitches  '(6000 6200 8000) nil 2)
(make-pitches  '(6000 6200 8000) nil 3)
(make-pitches  '(6000 6200 6500 8000) nil 4)
(make-pitches  '(6000 6200 8000) nil 4)
(make-pitches  '(6000 6200 8000) nil 8)
(make-pitches  4800 nil nil)
(make-pitches  4800 nil 1)
(make-pitches  4800 nil 2)
(make-pitches  4800 nil 7)
(make-pitches  4800 nil 7 :previous-pitch 7200)  ;; ????? 
(make-pitches  nil nil nil)
(make-pitches  nil nil 3)
(make-pitches  nil nil 2)
(make-pitches  nil nil 1)
(make-pitches  nil 10 1)
(make-pitches  nil 10 nil :previous-pitch 7200)
(make-pitches  'c4 nil nil)
|#

(defun ldlp (arg)
  (not (member 'nil (mapcar #'listp arg))))

(defun mk-offset-from-dur (durs)
  (let ((r '(0)))
    (dolist (d durs (reverse r))
      (push (+ d (car r)) r))))

#|
(mk-offset-from-dur  '(100 200 100))
|#


(defun trait-sp5 (extend pdate pitches dur harm rythm nuances)
  (let (notes
        (onset1 '()) (onset2 '())
        (dur1 '()) (dur2 '())
        (vel1 '()) (vel2 '()))
    (multiple-value-bind (onsets durs vels) (make-rythme extend rythm dur nuances)
      (setf notes (make-pitches pitches harm (length onsets)
                                :previous-pitch (caadr pdate)))
      (dotimes (n (length onsets))
        (if (car (nth n notes))
          (and (push (nth n durs) dur1)
               (push (nth n onsets) onset1)
               (push (nth n vels) vel1))
          (and (push (nth n durs) dur2)
               (push (nth n onsets) onset2)
               (push (nth n vels) vel2))))
      (values (remove 'nil (mapcar #'car notes))
              (remove 'nil (mapcar #'cadr notes))
              (reverse onset1) (reverse onset2)
              (reverse dur1) (reverse dur2)
              (reverse vel1) (reverse vel2)
              (list (if (caar (last notes))
                      (caaar (last notes))
                      (caadar (last notes))) (car (last durs)))))))

#|
(trait-sp5 2000 '(1000 (6000 100)) '(6000 7200) '(500) nil '(250) '(30 60 100))
(trait-sp5 2000 '(1000 (6000 100)) '(6000 7200) 500 nil 250 '(30 60 100))
(trait-sp5 2000 nil '(6000 7200) nil nil nil nil)
(trait-sp5 2000 nil '(6000) nil nil nil nil)
(trait-sp5 2000 nil 6000 nil nil nil nil)
(trait-sp5 2000 nil nil nil nil nil nil)
(trait-sp5 nil nil nil nil nil nil nil)
(trait-sp5 2000 nil nil nil nil '(125 250) nil)
|#
(symbolp nil)

(defun make-chord-seq (pitches onsets durs vels chan)
  (make-instance 'om::chord-seq 
        :Lmidic pitches
        :LOnset onsets
        :Ldur durs
        :LVel vels
        :LOffset (list 0)
        :Lchan chan))

(defun make-multiseq (list-of-chords-seqs)
  (make-instance 'om::multi-seq :chord-seqs list-of-chords-seqs))

#|
(make-chord-seq '((6000) (6300) (6600) (6900) (7200))
                '(0 500 1000 1500 2000)
                '((125) (125) (125) (125) (125))
                '(100) '(1))
(make-multiseq (list (make-chord-seq '(6000) '(250) '(250) '(100) '(1)) 'nil))
|#


(defun call-harm (call)
  (assert (listp call))
  (if (stringp (car call))
     (print-error "call-harm : not yet implemented.")
     (cond ((not (member nil (mapcar #'numberp call)))
            (list 'harmofiltre call))
           (t (print-error "call-harm : not yet implemented.")))))

(om::defmethod! strait1 ((tempobj om::omboxtempobj) (offset t) (extend t) (pdate t) (pitches t) (dur t) (harm t) (rythm t) (nuances t))
  :icon 128
  :numouts 8
  (multiple-value-bind (notes1 notes2 onsets1 onsets2 durs1 durs2 vel1 vel2 lastnote)
                       (trait-sp5 extend pdate pitches dur harm rythm nuances)
    (let ((chan1 (if (null notes1) nil (make-chord-seq notes1 onsets1 durs1 vel1 1)))
          (chan2 (if (null notes2) nil (make-chord-seq notes2 onsets2 durs2 vel2 2)))
          (realextend (+ (apply #'max (append onsets1 onsets2)) (cadr lastnote))))
      
      (values (make-multiseq (remove nil (list chan1
                                               chan2)))   ;; multi-seq
              tempobj                                     ;;tempobject self
              (if pdate (car pdate) offset)                  ;;date fin tempobj precedent
              realextend                                  ;;tempobject real extend
              (+ (if (and offset (numberp offset))
                   offset 0) realextend)                  ;;tempobj end date
              lastnote                                    ;;multi-seq last note
              nil                                         ;; in case of
              nil                                         ;; in case of
              ))))

(om::defmethod! strait1 ((tempobj null) (offset t) (extend t) (pdate t) (pitches t) (dur t) (harm t) (rythm t) (nuances t))
  :icon 128
  :numouts 8
  (multiple-value-bind (notes1 notes2 onsets1 onsets2 durs1 durs2 vel1 vel2 lastnote)
                       (trait-sp5 extend pdate pitches dur harm rythm nuances)
    (let ((chan1 (if (null notes1) nil (make-chord-seq notes1 onsets1 durs1 vel1 1)))
          (chan2 (if (null notes2) nil (make-chord-seq notes2 onsets2 durs2 vel2 2)))
          (realextend (+ (apply #'max (append onsets1 onsets2)) (cadr lastnote))))
      (values (make-multiseq (remove nil (list chan1
                                               chan2)))   ;; multi-seq
              nil                                         ;;tempobject self
              (if pdate (car pdate) offset)                  ;;date fin tempobj precedent
              realextend                                ;;tempobject real extend
              (+ (if (and offset (numberp offset))
                   offset 0) realextend)                  ;;tempobj end date
              lastnote                                    ;;multi-seq last note
              nil                                         ;; in case of
              nil                                         ;; in case of
              ))))



#|
(strait1 nil nil 2000 nil nil '(125 250) nil '(125 250) '(60 80))
(strait1 nil nil nil nil nil nil nil nil nil)
(strait1 nil nil nil nil nil nil nil nil nil)
(strait1 nil nil 500 nil nil nil nil nil nil)
(strait1 nil nil 250 nil nil nil nil nil nil)
|#

;; filtres de champs harmoniques


(defmethod harm ((pitches number) (set list))
  (assert (not (ldlp set)))
  (let ((delta (mapcar #'(lambda (x) (abs (- x pitches))) set)))
    (nth (position (apply #'min delta) delta) set)))

(defmethod harm ((pitches list) (set list))
  (if (ldlp pitches)
    (print-error "harm : Not yet implemented")
    (mapcar #'(lambda (x) (harm x set)) pitches)))

(defmethod harm ((pitches number) (set number))
  (list set))

(defmethod harm ((pitches list) (set number))
  (list set))

#|
(harm 59 '(58 60 62))
(harm '(59 61 63 65) '(60 64))
(harm '(59 61 63 65) '(60 62 64 66))
|#

 ;;LA note la plus proche...
(defmethod filtre-marg ((pitches list) (set list) (margin integer))
  (assert (not (ldlp set)))
  (let ((min-delta (mapcar #'(lambda (x)
                               (mapcar #'(lambda (y) (abs (- x y))) set)) pitches)))
    (setf min-delta (mapcar #'(lambda (x)
                                (position-if #'numberp
                                             (mapcar #'(lambda (a) (in-margin a margin)) x))) min-delta))
    (dotimes (n (length pitches) pitches)
      (when (nth n min-delta) (setf (nth n pitches)
                                    (nth (nth n min-delta) set))))))

#|
(filtre-marg '(0 1 2 3 4 5) '(2 4) 1)
|#

(defun in-margin (x m)
  (if (<= x m)
    x nil))


(defmethod filtre-harm ((pitches list) (set list))
  (assert (not (ldlp set)))
  (let ((min-delta (mapcar #'(lambda (x)
                               (mapcar #'(lambda (y) (abs (- x y))) set)) pitches))
        (ok '()))
    (dotimes (n (length pitches) pitches)
      (let ((position (position (apply #'min (nth n min-delta)) (nth n min-delta))))
        (when (<= (nth position (nth n min-delta))
                  (apply #'min (mapcar #'(lambda (x) (nth position x)) min-delta)))
          (when (not (assoc (nth position set) ok))
            (setf ok (append ok (list (list (nth position set) (nth n pitches)))))
            (dotimes (k  (- (length pitches) n 1))
              (setf (nth position (nth (+ k n 1) min-delta))
                    (+ 99999 (nth position (nth (+ k n 1) min-delta))))))
          (setf (nth n pitches) (nth position set)))))))

#|
(filtre-harm '(0 1 1.5 1.6 1.5 2.5 3 4.6 5 2.1 5 4.3 3.4 1.5 2.5 1 0) '(2 4))
|#

(defmethod filtre-harm2 ((pitches list) (set list) (marge number))
  (assert (not (ldlp set)))
  (let ((min-delta (mapcar #'(lambda (x)
                               (mapcar #'(lambda (y) (abs (- x y))) set)) pitches))
        (ok '()))
    (dotimes (n (length pitches) pitches)
      (let ((position (position (apply #'min (nth n min-delta)) (nth n min-delta))))
        (when (and (<= (nth position (nth n min-delta))
                       (apply #'min (mapcar #'(lambda (x) (nth position x)) min-delta)))
                   (<= (abs (- (nth position set) (nth n pitches)))
                       marge))
          (when (not (assoc (nth position set) ok))
             (setf ok (append ok (list (list (nth position set) (nth n pitches)))))
            (dotimes (k  (- (length pitches) n 1))
              (setf (nth position (nth (+ k n 1) min-delta))
                    (+ 99999 (nth position (nth (+ k n 1) min-delta))))))
          (setf (nth n pitches) (car (assoc (nth position set) ok))))))))

#|
(time (filtre-harm '(0.6 1.7 3 4.6 5 5 4.6 3.4 1.8 1) '(0 2 4)))
(time (filtre-harm2 '(0.6 1.7 3 4.6 5 5 4.6 3.4 1.8 1) '(0 2 4) 1))
|#

(defmethod harmofiltre ((pitches list) (set list) &optional marge)
  (if marge
    (filtre-harm2 pitches set marge)
    (filtre-harm pitches set)))

(defmethod harmofiltre ((pitches number) (set list) &optional marge)
  (harmofiltre (list pitches) set marge))

#|
(numberp (position #\Space "123"))

(defun strtosy (string)
  (if (not (numberp (position #\Space string)))
    (read-from-string string)
    (let ((newstring (strin
|#








                 
  





