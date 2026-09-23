(in-package :lol)


(defclass garbage ()
  ())

(defun kill-class (class)
  (eval `(defclass ,class (garbage) ())))


(defmethod body ((self symbol))
  (body (eval self)))


(defmethod rename ((self situation) (new-name symbol))
   (let ((old (name self)))
     (setf (symbol-value (make-new-symbol new-name))
           self
           (symbol-value (read-from-string (name self)))
           nil)
     (makunbound (read-from-string (name self)))
     (setf (name self) (string new-name))
     (format t "~%Situation ~S renamed as ~S." old (name self))
     (eval self)))
  

#|
(setf b (make-instance 'situation))
(rename b 'c)
(inspect c)
|#

(defmethod lol-add ((in SITUATION) &rest elts )
  (loop for e in elts
        do
        (cond ((bodypart-p e)
               (if (find e (body in))
                 (format t "~%LOL> Warning : ~S is already in ~S." (name e) (name in))
                 (setf (slot-value in 'body) (append (body in) (list e)))
                   ))
              
              ((member (find-class 'dimension) (class-precedence-list e))  ;;; e ici doit etre un find-class...
               (mapcar #'(lambda (b)
                           (let ((a (slot-value b 'dimension-list)))
                             (when (not (find-if #'(lambda (x)
                                                     (eq (class-name e) (class-name (class-of x))))
                                                 a))
                               
                               (setf (slot-value b 'dimension-list)
                                     (append a
                                             (list (make-instance (class-name e))))))))
                       (body in)))
              (t (format t "~‰LOL>lol-add not yet implemented (~S)..." (type-of e)))))
  (update-dimension-list in)
  (setf (slot-value in 'modif-date) (get-universal-time))
  in)

(defmethod lol-add ((in BODYPART) &rest elts)
   (loop for e in elts
         do
         (if (find e (dimension in))
           (format t "~%LOL> Warning : ~S is already in ~S." (name e) (name in))
           (if (dimension-p e)
             (setf (slot-value in 'dimension-list) (append (dimension-list in) (list e)))
             (format t "~%LOL> Warning : ~S is not a dimension; can't add to ~S." (name e) (name in)))))
   (setf (slot-value in 'modif-date) (get-universal-time))
   in)



(defmethod lol-add ((in symbol) &rest elts )
  (apply #'lol-add (append (list (eval in)) elts)))


(defmethod lol-remove ((in symbol) &rest elts )
  (apply #'lol-remove (append (list (eval in)) elts)))



(defmethod lol-remove ((in SITUATION) &rest elts )
   (loop for e in elts
         do
         (cond ((bodypart-p e)
                (setf (slot-value in 'body) (remove-if #'(lambda (x) (member x elts :test #'equalp)) (body in))))
               ((member (find-class 'dimension) (class-precedence-list e))  ;;; e ici doit etre un find-class...
                (mapcar #'(lambda (b)
                            (setf (slot-value b 'dimension-list)
                                  (remove-if #'(lambda (x) (equal (class-name e)
                                                                  (class-name (class-of x))))
                                             (dimension-list b))))
                        (body in))
                )
               (t (format t "~‰LOL>lol-remove not yet implemented (~S)..." (type-of e)))))
  (setf (slot-value in 'modif-date) (get-universal-time))
  (update-dimension-list in)
  in)


(defmethod lol-remove ((in BODYPART) &rest elts )
  (setf (slot-value in 'dimension-list) (remove-if #'(lambda (x) (member x elts :test #'equalp)) (dimension-list in)))
  (setf (slot-value in 'modif-date) (get-universal-time))
  (update-dimension-list in)
  in)


#|
(setq a (make-instance 'situation))
(lol-remove a (find-class 'support))
(inspect a)
(setq b (make-instance 'bodypart))
(lol-add a b)
(inspect a)
(inspect b)
(slots 'bodypart)
(setq c (make-instance 'dimension))
(inspect c)
(lol-add b c)
|#


(defun make-body-part-instance (&optional bodypart-class)
  (if bodypart-class
    (make-instance bodypart-class)
    (let ((selected (select-item-from-list  (append (list 'bodypart)
                                                    (mapcar #'class-name
                                                            (find-subclasses 'BODYPART)))
                                            :default-button-text "select"
                                            :selection-type :disjoint
                                            :window-title "bodyparts")))
       (mapcar #'(lambda (x)
                    (make-instance x))
                selected))))



(defun make-dimension-instance (&optional dimension-class)
  (if dimension-class
    (make-instance dimension-class)
    (let ((selected (select-item-from-list  (append (list 'dimension)
                                                    (mapcar #'class-name
                                                            (find-subclasses 'dimension)))
                                            :default-button-text "select"
                                            :selection-type :disjoint
                                            :window-title "dimensions")))
       (mapcar #'(lambda (x)
                    (make-instance x))
                selected))))


(defmethod update-dimension-list ((self situation))
  (if (null (dimension-list self))
    (setf (slot-value self 'dimension-list) 
          (remove-duplicates (mapcar #'(lambda (x)
                                         (class-name (class-of x)))
                                     (apply #'append (mapcar #'dimension (body self))))
                             :test #'eq))
    (let ((dims (remove-duplicates
                 (mapcar #'(lambda (x)
                             (class-name (class-of x)))
                         (remove 'nil (apply #'append (mapcar #'dimension (body self)))))))
          (dimension-list (dimension-list self)))
      (loop for d in dims
            do
            
            (when (not (member d dimension-list :test #'eq))
              (setf (slot-value self 'dimension-list)
                    (append dimension-list (list d)))))
      (loop for d in dimension-list
            do
            (when (not (member d dims :test #'eq))
              (setf (slot-value self 'dimension-list)
                    (remove d (slot-value self 'dimension-list) :test #'equal))))))
  self)


(defmethod copy ((self dimension))
  (make-instance (class-name (class-of self))
                       :name (copy-tree (name self))
                       :type (copy-tree (type self))
                       :mode (copy-tree (mode self))
                       :values-set (copy-tree (values-set self))
                       :datum (copy-tree (datum self))
                       :free (copy-tree (free self))))


(defmethod copy ((self bodypart))
  (let ((copy-of-b (make-instance (class-name (class-of self))
                       :name (string (class-name (class-of self)))
                       :creation-date (get-universal-time)
                       )))
    (setf (slot-value copy-of-b 'dimension-list)
          (loop for d in (dimension-list self)
                collect (copy d)))
    
    copy-of-b ))


(defmethod copy ((self situation))
  (let ((copy-of-sit (make-instance 'situation
                       :name (copy-tree (name self))
                       :danser (make-instance (class-name (class-of (danser self))))
                       ;:data (copy-tree (data self))
                       :color (copy-tree (color self))
                       ;:body (copy-tree (list))
                       :ancestor (list (name self) 'copy)
                       :dimension-list (copy-tree (dimension-list self))
                       :creation-date (get-universal-time))))
    (setf (slot-value copy-of-sit 'data)
          (loop for d in (data self)
                collect (copy d))
          (slot-value copy-of-sit 'body)
          (loop for b in (body self)
                collect (copy b)))
          
    
    (format t "LOL> Situation ~S copied as ~S." (name self) (name copy-of-sit))
    copy-of-sit))


(defmethod copy ((self null))
   (format t "~%LOL> Warning : copying empty value...~%")
  nil)


(defmethod copy ((self symbol))
   (copy (eval self))
  )

(defmethod update-dimension-list ((self bodypart))
  self)


(defmethod update-dimension-list ((self symbol))
  (update-dimension-list (eval self)))

(defmethod delete-situation ((situation symbol))
  ;(setf situation nil)
  (MAKUNBOUND (if (string (name situation))
                      (read-from-string (name situation))
                      (name situation)))
  ;(format t "~&Situation ~S deleted." (name situation))
  )

(defmethod delete-situation ((situation situation))
  (delete-situation (if (string (name situation))
                      (read-from-string (name situation))
                      (name situation)))
  (format t "~&situation ~S deleted." (name situation)))


(defmethod DIMENSION-LIST ((self symbol))
  (DIMENSION-LIST (eval self)))



(defmethod lolreplace ((old bodypart) (new symbol))
   (assert (find-class new))
   ;(assert (bodypart-p (find-class 'brasd)))
   (make-instance new :dimension-list (mapcar #'copy (dimension-list old))))


#|
(lolreplace (make-instance 'bras) 'brasg)
|#



