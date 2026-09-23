(in-package :lol)



(defvar *LOL-ENVIRONNEMENT* nil)

(defparameter *support-set* '(w! No Yes))
(defparameter *direction-set* '(w! P 1 2 3 4 5 6 7 8)) ; w!
(defparameter *level-set* '(w! M B H))
(defparameter *flexion-set* '(w! -2 -1 1 2 3)) ; 0
(defparameter *distance-set* '(w! -2 -1 1 2)) ;0
(defparameter *curve-set* '(w! -2 -1 1 2)) ; 0
(defparameter *rotation-set* '(w! -2 -1 1 2)) ; 0
(defparameter *contact-set* '(w! t 1 2 3 4 5 6 7 8 9 10)) ; w!
(defparameter *address-set* '(w! )) ;w!



(setf *gensym-counter* 1)

(defvar *wind-sit-back-color* 14592000)
(defvar *wind-sit-edit-color* 14605000)



(defparameter *dimension-set* '((:support (w! No Yes))
                                (:direction (w! P 1 2 3 4 5 6 7 8))
                                (:level (w! M B H))
                                (:flexion (w! -2 -1 1 2 3))
                                (:distance (w! -2 -1 1 2 3))
                                (:curve (w! -2 -1 1 2 3))
                                (:rotation (w! -2 -1 1 2 3))
                                (:contact (w! -2 -1 1 2 3))
                                (:address (w! -2 -1 1 2 3))
                                ))

(defmethod get-def-values ((dimension keyword))
  (cadr (assoc dimension *dimension-set*)))

(defmethod get-def-values ((dimension symbol))
  (cadr (assoc (make-keyword dimension) *dimension-set*)))

(defmethod get-def-values ((dimension list))
  (mapcar #'get-default-values dimension))

(defmethod get-def-values ((dimension null))
  (let ((dim (mapcar #'car *dimension-set*)))
    (values (mapcar #'get-default-values dim)
            dim)))


(defun get-default-values (&optional dim)
  (get-def-values dim))

;(get-default-values :direction)
;(get-default-values 'direction)
;(get-default-values)




(defmethod set-default-values ((parameter symbol))
  (cond ((eq 'support parameter)
         (setf *support-set* (mapcar #'read-from-string
                                     (seqstring-to-liststrings
                                      (get-string-from-user (format nil "Please, enter your support values. Previous values was:~%~S" *support-set*)
                                                            :size #@(500 100)
                                                            :position :centered
                                                            :window-title "Support values set")))))
        ((eq 'direction parameter)
         (setf *direction-set* (mapcar #'read-from-string
                                     (seqstring-to-liststrings
                                      (get-string-from-user (format nil "Please, enter your direction values. Previous values was:~%~S" *direction-set*) 
                                                            :size #@(500 100)
                                                            :position :centered
                                                            :window-title "Direction values set")))))
        
        ((eq 'level parameter)
         (setf *level-set* (mapcar #'read-from-string
                                     (seqstring-to-liststrings
                                      (get-string-from-user (format nil "Please, enter your level values. Previous values was:~%~S" *level-set*)
                                                            :size #@(500 100)
                                                            :position :centered
                                                            :window-title "Level values set")))))
        ((eq 'flexion parameter)
         (setf *flexion-set* (mapcar #'read-from-string
                                     (seqstring-to-liststrings
                                      (get-string-from-user (format nil "Please, enter your flexion values. Previous values was:~%~S" *flexion-set*)
                                                            :size #@(500 100)
                                                            :position :centered
                                                            :window-title "Flexion values set")))))
        ((eq 'distance parameter)
         (setf *distance-set* (mapcar #'read-from-string
                                     (seqstring-to-liststrings
                                      (get-string-from-user (format nil "Please, enter your distance values. Previous values was:~%~S" *distance-set*)
                                                            :size #@(500 100)
                                                            :position :centered
                                                            :window-title "Distance values set")))))
        ((eq 'curve parameter)
         (setf *curve-set* (mapcar #'read-from-string
                                     (seqstring-to-liststrings
                                      (get-string-from-user (format nil "Please, enter your curve values. Previous values was:~%~S" *curve-set*)
                                                            :size #@(500 100)
                                                            :position :centered
                                                            :window-title "Curve values set")))))
        ((eq 'rotation parameter)
         (setf *rotation-set* (mapcar #'read-from-string
                                     (seqstring-to-liststrings
                                      (get-string-from-user (format nil "Please, enter your rotation values. Previous values was:~%~S" *rotation-set*)
                                                            :size #@(500 100)
                                                            :position :centered
                                                            :window-title "Rotation values set")))))
        ((eq 'contact parameter)
         (setf *contact-set* (mapcar #'read-from-string
                                     (seqstring-to-liststrings
                                      (get-string-from-user (format nil "Please, enter your contact values. Previous values was:~%~S" *contact-set*)
                                                            :size #@(500 100)
                                                            :position :centered
                                                            :window-title "Contact contact set")))))
        ((eq 'address parameter)
         (setf *address-set* (mapcar #'read-from-string
                                     (seqstring-to-liststrings
                                      (get-string-from-user (format nil "Please, enter your address values. Previous values was:~%~S" *address-set*)
                                                            :size #@(500 100)
                                                            :position :centered
                                                            :window-title "Address values set")))))
        ((eq 'all parameter)
         (mapcar #'set-default-values '(support direction level distance flexion curve rotation contact address)))
        (t (lol-error-msg 
            (format nil "I don't understand this parameter name~%Please try :~%
'support~%'direction~%'level~%'distance~%'flexion~%'curve~%'rotation~%'contact~%'address~%'all")
            :size #@(350 250)))))


#|
(set-default-values 'distance)
(set-default-values 'all)
|#

#|
(defun print-default ()
  (format t "Default values : ~%")
  (format t "~%support : ~S~%" *support-set*)
  (format t "direction : ~S~%" *direction-set*)
  (format t "level : ~S~%" *level-set*)
  (format t "flexion : ~S~%" *flexion-set*)
  (format t "distance : ~S~%" *distance-set*)
  (format t "curve : ~S~%" *curve-set*)
  (format t "rotation : ~S~%" *rotation-set*)
  (format t "contact : ~S~%" *contact-set*)
  (format t "address : ~S~%" *address-set*)
  (values))
|#


(print-default)

