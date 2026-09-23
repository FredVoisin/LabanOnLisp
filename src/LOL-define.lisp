(in-package :lol)

;;keywords are sometimes used as symbols to point to classes...
;; see for instance laban-dimensions
(defmethod make-instance ((self keyword) &rest args)
  (setf self (read-from-string (symbol-name self) nil))
  (if args 
  (eval `(make-instance ',self ,.args))
  (eval `(make-instance ',self))))


(defmacro define (name &optional class)
  (cond ((find-class name nil)
         (lol-error-msg (format nil "The category ~S already exists." name))
         (values))
        (t
         (if (null class)
           `(defclass ,name ()
              ())
           `(defclass ,name (,class)
              ())))))

(defun fdefine (category &optional parent)
  "Macro define as a function"
  (eval `(define ,category ,parent)))


#|
(define bras bodypart)
(define pied bodypart)
(define bras0 bras)
(define dim)
(find-subclasses 'bodypart)
(find-subclasses 'bras)
(subclass 'bras 'bodypart)
|#



(defun find-view1 (list class)
  "REDEFINE FIND_VIEW IN THAT WAY....!!!!"
  (loop for view in (coerce list 'list)
        when (eq (class-of view) (find-class class))
        collect view))

(defun define-window (&optional category)
  (let ((category-name (make-instance 'editable-text-dialog-item
                         :view-position #@(62 45) 
                         :view-size #@(140 15)
                         :dialog-item-text "name"
                         :view-nick-name 'category-name))
        parent-name
        (comment-window (make-instance 'static-text-dialog-item
                          :dialog-item-text ""
                          :view-font '("Monaco" 14)
                          :view-position #@(60 75)
                          :view-size #@(400 65)
                          :view-nick-name 'comment-window))
        (allowable-categories (append (list category) (mapcar #'class-name (find-subclasses category)))))
    (setf parent-name
          (make-instance 'pop-up-menu ;'pull-down-menu
                             :view-position #@(282 41) 
                             :view-size #@(160 22)
                             ;:view-font '("Monaco" 14)
                             :menu-title ""
                             :auto-update-default  t
                             :item-display :selection
                             :view-nick-name 'parent-name
                             :menu-items
                             (loop for c in allowable-categories
                                   collect
                                   (make-instance 'menu-item
                                     :menu-item-title (string c)
                                     ;:menu-item-action #'(lambda ())
                                                           ))))
    (make-instance 'window
      :window-title (if category (format nil "Define a new ~S" category) "Define a new category")
      :view-nick-name category
      :window-type :document
      :view-position #@(245 100)
      :view-subviews (list (make-instance 'static-text-dialog-item
                             :dialog-item-text "Category name"
                             :view-font '("Monaco" 14)
                             :view-position #@(75 20))
                           (make-instance 'static-text-dialog-item
                             :dialog-item-text (if category (format nil "Parent ~S" category) "Parent category")
                             :view-font '("Monaco" 14)
                             :view-position #@(290 20))
                           (make-instance 'button-dialog-item
                             :dialog-item-text "Define"
                             :view-size #@(120 20)
                             :view-position (make-point
                                             (- (* (round (/ (point-h *window-default-size*) 3)) 2)
                                                60)
                                             (- (point-v *window-default-size*)
                                                35) )
                             :dialog-item-action #'(lambda (self)
                                                     (get-define-window-data
                                                      (view-container self) 
                                                      (view-nick-name (view-container self)))))
                           (make-instance 'button-dialog-item
                             :dialog-item-text "Default values"
                             :view-size #@(120 20)
                             :view-position (make-point
                                             (- (round (/ (point-h *window-default-size*) 3))
                                                60)
                                             (- (point-v *window-default-size*)
                                                35) )
                             :dialog-item-action #'(lambda (self)
                                                     (get-define-window-data
                                                      (view-container self) 
                                                      (view-nick-name (view-container self)))))
                           category-name
                           parent-name
                           comment-window))))

;(inspect (define-window 'bodypart))


(defun get-define-window-data (window &optional super-class)
  (let ((category (read-from-string (dialog-item-text (view-named 'category-name window))))
        (parent (read-from-string (menu-item-title (selected-item (view-named 'parent-name window))))))
    (cond ((and parent (not super-class))
           (cond ((member category (mapcar #'class-name (find-subclasses parent)))
                  (set-dialog-item-text (view-named 'comment-window window)
                                        (format nil "Can not define ~S :~%~S is already defined as a ~S..."
                                                category category parent))
                  (view-draw-contents (view-named 'comment-window window))
                  (view-draw-contents window)
                  (invalidate-view window))
                 (t (fdefine category parent)
                    (set-dialog-item-text (view-named 'comment-window window)
                                          (format nil "Category ~S defined." category))
                    (view-draw-contents (view-named 'comment-window window))
                    (view-draw-contents window)
                    (invalidate-view window))))
          ((and parent super-class)
           (cond ((or (member parent (mapcar #'class-name (find-subclasses super-class)))
                      (equal parent super-class))
                  (cond ((member category (mapcar #'class-name (find-subclasses parent)))
                         (set-dialog-item-text (view-named 'comment-window window)
                                               (format nil "Can not define ~S :~%~S is already defined as a ~S..."
                                                       category category parent))
                         (view-draw-contents (view-named 'comment-window window))
                         (view-draw-contents window)
                         (invalidate-view window))
                        (t (fdefine category parent)
                           (set-dialog-item-text (view-named 'comment-window window)
                                                 (format nil "Category ~S defined." category))
                           (view-draw-contents (view-named 'comment-window window))
                           (view-draw-contents window)
                           (invalidate-view window))))
                 (t (set-dialog-item-text (view-named 'comment-window window)
                                          (format nil "Error :~%parent ~S is not a ~S..."
                                                  parent super-class))
                    (view-draw-contents (view-named 'comment-window window))
                    (view-draw-contents window)
                    (invalidate-view window))))
          (t (fdefine category super-class)
             (set-dialog-item-text (view-named 'comment-window window)
                                   (format nil "~S ~S defined." super-class category))
             (view-draw-contents (view-named 'comment-window window))
             (view-draw-contents window)
             (invalidate-view window)))))


(defun select-keys0 (args r1 r2 keys)
  (if (null args)
    (values r2 r1)
    (let ((arg (pop args)))
      (if (keywordp arg)
        (if keys
          (if (member arg keys :test #'eq)
            (select-keys0 (cdr args) r1 (append r2 (list (list arg (pop args)))) keys)
            (select-keys0 args (append r1 (list arg)) r2 keys)))
        (select-keys0 args (append r1 (list arg)) r2 keys)))))

#|
(select-keys0 '(1 2 3 'b :test #'= 8 :level 1 9) nil nil nil)
(select-keys0 '(1 2 3 'b :test #'= 8 :level 1 9) nil nil '(:test :level))
(select-keys0 '(1 2 3 'b ) nil nil nil)
|#

(defun select-keys (args allowable-keys)
  (select-keys0 args nil nil allowable-keys))

;(select-keys '(1 2 3 'b :test #'= 8 :level 1 9) '(:level))


(defgeneric edit (self &rest args)
  (:documentation
   "si symbol alors edit class, si instance de class autre que symbol, edit instance..."))


;; A FINIR....cf wedit

(defmethod edit ((self t) &rest args)
  (let ((slots (select-keys args
                            (mapcar #'make-keyword
                                    (slots (class-name (class-of self)))))))
    (loop for slot in slots
          do
          (setf (slot-value self (symbol (car slot))) (cadr slot))))
  (values self))

;(edit (make-instance 'dimension))
;(edit 'dimension)


(defmethod wedit ((self t))
   (let* ((slots (slots self :not '(name)))
          (save-button (make-instance 'button-dialog-item
                         :dialog-item-text "change"
                         :view-size #@(120 20)
                         :view-position (make-point
                                         (- (round (/ (point-h *window-default-size*) 2))
                                            100)
                                         (+ (* 30 (length slots)) 70) )
                         :dialog-item-action #'(lambda (v)
                                                 (let ((default-data 
                                                         (mapcar #'(lambda (view)
                                                                     (list (slot view)
                                                                           (read-from-string
                                                                            (dialog-item-text (elt (view-subviews view) 1))
                                                                            nil)))
                                                                 (find-view1 (view-subviews (view-container v))
                                                                             'edit-slot-view))))
                                                   (loop for d in default-data
                                                         do
                                                         (setf (slot-value self (car d)) (cadr d))))
                                                 (set-dialog-item-text (view-named 'comment-window (view-container v))
                                                                             (format nil "~S changed" self)))))
          (comment-window (make-instance 'static-text-dialog-item
                          :dialog-item-text ""
                          :view-font '("Monaco" 14)
                          :view-position (make-point 30 (+ 35 (* 30 (length slots))))
                          :view-size #@(300 25)
                          :view-nick-name 'comment-window))
          (slots-views (loop for n from 0 to (1- (length slots))
                             collect
                             (make-instance 'edit-slot-view
                               :slot (print (nth n slots))
                               :data (if (nth n slots)
                                       (funcall (nth n slots) self)
                                       nil)
                               :view-size (make-point 400 30)
                               :view-position (make-point 10 (+ 30 (* n 30)))))))
    
    (make-instance 'window
      :window-title (format nil "Edit ~S" self)
      :window-type :document
      :view-position #@(245 100)
      :view-size (make-point 400 (+ (* 30 (length slots)) 100))
      :view-subviews (append slots-views (list save-button comment-window)))
    (values)))

(defmethod wedit ((self symbol))
   (print "not yet implemented to edit class slot"))

;(wedit 'dimension)
;(wedit (make-instance 'dimension))
#|
(let ((dim (make-instance 'dimension)))
  (mapcar #'(lambda (x) (funcall x dim))
          (slots 'dimension)))
|#

(defclass edit-slot-view (view)
  ((slot
    :initarg :slot
    :initform nil
    :reader slot
    :accessor slot
    :type symbol)
   (data
    :initarg :data
    :initform nil
    :reader data
    :accessor data
    :type symbol))
  (:documentation ""))

(defmethod initialize-instance :after ((self edit-slot-view) &key
                                         slot data)
   (apply #'add-subviews (list self (make-instance 'static-text-dialog-item
                              :dialog-item-text (string slot)
                              :view-font '("Monaco" 12)
                              :view-position #@(3 3)
                              :view-size #@(100 20))
                            (make-instance 'editable-text-dialog-item
                              :view-position #@(110 3) 
                              :view-size #@(200 15)
                              :dialog-item-text (format nil "~S" data)
                              :view-nick-name 'data))))


;(make-instance 'edit-slot-view :slot 'support :data '(Y N))






;;;;;  MAKE = MAKE_INSTANCE...




(defgeneric make (object &optional args)
  (:documentation
   "MAKE : LOL generic function.
Make an instance of LOL <object> with arguments <args>.
Example : (make 'body)"))

(defmethod make ((object symbol) &optional args)
   (when (not args) (setf args '(nil)))
   (let ((build-fct (read-from-string (format nil "make-~S" object))))
     (cond ((fboundp build-fct)
            (apply (read-from-string (format nil "make-~S" object)) args))
           (t
            (lol-error-msg
             (format nil "~TInternal error :~%function ~S does not exist.~%~%~TPlease report bug."
                     build-fct)
             :size #@(400 150))))))

(defmethod make ((symbol list) &optional args)
  (mapcar #'(lambda (s) (make s args)) symbol))




