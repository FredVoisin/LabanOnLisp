(in-package :lol)

(require 'dynamic-views.lisp)



(setf *wind-sit-back-color* *white-color*)



(defmethod color ((self symbol))
  (color (eval self)))


(defun make-midi-send-button ()
  (make-dialog-item 'moving-button-view
                    #@(700 15)
                    #@(66 16)
                    "SEND"
                    'nil
                    :view-nick-name 'send-button
                    :default-button t))

(defclass situation-window (dynamic-views-dialog)
   ((situation :initarg :situation :initform nil :accessor situation)
    )
   (:default-initargs
     :window-type :document
     :view-size #@(240 100)
     ;:back-color *wind-sit-back-color*
     :view-position #@(50 110)
     :color-p t
     :window-show nil
     )
   (:documentation "Class of data fiels"))

(defmethod initialize-instance :after ((self situation-window)
                                         &key situation)
   (when (not situation) (setf situation (make-instance 'situation)))
   (setf (situation self) situation)
   (set-view-nick-name self (name situation))
   (set-window-title self (name situation))
   ;(set-back-color self (color situation))
   (set-part-color self :title-bar (color situation))
   (instal-views-in-situation self)
   )



#|
(defmethod view-draw-contents ((self situation-window))
   ;
   ;(set-part-color self :frame (color (situation self)))
   self
   )
|#


(defun instal-views-in-situation (situation-window)
  (if (not (body (situation situation-window)))
    (apply #'add-subviews (append (list situation-window)
                                  (make-situation-menubar situation-window)))
    
    (let* ((situation (update-dimension-list (situation situation-window)))
           (situation-data-views (make-situation-data-name-views situation))
           (dimensions-views (make-dimension-name-views situation))
           (bodypart-views (make-bodypart-name-views situation))
           (data-views (make-all-data-views situation-window))
           (dim-button (make-bodypart-dim-util-view situation))
           )
      (apply #'add-subviews (append (list situation-window)
                                    (make-situation-menubar situation-window)
                                    situation-data-views
                                    dimensions-views
                                    bodypart-views
                                    data-views
                                    dim-button
                                    (list (make-midi-send-button))
                                    ))
      (update-dimension-situation-views situation-window)))
  (set-part-color situation-window :frame (color (situation situation-window)))
  (set-view-position situation-window 50 110)
  (window-select situation-window))


(defmethod remove-elt-subviews (situation-window &rest elt-name)
  (apply #'remove-subviews (append (list situation-window)
                                   (mapcar #'(lambda (x) (view-named (read-from-string x) situation-window))
                                           elt-name))))


(defmethod remove-dim-text-subviews (situation-window &rest dim-name)
  (apply #'remove-subviews (append (list situation-window)
                                   (mapcar #'(lambda (x) (view-named x situation-window))
                                           dim-name))))
                     


(defmethod make-data-view ((data dimension) (h integer) (v integer) &optional (size #@(40 15)))
   (let ((type (type data))
         views)
     (case type
       (:continuous (push (make-instance  'editable-text-dialog-item
                            :view-position (make-point h v)
                            :view-size size
                            :view-nick-name 'data
                            :dialog-item-text (format nil "~S" (datum data))
                            :view-font '("Geneva" 12 :BOLD))
                          views))
       (:crispy 
        (let ((menu (make-instance 'pull-down-menu
                      :view-position (make-point h v)
                      :view-size  #@(40 20)
                      :menu-title ""
                      :auto-update-default  t
                      :item-display :selection
                      :default-item (let ((pos (position (datum data) (values-set data) :test #'equalp)))
                                      (if pos (values (1+ pos)) 1) )
                      :menu-items
                      (mapcar #'(lambda (d)
                                  (let ((m (make-instance 'menu-item
                                             :menu-item-title (format nil "~S" d)
                                             :menu-item-action #'(lambda ()
                                                                   (setf (slot-value data 'datum) d)
                                                                   ;(set-view-font (view-container m) '("Geneva" 10 :SRCCOPY :BOLD (:COLOR-INDEX 0)))
                                                                   ))))
                                    (set-menu-item-update-function m #'(lambda (m)
                                                                         (set-menu-item-style m :bold)))
                                    m))
                              (values-set data)))))
          (set-view-font menu '("Geneva" 10 :SRCCOPY :PLAIN (:COLOR-INDEX 0)))
          (push menu views))))
     views))

(defclass dimension-edit-view (view)
   ((data :initarg :data :initform nil :accessor data)
    (bodypart :initarg :bodypart :initform nil :accessor bodypart)
    )
   (:default-initargs
     :view-size #@(50 40)
     ;:back-color *wind-sit-back-color*
     )
   (:documentation "Class of data fiels"))

(defmethod initialize-instance :after ((self dimension-edit-view)
                                         &key data)
   (when (not data) (setf (slot-value self 'data) (make-instance 'dimension)
                          data (data self)))
   (let ((type (type data))
         ;(mode (mode data))
         ;(vset (values-set data))
         (free (free data))
         (data-view (make-data-view data 0 0))
         )
     
     (set-view-nick-name self (class-name (class-of data)))
     (apply #'add-subviews (append (list self 
                                         (make-dialog-item 'check-box-dialog-item
                                                           (make-point -2 (if (eq :continuous type) 17 20))
                                                           #@(40 12)
                                                           "free"
                                                           'nil
                                                           :dialog-item-action #'(lambda (self)
                                                                                   (setf (free data)
                                                                                         (check-box-checked-p self)))
                                                           :check-box-checked-p free
                                                           :view-font '("Monaco" 8)))
                                   data-view))
     
     ))

#|

(defclass general-edit-view (view)
   ((data :initarg :data :initform nil :accessor data))
   (:default-initargs
     :view-size #@(50 40)
     ;:back-color *wind-sit-back-color*
     )
   (:documentation "Class of general data fiels describing situation."))

(defmethod initialize-instance :after ((self general-edit-view)
                                         &key data)
   (when data
     (let ((type (type data))
           ;(mode (mode data))
           ;(vset (values-set data))
           (free (free data))
           (data-view (make-data-view data 0 0))
           )
       (set-view-nick-name self (class-name (class-of data)))
       (apply #'add-subviews (append (list self 
                                           (make-dialog-item 'check-box-dialog-item
                                                             (make-point -2 (if (eq :continuous type) 17 20))
                                                             #@(40 12)
                                                             "free"
                                                             'nil
                                                             :dialog-item-action #'(lambda (self)
                                                                                     (setf (free data)
                                                                                           (check-box-checked-p self)))
                                                             :check-box-checked-p free
                                                             :view-font '("Monaco" 8)))
                                     data-view)) )))

|#

(defun make-all-data-views (view)
  (let* ((situation (situation view))
         (body (if situation (body situation) nil))
         (data (if situation (data situation) nil))
         bodyviews dataviews
         )
    
    (when body
      (setf bodyviews
            (apply #'append
                   (loop for b in body
                         collect (mapcar #'(lambda (dim)
                                             (make-instance 'dimension-edit-view
                                               :data dim
                                               :bodypart (if (stringp (name b))
                                                           (read-from-string (name b) nil)
                                                           (name b))))
                                         (dimension b))))))
    (when data
      (setf dataviews
            (loop for d in data
                  collect (make-instance 'dimension-edit-view
                            :data d
                            :bodypart nil))))
                                       
    (append dataviews bodyviews)))

;;; MENU BAR FOR SITUATION WINDOW

(defmethod record-edited-data ((self situation-window))
   (loop for dim in (append *LABAN-DIMENSIONS* (other-dimensions (eval (situation self))))
         do
         (when dim
           (setf (slot-value (eval (situation self)) (symbol dim))
                 (mapcar #'(lambda (d) (read-from-string (dialog-item-text d) nil nil))
                         (find-view self dim)))))
   (values))

(defun make-GENERAL-situation-menu (view)
  (let* ((dansersclasses (append (list (find-class 'danser)) (find-subclasses 'danser)))
         (dansers (if dansersclasses (mapcar #'class-name dansersclasses) '(danser))))
    (make-instance 'pull-down-menu
      :item-display "General"
      :view-position #@(1 1)
      :menu-items
      (list (make-instance 'pop-up-menu
              :menu-title "danser"
              :auto-update-default t
              :item-display :selection
              :default-item (let ((pos (position (class-name (class-of (danser (situation view)))) dansers :test #'equal)))
                              (if pos (values (1+ pos)) 1) )
              :menu-items (mapcar #'(lambda (d)
                                      (let ((m (make-instance 'menu-item
                                                 :menu-item-title (string d)
                                                 :menu-item-action #'(lambda ()
                                                                       (setf (slot-value (situation view) 'danser)
                                                                             (make-instance d))))))
                                        m)) dansers))
            
            (make-instance 'menu-item
              :menu-item-title "describe"
              :menu-item-action #'(lambda ()
                                    (print 'mom!)))
            (make-instance 'menu-item
              :menu-item-title "copy"
              :menu-item-action #'(lambda ()
                                    (let ((copy-of (copy (situation view))))
                                      (make-instance 'situation-window :situation copy-of))
                                    ))
            (make-instance 'menu-item
              :menu-item-title "save"
              :menu-item-action #'(lambda ()
                                    (save (situation view))
                                    ))
            (make-instance 'menu-item
              :menu-item-title "rename"
              :menu-item-action #'(lambda ()
                                    (let ((new-name (read-from-string (get-string-from-user  "Rename situation" :window-title "Rename"
                                                                                             :initial-string (name (situation view))))))
                                      (rename (situation view) new-name)
                                      (window-close view)
                                      (make-instance 'situation-window :situation new-name))))
            (make-instance 'menu-item
              :menu-item-title "color"
              :menu-item-action #'(lambda ()
                                    (let ((color (USER-PICK-COLOR :color (color (situation view)))))
                                      ;(SET-BACK-COLOR view color)
                                      (set-part-color view :title-bar color)
                                      (setf (color (situation view)) color)
                                      (view-draw-contents view)
                                      )))))))

(defun make-BODY-situation-menu (view)
  (make-instance 'pull-down-menu
    :item-display "Body"
    :view-position #@(60 1)
    :menu-items
    (list (make-instance 'menu-item
            :menu-item-title "add part"
            :menu-item-action #'(lambda ()
                                  (let ((news (make-body-part-instance)))
                                    (eval `(lol-add (situation ,view) ,.news)))
                                  (update-dimension-situation-views view)
                                  (instal-views-in-situation view)))
          (make-instance 'menu-item
            :menu-item-title "remove part"
            :menu-item-action #'(lambda ()
                                  (let* ((names (select-item-from-list (mapcar #'(lambda (x)
                                                                                   (name x))
                                                                               (body (situation view)))))
                                         (parts (mapcar #'(lambda (name)
                                                            (find-if #'(lambda (x)
                                                                         (eq (name x) name))
                                                                     (body (situation view))))
                                                        names)))
                                    (eval `(lol-remove (situation ,view) ,.parts))
                                    ;(remove-elt-subviews view names)
                                    (window-close view)
                                    (make-instance 'situation-window :situation (situation view))
                                    ))))))


(defun make-DIMENSION-situation-menu (window)
  (let ((situation (situation window)))
    (make-instance 'pull-down-menu
      :item-display "Dimensions"
      :view-position #@(120 1)
      :menu-items
      (list (make-instance 'menu-item
              :menu-item-title "add dimension"
              :menu-item-action #'(lambda ()
                                    (let ((dimensions-to-add
                                           (mapcar #'find-class
                                                   (select-item-from-list  (mapcar #'class-name (find-subclasses 'dimension))
                                                                           :selection-type :disjoint))
                                                     ))
                                      (apply #'lol-add (append (list situation) dimensions-to-add))
                                      (window-close window)
                                      (make-instance 'situation-window :situation situation))))
            (make-instance 'menu-item
              :menu-item-title "remove dimension"
              :menu-item-action #'(lambda ()
                                    (let ((dimensions-to-remove
                                           (mapcar #'find-class
                                                   (select-item-from-list (dimension-list situation)
                                                                          :selection-type :disjoint))))
                                      (apply #'lol-remove (append (list situation) dimensions-to-remove))
                                      (window-close window)
                                      (make-instance 'situation-window :situation situation))
                                    ))))))

(defclass situation-menubar (inspector::bottom-line-mixin view) ())

(defmethod view-default-size ((view situation-menubar))
  (let ((container (view-container view)))        
    (when container       
      (make-point (point-h (view-size container))
                  (+ (ccl::view-font-line-height view) 4)))))

(defmethod install-view-in-window ((view situation-menubar) w)
  (declare (ignore w))
  (multiple-value-bind (ff ms)(view-font-codes view)
    (let ((container (view-container view)))
      (when ff
        (do-subviews (sub view)
          (set-view-font-codes sub ff ms)))
      ; sets slots
      (ccl::set-default-size-and-position view container)
      (call-next-method)
      (invalidate-view view))))

(defun make-situation-menubar (view)
  (list (make-instance 'situation-menubar
          :view-font '("geneva" 10)  ; try this
          :view-subviews
          (list 
           (make-GENERAL-situation-menu view)
           (make-BODY-situation-menu view)
           (make-DIMENSION-situation-menu view)))))


(defclass h-dynamic-views-dialog (color-dialog drag-view-mixin)
  ((v-position :initarg :v-position :initform 10 :accessor v-position :type integer)
   )
  (:default-initargs
    :drag-allow-copy-p nil
    :drag-allow-move-p t
    :drag-auto-scroll-p nil
    :drag-accepted-flavor-list :|view|))


;moving-static-text-view

(defclass moving-dimension-text-view (moving-static-text-view)
  ())


(defun make-dimension-name-views (situation)
  (let* ((dimensions (remove nil (if (dimension-list situation)
                                   (dimension-list situation)
                                   (remove-duplicates (apply #'append (mapcar #'dimension-list (body situation))) ;)))
                                                      :key 'class-of))))
         )
    (mapcar #'(lambda (x)
                
                (make-dialog-item 'moving-dimension-text-view
                                  (make-point (+ 115 (* 67 (position x dimensions)) ) 35)
                                  (make-point (+ 10 (string-width (string x) '("Monaco" 10 :bold)))
                                              (font-line-height '("Monaco" 10 :bold))) ;#@(73 29)
                                  (string x)
                                  'nil
                                  :view-nick-name x
                                  :view-font '("Monaco" 10 :bold)))
            (dimension-list situation))))


(defun make-dimension-name-views2 (window situation dimensions part-name part)
  (apply #'append
         (mapcar #'(lambda (x)
                     (list (make-dialog-item  'moving-dimension-text-view
                                              (make-point (+ 115 (* 67 (position (class-name (class-of x))
                                                                                 (dimension-list situation) :test #'eq)))
                                                          65)
                                              (make-point (+ 10 (string-width (string (class-name (class-of x))) '("Monaco" 10)))
                                                          (font-line-height '("Monaco" 10))) ;#@(73 29)
                                              (string (class-name (class-of x)))
                                              'nil
                                              :view-nick-name (class-name (class-of x))
                                              :view-font '("Monaco" 10))
                           (make-instance 'dimension-edit-view
                             :view-position (make-point (+ 140 (* 70 (length (dimension-list situation))))
                                                        (point-v (view-position (view-named (read-from-string (name part)) window))))
                             :data x
                             :bodypart (if (stringp part-name)
                                         (read-from-string part-name nil)
                                         part-name))))
                 dimensions )))





;(setq a (make-instance 'situation))
;(setq b (make-instance 'bodypart))
;(setq c (make-instance 'bodypart))
;(lol-add b (make-instance 'dimension))
;(lol-add c (make-instance 'dimension))
;(lol-add a b)
;(lol-add a c)
;(make-dimension-name-views a)
;(inspect a)


(defclass moving-bodypart-text-view (moving-static-text-view)
  ())


(defun make-bodypart-name-views (situation)
  (mapcar #'(lambda (x)
              (let ((class-name (string (class-name (class-of x))))
                    (name (name x)))
                (make-dialog-item 'moving-bodypart-text-view
                                  (make-point 10 (+ 130 (* (+ 30 (font-line-height '("Monaco" 10 :bold)))
                                                           (get-position situation x))))
                                  (make-point (+ 10 (string-width name '("Monaco" 10 :bold)))
                                              (font-line-height '("Monaco" 10 :bold)))  ;#@(73 29)
                                  class-name
                                  'nil
                                  :view-nick-name (read-from-string name)
                                  :view-font '("Monaco" 10 :bold)
                                  )))
          (body situation)))


;(make-bodypart-name-views a)

(defclass moving-menu-item-view (pull-down-menu moving-view-mixin)
  ((bodypart :initarg :bodypart :initform nil :accessor bodypart :type symbol))
  (:default-initargs
    :view-size  (make-point 55 (+ 4 (font-line-height '("Monaco" 8))))
    :menu-title ""
    :auto-update-default  t
    :item-display :selection
    :view-font '("Monaco" 8)
    ))


(defun make-bodypart-dim-util-view (situation)
  (mapcar #'(lambda (x)
              (let ((m 
                     (make-instance 'moving-menu-item-view
                       :view-position (make-point 10 (+ 73 (* (+ 20 (font-line-height '("Monaco" 9)))
                                                              (get-position situation x))))
                       :view-nick-name (read-from-string (name x))
                       :view-font '("Monaco" 9)
                       :bodypart (name x)
                       :menu-items
                       (list
                        (make-instance 'menu-item
                          :menu-item-title " < > ")
                        (make-instance 'menu-item
                          :menu-item-title "add dim"
                          :menu-item-action #'(lambda nil
                                                (let ((dims (make-dimension-instance)))
                                                  (eval `(lol-add ,x ,.dims))
                                                  (update-dimension-list situation)
                                                  (window-close (front-window :class 'situation-window))
                                                  (make-instance 'situation-window :situation situation))))
                        (make-instance 'menu-item
                          :menu-item-title "rem dim"
                          :menu-item-action #'(lambda nil
                                                (let ((dims (select-item-from-list
                                                             (mapcar #'(lambda (x) (class-name (class-of x)))
                                                                     (dimension-list x))
                                                             :SELECTION-TYPE :disjoint))
                                                      (window (front-window :class 'situation-window)))
                                                  (setf dims (mapcar #'(lambda (a)
                                                                                (find-if #'(lambda (n) (eq a (class-name (class-of n))))
                                                                                         (dimension-list x)))
                                                                            dims))
                                                  (eval `(lol-remove ,x ,.dims))
                                                  (window-close window)
                                                  (make-instance 'situation-window :situation situation))
                                                ))))))
                (set-part-color m :menubackground (color situation))
                m))
          (body situation)))


(defclass moving-button-dialog-item-view (ccl::button-dialog-item
                                          moving-view-mixin)
  ())



(defclass moving-situation-data-text-view (moving-static-text-view)
  ())


(defun make-situation-data-name-views (situation)
  (let ((data (data situation)))
    (mapcar #'(lambda (x)
                (let ((class-name (string (class-name (class-of x))))
                      (name (name x)))
                  (make-dialog-item 'moving-situation-data-text-view
                                    (make-point (+ 250 (* 100 (position x data :test #'equalp)))
                                                30)
                                    (make-point (+ 10 (string-width (string name) '("Monaco" 10 :bold)))
                                                (font-line-height '("Monaco" 10 :bold)))  ;#@(73 29)
                                    class-name
                                    'nil
                                    :view-nick-name name
                                    :view-font '("Monaco" 10 :bold)
                                    )))
            data)))



(defmethod update-dimension-situation-views ((view situation-window))
  (let ((situation (situation view)))
    ;(update-dimension-list situation)
    (when (dimension-list situation)
      (loop for v in (coerce (view-subviews view) 'list)
            do
            
            (cond ((eq (find-class 'moving-situation-data-text-view) (class-of v))
                   (set-view-position v (+ 150 (* 140 (position (view-nick-name v)
                                                                     (mapcar #'name (data situation)))))
                                      30))
                  ((eq (find-class 'moving-dimension-text-view) (class-of v))
                   (set-view-position v (+ 115 (* 67 (position (view-nick-name v)
                                                               (dimension-list situation))))
                                      65))
                  ((eq (find-class 'moving-bodypart-text-view) (class-of v))
                   (set-view-position v
                                      10
                                      (+ 83 (* (+ 20 (font-line-height '("Monaco" 10 :bold)))
                                               (position (view-nick-name v)
                                                         (mapcar #'(lambda (x) (read-from-string (name x)))
                                                                 (body situation)))
                                               ))))
                  ((eq (find-class 'moving-menu-item-view) (class-of v))
                   (set-view-position v
                                      0
                                      (+ 95 (* (+ 20 (font-line-height '("Monaco" 10 :bold)))
                                               (position (view-nick-name v)
                                                         (mapcar #'(lambda (x) (read-from-string (name x)))
                                                                 (body situation)))
                                               ))))
                  (t (values))))
      (loop for v in (coerce (view-subviews view) 'list)
            do
            (if (eq  (find-class 'dimension-edit-view) (class-of v))
              (let ((body (bodypart v)))
                (if body
                  (set-view-position v 
                                     (point-h (view-position (view-named (view-nick-name v) view)))
                                     (- (point-v (view-position (view-named (bodypart v) view))) 4))
                  (set-view-position v 
                                     (+ (point-h (view-position (view-named (view-nick-name v) view))) 
                                        (string-width (string (view-nick-name v)) '("Monaco" 10 :bold)) 15)
                                     19)))
              (values)))
      (set-view-size view
                     (+ 115 50 (* 67 (length (dimension-list situation)) ))
                     (+ 85 (* 35 (length (body situation)))))
      (set-view-position (view-named 'send-button view) (make-point (- (point-h (view-size view)) 75) 8 ))
      )))


(defmethod drag-receive-dropped-flavor ((view situation-window)
                                          (flavor (eql :|view|))
                                          (data-ptr t) (data-size integer)
                                          (item-reference integer))
   (declare (ignore data-size item-reference))
   (let* ((drag-pos (drag-mouse-original-position view))
          (drop-pos (drag-mouse-drop-position view))
          (old-view-pos (view-position data-ptr))
          (offset (subtract-points drag-pos old-view-pos))
          (new-pos (subtract-points drop-pos offset))
          (situation (situation view)))
     (when (symbolp situation)
       (setf situation (eval situation)))
     (cond ((eq (find-class 'moving-dimension-text-view) (class-of data-ptr))
            (set-view-position data-ptr (make-point new-pos))
            (let ((new-order (order
                              (mapcar #'(lambda (x)
                                          (point-h (view-position (view-named x view))))
                                      (dimension-list situation)))))
              (setf (slot-value situation 'dimension-list)
                    (reorder (dimension-list situation) new-order))))
           ((eq (find-class 'moving-situation-data-text-view) (class-of data-ptr))
            (set-view-position data-ptr (make-point new-pos))
            (let ((new-order (order
                              (mapcar #'(lambda (x)
                                          (point-h (view-position (view-named x view))))
                                      (mapcar #'name (data situation))))))
              (setf (slot-value situation 'data)
                    (reorder (data situation) new-order))))
           
           ((eq (find-class 'moving-bodypart-text-view) (class-of data-ptr))
            (set-view-position data-ptr (make-point new-pos))
            (let ((new-order (order
                              (mapcar #'(lambda (x)
                                          (point-v (view-position (view-named (read-from-string x) view)))) 
                                      (mapcar #'name (body situation))))))
              (setf (slot-value situation 'body)
                    (reorder (body situation) new-order))))
           (t (values)))
     (update-dimension-situation-views view)
     )
   (view-draw-contents view)
   t)


(defmethod view-draw-contents ((self situation-window))
   (call-next-method)
   )



(defmethod show-situation-window ((situation situation))
  (make-instance 'situation-window
    :window-type :document-with-grow
    :situation situation))



(defmethod show-situation-window ((situation list))
  (loop for s in situation
        do
        (make-instance 'situation-window
          :window-type :document-with-grow
          :situation s)))


(defmethod show-situation-window ((situation symbol))
  (show-situation-window (eval situation)))


#|

(setq a (make-instance 'situation))
(setq b (make-instance 'bodypart))
(lol-add a b)
(setq c (make-instance 'dimension))
(inspect c)
(lol-add b c)

(inspect
(make-instance 'situation-window
  :window-type :document-with-grow
  :situation a)
(inspect a)
(length (body a))
(setf (dimension-list a) nil)
(setq z (make-instance 'bodypart :name "pied"))
(lol-add a z)

(setq r (make-instance 'situation))
(lol-add r z b)
(inspect r)
(make-instance 'situation-window
  :window-type :document-with-grow
  :situation a)


(setf (body r) (remove-if #'dimension-p (body r)))
(setf (body r) (remove-duplicates (body r) :test #'equalp))

|#





;(DYNAMIC-VIEWS)
