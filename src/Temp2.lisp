(defun make-bodypart-dim-util-view (situation)
  (remove 'nil
  (mapcar #'(lambda (x)
              (when (bodypart-p x)
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
                                                  ))
                          (make-instance 'menu-item
                            :menu-item-title "change elt"
                            :menu-item-action #'(lambda nil
                                                  (let ((new (first (select-item-from-list  (append (list 'bodypart)
                                                                                                    (mapcar #'class-name
                                                                                                            (find-subclasses 'BODYPART)))
                                                                                            :default-button-text "select"
                                                                                            :view-size (make-point 165
                                                                                                                   (min 680
                                                                                                                   (* 15 (length (find-subclasses 'BODYPART)))))
                                                                                            ;:selection-type :disjoint
                                                                                            :window-title "bodyparts")))
                                                        (window (front-window :class 'situation-window))
                                                        (pos (position x (body situation) :test #'equalp)))
                                                    
                                                    (replace (body situation) (list (eval `(lolreplace ,x ',new)))
                                                             :start1 pos :end1 (1+ pos)
                                                             :start2 0 :end2 1)
                                                    (window-close window)
                                                    (make-instance 'situation-window :situation situation))
                                                  ))))))
                  ; (set-part-color m :menubackground (color situation))
                  m)))
          (body situation))))