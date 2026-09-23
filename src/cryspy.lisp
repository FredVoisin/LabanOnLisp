(defun make-crispy-edit-view (data)
  (make-instance 'pull-down-menu ;'pop-up-menu
    :view-position #@(45 3)
    :view-size  #@(40 20)
    :menu-title ""
    :auto-update-default  t
    :item-display :selection
    :menu-items
    (loop for d in data
          collect
          (make-instance 'menu-item
            :menu-item-title "1"
            :menu-item-action #'(lambda ()(print d)))
          
          )))



(mapcar #'datum (car
(mapcar #'dimension
(body situation-1247))))

(make-instance 'situation)
(make-instance 'bodypart)

(inspect bodypart-1246)

(make-instance :support)


(mapcar #'class-name (find-subclasses 'dimension))