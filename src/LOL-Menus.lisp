(in-package :lol)

;;;*****************************
;;;*********** MENUS ***********
;;;*****************************


#|
;;(menu-items *file-menu*)
;(find-menu-item *windows-menu*  "Listener")
;(inspect *file-menu*)

(set-command-key (elt (menu-items *file-menu*)  15) nil)



|#

;;*** LOL MENU ****
(setf *LOL-menu* (make-instance 'menu
                   :menu-title "   LOL   "
                   ))

;(setf *LOL-menu* nil)

;;; MENU PART MENUS

(defvar *HELP-menu* (make-instance 'menu-item
                      :menu-item-title "LOL help"
                      :menu-item-action #'(lambda nil (print 'help))))

;;; SITUATIONS PART MENUS

(setf *new-situation-menu* (make-instance 'menu-item
                               :menu-item-title "new"
                               :command-key #\j
                               :menu-item-action #'(lambda nil
                                                     (let ((sit (make-instance 'situation)))
                                                       (make-instance 'situation-window)
                                                         :situation sit))))

(setf *show-situation-menu* (make-instance 'menu-item
                                :menu-item-title "select"
                                :command-key #\k
                                :menu-item-action #'(lambda nil
                                                      (let* ((sits (search-situations))
                                                             (selected
                                                              (select-item-from-list sits
                                                                                     :default-button-text "select"
                                                                                     :selection-type :disjoint
                                                                                     :window-title "show situations"
                                                                                     :view-size (make-point
                                                                                                 200
                                                                                                 (min 600 (* 14 (length sits))))
                                                                                     )))
                                                        (loop for s in selected
                                                              do
                                                              (make-instance 'situation-window
                                                                :situation (eval s)))))))

(setf *delete-situation-menu* (make-instance 'menu-item
                                :menu-item-title "delete"
                                :menu-item-action #'(lambda nil
                                                      (let ((selected (select-item-from-list  (search-situations)
                                                                                              :default-button-text "select"
                                                                                              :selection-type :disjoint
                                                                                              :window-title "Delete selected situations")))
                                                        (if (= 1 (length selected))
                                                          (delete-situation (eval (car selected)))
                                                          (mapcar #'(lambda (x)
                                                                      (delete-situation (eval x)))
                                                                  selected))))))

(setf *save-all-situations* (make-instance 'menu-item
                              :menu-item-title "save all..."
                              :command-key  nil
                              :menu-item-action #'(lambda nil
                                                    (let ((file (choose-new-file-dialog :button-string "Save all" :prompt "Save all situations in a new file")))
                                                      (mapcar #'(lambda (x)
                                                                  (save (eval x) file))
                                                              (search-situations))))))

(setf *SITUATION-menu* (make-instance 'menu
                         :menu-title "SITUATIONS"
                         :menu-items (list *new-situation-menu*
                                           *show-situation-menu*
                                           *delete-situation-menu*
                                           *save-all-situations*
                                           )))

;;; BODY PART MENUS

(setf *new-body-part-menu* (make-instance 'menu-item
                                 :menu-item-title "edit"
                                 :menu-item-action #'(lambda ()
                                                       (define-window 'bodypart))))



(setf *body-part-menu* (make-instance 'menu-item
                           :menu-item-title "part list"
                           :command-key #\p
                           :menu-item-action #'(lambda nil
                                                 (let ((selected (select-item-from-list  (append (list 'bodypart)
                                                                                                 (mapcar #'class-name
                                                                                                         (find-subclasses 'BODYPART)))
                                                                                         :default-button-text "select"
                                                                                         :selection-type :disjoint
                                                                                         :window-title "bodyparts")))
                                                   (if (= 1 (length selected))
                                                     (window-select (window (eval (car selected))))
                                                     (mapcar #'(lambda (x)
                                                                 (window-select (window (eval x))))
                                                             selected))))))

(setf *BODYPART-menu* (make-instance 'menu
                         :menu-item-title "Parts"
                         :menu-items (list *new-body-part-menu*
                                           *body-part-menu*
                                           ;*delete-body-part-menu*
                                           )))

(setf *separation* (make-instance 'menu
                         :menu-item-title "-"))

;;; DIMENSIONS PART MENUS

(setf *new-dimension-menu* (make-instance 'menu-item
                               :menu-item-title "edit"
                               :menu-item-action #'(lambda nil
                                                       (define-window 'dimension))))

(setf *DIMENSION-menu* (make-instance 'menu
                     :menu-item-title "Dimensions"
                     :menu-items (list *new-dimension-menu*)))



;;; DANSER MENUS

(setf *new-danser-menu* (make-instance 'menu-item
                               :menu-item-title "danser..."
                               :menu-item-action #'(lambda nil
                                                       (define-window 'danser))))

(setf *DANSER-menu* (make-instance 'menu
                     :menu-item-title "danser"
                     :menu-items (list *new-danser-menu*)))



;;; UTILS MENUS



(defvar *search-menu* (make-instance 'menu-item
                      :menu-item-title "Search"
                      :menu-item-action #'(lambda ()
                                            (let ((name (read-from-string (get-string-from-user "Search for name..."))))
                                              (print (search-name name))))))
(defvar *UTILITIES-menu* (make-instance 'menu
                          :menu-item-title "Utilities"
                          :menu-items (list *search-menu*)))


(add-menu-items *LOL-menu* *DANSER-menu*
                *SITUATION-menu* *BODYPART-menu* *DIMENSION-menu*
                *separation* *UTILITIES-menu* *HELP-menu* )

(menu-install *LOL-menu*)
;(menu-deinstall *LOL-menu*)
;(menu-deinstall *HELP-menu*)





;(menu-deinstall *LOL-menu*)
;(setf *LOL-menu* nil)



;;;; MENU ENVIRONNEMENT


(defvar *LOL-environnement-menu* (make-instance 'menu
                   :menu-title "   LOL-env   "
                   ))

#|

(setf *new-env-menu* (make-instance 'menu-item
                               :menu-item-title "New environnement"
                               :menu-item-action #'(lambda nil
                                                     (let ((anv-name (get))
                                                       (make-instance 'situation-window)
                                                         :situation sit))))




|#



