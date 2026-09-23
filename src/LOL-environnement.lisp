(in-package :lol)

(make-package :DATABASE :nicknames '(db) :use '(common-lisp))
(in-package :database)
(shadowing-import '(setf documentation) :database)
(use-package :ccl)





(in-package :lol)

;(load (make-pathname :directory lol::*lol-dir* :name "DEFAULT-ENV.LOL"))






(defmethod create-environnement-file (&optional (name 'default-env) (directory *lol-dir*))
  (let ((environnement-file  (make-pathname :directory directory
                                           :name (if (stringp name) name (string name))
                                           :type "LOL")))
    
    (create-file environnement-file
                 :if-exists :error
                 :mac-file-type "TEXT"
                 :mac-file-creator "CCL2")
    (with-open-file (env-stream environnement-file
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
      (format env-stream (format nil
                                 "~%(setf *LOL-ENVIRONNEMENT* '~S)" name)))
    (format t "~%LOL> Environnement file for ~S created.~%" name)
    (values)
    ))


;(create-environnement-file)

(defmethod load-environnement-file (&optional (name "DEFAULT-ENV") (directory *lol-dir*))
  (let (environnement-file)
    (cond ((and directory (not name))
           (setf name (choose-file-dialog 
                       :directory directory
                       :button-string "choose"
                       :mac-file-type "LOL ")))
          ((and (not directory) name)
           (setf directory (choose-directory-dialog
                            :directory *lol-dir*
                            :window-title "choose a directory"
                            :prompt "Choose the directory where your environement file .LOL should be")))
          ((and (not directory) (not name))
           (setf directory (choose-directory-dialog
                            :directory *lol-dir*
                            :window-title "choose a directory"
                            :prompt "Choose the directory where your environement file .LOL should be"))
           (setf name (choose-file-dialog 
                       :directory directory
                       :button-string "choose"
                       :mac-file-type "TEXT")))
          (t
           (setf environnement-file (make-pathname :directory directory
                                                   :name name
                                                   :type "LOL"
                                                   ))))
    (cond ((probe-file environnement-file)
           (format t "~%LOL> Loading environnement ~S ..." name)
           (load  (probe-file environnement-file)
                  :verbose t))
          (t (format t "~%LOL> WARNING : No environnement file for ~S. Loading aborted." name)
             (values)))))

;(load-environnement-file)


(defmethod save-to-environnement ((self t))
  )





