(in-package :lol)


(defmethod save ((self t) &optional file)
   (when (not file)
    (setf file (choose-file-dialog :button-string "Save")))
   (let ((name (name self)))
     (db::save-object (eval self) file
                      :variable (if (stringp name)
                                                   (read-from-string name nil)
                                                   name)
                      )
     ))


(defmethod save ((self situation) &optional file)
   (when (not file)
    (setf file (choose-file-dialog :button-string "Save")))
   (let ((name (name self)))
     (db::save-object (eval self) file :variable (if (stringp name)
                                                   (read-from-string name nil)
                                                   name))
     ))


#|
(new-situation)
(save (full-jam SITUATION))
(save SITUATION)


(defun read-situation (&optional file)
  (when (not file)
    (setf file (choose-file-dialog :button-string "read")))
  (let ((situation (make-instance 'situation)))
    (with-open-file (in-stream file :direction :input
                               :element-type 'character)
      (loop with length = (file-length in-stream)
            while (< (file-position in-stream) length)
            do
            (let ((line (read-line in-stream)))
              (print line)
              ;(print (read-from-string line t ))
              )))))


;(read-situation)

(defun load-situation (&rest args)
  (let ((sit (make-instance 'situation)))
    (mapcar #'(lambda (a) 
                (print (cadr a))
                (funcall (car a) (cadr a) sit)) args)
    sit))

(load-situation '(write-situation-name 'test))
(setf a (make-instance 'situation))
(funcall #'write-situation-name 'testa a)
(write-situation-name 'testa a)
a
|#



(defun list-files (pathname r &key resolve-aliases type)
  "Recursive list of files in a directory."
  (if (null pathname)
    (if type
      (remove-if-not #'(lambda (x) (equalp (mac-file-type x) type)) r)
      r)
    (if (atom pathname)
      (list-files (directory (make-pathname :directory (directory-namestring pathname) :name "*")
                             :files nil
                             :directories t
                             :resolve-aliases resolve-aliases)
                  (append (directory (make-pathname :directory (directory-namestring pathname) :name "*")
                                     :files t
                                     :directories nil
                                     :resolve-aliases resolve-aliases)
                          r))
      (if (listp (car pathname))
        (list-files (cdr pathname) (append (list (car pathname)) r))
        (list-files (append (directory (make-pathname :directory (directory-namestring (car pathname))
                                                      :name "*")
                                       :files nil
                                       :directories t
                                       :resolve-aliases resolve-aliases)
                            (cdr pathname))
                    (append (directory (make-pathname :directory (directory-namestring (car pathname)) :name "*")
                                       :files t
                                       :directories nil
                                       :resolve-aliases resolve-aliases)
                            r))
        ))))



(defun directory-list-files (directory &key resolve-aliases type)
  "Main function for recursive list of a directory."
  (when (null directory) (setf directory (choose-directory-dialog)))
  (list-files directory nil :resolve-aliases resolve-aliases :type type))


#|
(mac-namestring)
(directory-list-files nil :resolve-aliases t :type "LOL")
(list-files (choose-directory-dialog) nil :resolve-aliases t)
|#

           

      


