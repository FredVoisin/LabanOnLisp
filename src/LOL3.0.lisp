;*********************************************************************************
;*                                       LOL                                     *
;*                                  LABAN ON LISP                                *
;*                                                                               *
;*         COMPUTER ASSISTED CHOREGRAPHIC COMPOSITION ENVIRONMENT                *
;*                                                                               *
;*                                Frédéric VOISIN                                *
;*                   for Myriam Gourfink and Association LOL (Paris)             *
;*                                    1999-2001                                      *
;*                                                                               *
;*                                                                               *
;*********************************************************************************
;;; Copyright (c) 1999-2026 Frédéric Voisin
;;; Licensed under the PolyForm Noncommercial License 1.0.0
;;; <https://polyformproject.org/licenses/noncommercial/1.0.0> (see LICENSE)
;;; Required Notice: Copyright (c) 1999-2026 Frédéric Voisin (https://fredvoisin.com)
;;; **********************************************************************



;requires Common Lisp, Common Lisp Object System, Macintosh Common Lisp
;Quickdraw and QuickTime on Macintosh computers


;;; *********************************************************
;;; ********************  DEF PACKAGE *******************
;;; *********************************************************


(defpackage :LOL)

(in-package :LOL)



;;; *********************************************************
;;; ********************  LOAD CODE FILES *******************
;;; *********************************************************


;; LOL code directory

(defvar *lol-dir* 'nil)
(if (not *lol-dir*)
  (setf *lol-dir* (directory-namestring (choose-directory-dialog)))
  (format t "Using directory ~S~%" *lol-dir*))
(when (not (member *lol-dir* *module-search-path*))
  (push *lol-dir* *module-search-path*))

;; files

(require 'LOL-ENVIRONNEMENT.lisp)
(require 'save-object.lisp)
(require 'LOL-Classes.lisp )
(require 'LOL-Tools.lisp )
(require 'LOL-maths.lisp )
(require 'fuzz2.0.lisp)
(require 'LOL-default.lisp)
(require 'LOL-define.lisp)
(require 'LOL-windows.lisp )
(require 'LOL-combinatoire.lisp)
(require 'LOL-edit.lisp)

(require 'LOL-IO.lisp)
(require 'LOL-Menus.lisp )

;; for natural language processing
;(require 'Fredgram.lisp )
;(require 'lolgram.lisp)


;; for Laban notation & representation
(require 'Laban.lisp)


;;; ***************  KERNEL LOL CODE BEGINS HERE ************



(defun make-new-symbol (name &optional content &key key)
  (let ((sym (intern (string (if (boundp (read-from-string (string name)))
                               (gensym (format nil "~S-" name))
                               name)))))
    (setf (symbol-value sym) content)
    sym
    ))

#|
(MAKE-NEW-SYMBOL 'SITUATION-6250-7230)
|#


(defgeneric subclass (symbol class)
   (:documentation
   "SUBCLASS is a LOL generic function.
Tests if <symbol> is a subclass of <class>.
<symbol> and <class> must be both symbols."))

(defmethod subclass ((symbol t) (class t))
  (lol-error-msg "subclass? tests if <symbol> is a subclass of <class>.
<symbol> and <class> must be both symbols."))

(defmethod subclass ((symbol symbol) (class symbol))
   (let ((class-found (find-class symbol nil)))
     (if class-found
       (if (member class
                   (mapcar #'class-name 
                           (CLASS-PRECEDENCE-LIST (find-class symbol nil))))
         t nil)
       (lol-error-msg (format nil
                              "Error : class ~S does not exist."
                              symbol)))))



(defun search-situations (&optional name &key test)
  (let ((situations '()))
    (if test
      (do-symbols (sym (find-package 'lol))
        (when (boundp sym)
          (when (and (equalp (type-of (eval sym)) 'situation)
                     (funcall test (eval sym)))
            (push sym situations))))
      (do-symbols (sym (find-package 'lol))
        (when (boundp sym)
          (when (equalp (type-of (eval sym)) 'situation)
            (push sym situations)))))
    (if name
      (let ((n (characters (string name))))
        (loop for i from 0 to (1- (length situations))
              do
              (if (seq-member n (characters (string (nth i situations))))
                (setf (nth i situations) (list 0 (nth i situations)))
                (setf (nth i situations) (list (editing-distance
                                                (characters (string (nth i situations)))
                                                n 
                                                1.2 1 :test #'char-equal :scale t :fact 0)
                                               (nth i situations)))))
        (sort situations '< :key #'first))
      (sort situations
            #'string-lessp ))))

;(search-situations 'zozo)


(defun lolsearch (type &optional name &key test)
  (let ((instances '()))
    (if test
      (do-symbols (sym (find-package 'lol))
        (when (boundp sym)
          (when (and (equalp (type-of (eval sym)) type)
                     (funcall test (eval sym)))
            (push sym instances))))
      (do-symbols (sym (find-package 'lol))
        (when (boundp sym)
          (when (equalp (type-of (eval sym)) type)
            (push sym instances)))))
    (if name
      (let ((n (characters (string name))))
        (loop for i from 0 to (1- (length instances))
              do
              (if (seq-member n (characters (string (nth i instances))))
                (setf (nth i instances) (list 0 (nth i instances)))
                (setf (nth i instances) (list (editing-distance
                                               (characters (string (nth i instances)))
                                               n 
                                               1.2 1 :test #'char-equal :scale t :fact 0)
                                              (nth i instances)))))
        (sort instances '< :key #'first))
      (sort instances
            #'string-lessp ))))

;(lolsearch 'situation)
;(lolsearch 'bodypart)
;(lolsearch 'bras)
;(lolsearch 'support)


;;; *********************************************************
;;; ******************** HELP functions *********************
;;; *********************************************************


(defun help (&rest subject)
  (helper subject))


(defmethod helper ((object t))
  (cond ((boundp object)
         (format t "~%~S~ :%" object)
         (documentation object))
        (t
         (cond ((eq 'functions object)
                (mapcar #'(lambda (f) (format t "~%~S" f)) (find-functions))
                (values))
               ((eq 'classes object)
                (mapcar #'(lambda (f) (format t "~%~S" f)) (find-classes))
                (values))
               (t
         (format t "~S does not exist." object))))))

(defmethod helper ((object null))
  (format t "~% This is the Artificial Network Package v. 1.5.~%")
  (format t "~% This package contains functions and objects in lisp environnement.")
  (format t "~% Please, type : (help 'functions) or (help 'classes) to print the list
of available functions or objects."))

;(help 'functions)
;(help 'classes)

(defmethod helper ((object list))
  (mapcar #'helper object))

(defmethod helper ((object standard-class))
  (format t "~%~S :~%" (class-name object))
  (documentation (class-name object)))

;(helper (find-class 'situation))
;(help (find-class 'ann) (find-class 'mlp))

(defmethod find-subclasses ((root symbol))
  "Give the list of sub-class of root."
  (let ((classes '()))
    (do-symbols (sym)
      (let ((new-class (find-class sym nil)))
        (when (and new-class
                   (subtypep new-class root))
          (push new-class classes))))
    (sort (remove (find-class root) classes)
          #'string-lessp :key #'class-name)))

(defmethod find-subclasses ((root standard-class))
  "Give the list of sub-class of root."
  (find-subclasses (class-name root)))


(defmethod find-subclasses ((root null))
  "Give the list of sub-class of root."
  nil)

;(find-subclasses 'dimension)
;(find-subclasses nil)
;(find-subclasses 'BODYPART)


#|
(CLASS-PRECEDENCE-LIST (find-class 'arm nil))
|#


(defun find-classes (&optional (package 'lol))
  "Give the list of sub-class of root."
  (let ((classes '()))
    (do-symbols (sym)
      (let ((new-class (find-class sym nil)))
        (when (and (eql 'standard-class (type-of new-class))
                   (eq (find-package package)
                       (SYMBOL-PACKAGE (class-name new-class))))
          (push new-class classes))))
    (sort classes
          #'string-lessp :key #'class-name)))
;(find-classes)

(defun generic-function-p (self)
  "T if self is a class of <standard-generic-function>."
  (or (eq (class-name
           (class-of self))
          'standard-generic-function)
      (eq (class-name
       (class-of self))
      'compiled-function)))

(defun function-p (self)
  "T if self is a class of <standard-generic-function>."
  (or (eq (class-name
           (class-of self))
          'standard-generic-function)
      (eq (class-name
           (class-of self))
          'compiled-function)))

(defun type-p (self type)
  "T if self is a class of <standard-generic-function>."
  (if (member (find-class self nil)
              (find-subclasses type))
    t nil))

;(type-p 'mlp 'ann)

(defun find-functions (&optional (package (find-package 'lol)))
  "Give the list of generic functions in <package>."
  (when (symbolp package) (setf package (find-package package)))
  (let
    ((func '()))
    (do-symbols (sym package)
      (when (eq package (symbol-package sym))
        (let ((value
               (when (fboundp sym)
                 (symbol-function sym))))
          (when (and value
                     (generic-function-p value)) ;(generic-function-p value))
            (push sym func)))))
    (sort func #'string-lessp)))

;(find-functions 'lol)

(defun search-name (name &optional (test #'(lambda (x) (declare (ignore x)) t)))
  (let ((func '()) (lname (characters (string name))))
    (do-symbols (sym (find-package 'lol))
      (when (eq (find-package 'lol) (symbol-package sym)) ;;<- voir quand non
        (let ((value
               (list (when (and (fboundp sym) (not (macro-function sym)))
                      (symbol-function sym))
                     (when (find-class sym nil) (find-class sym nil))
                     (when (boundp sym) (eval sym))
                     (when (macro-function sym) (macro-function sym) )
                     
                     )))
          (dolist (v value)
            (when (and v (funcall test v))
              (if (seq-member lname (characters (string sym)))
                (push (list 0 sym (class-name (class-of v))) func)
                (push (list (editing-distance
                             (characters (string sym))
                             lname 
                             1.2 1 :test #'char-equal :scale t :fact 0)
                            sym
                            (class-name (class-of v))) func)))))))
    (mapcar #'cdr
            (sort (remove-if #'(lambda (a) (> a .74))
                             func :key #'car)
                  ;#'string-lessp
                  '<
                  :key #'car))))

;(search-name 'zozo  #'generic-function-p)
;(search-name 'new-)
;;(search-name 'currant)
;(help)
;(help 'functions)

;(search-name 'zozo) ;#'(lambda (x) (type-p x 'ann))) ;#'generic-function-p

