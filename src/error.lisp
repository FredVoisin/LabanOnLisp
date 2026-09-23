(setq aardvarks '(sam harry fred))
(check-type aardvarks (vector integer))
Error: The value of AARDVARKS, (SAM HARRY FRED), 
       is not a vector of integers. 

(setq naards 'foo)
(check-type naards (integer 0 *) "a positive integer")

;;;********
(defun check (expr &optional (stream t))
  (handler-case (eval expr)
    (division-by-zero (expr) (format stream "~%> Division by zero detected on ~S.~%> Process cancelled" (slot-value expr 'ccl::operands))
                      (values))
    (unbound-variable () (format stream "~%> Unknown variable detected.~%> Process cancelled" (values)))
    (undefined-function ()
                        
                        (format stream "~%> Unknown functions detected.~%> Process cancelled" (values))
                        )
    )
  )

#|
(check '(/ 10 0) a)
(check 'bb)
(check 'add1)
|#


(machine-type)
(machine-version)
(machine-instance)
(software-type)
(software-version)
(short-site-name)
(long-site-name)
(user-homedir-pathname)
*features*
(lisp-implementation-type)
(lisp-implementation-version)

#|
condition 
    simple-condition 
    serious-condition 
        error 
            simple-error 
            arithmetic-error 
                division-by-zero 
                floating-point-overflow 
                floating-point-underflow 
                ... 
            cell-error 
                unbound-variable 
                undefined-function 
                ... 
            control-error 
            file-error 
            package-error 
            program-error 
            stream-error 
                end-of-file 
                ... 
            type-error 
                simple-type-error 
                ... 
            ... 
        storage-condition 
        ... 
    warning 
        simple-warning 
        ... 
    ...
|#

#|
(check '(/ 1 0))
(check '(/ 1 a))
|#

(handler-case (/ 1 0) 
  (division-by-zero () (format t "Aborted")))



(setq aardvarks '(sam harry fred))
(check-type aardvarks (vector integer)) 
Error: The value of AARDVARKS, (SAM HARRY FRED), 
       is not a vector of integers. 

(setq naards 'foo)
(check-type naards (or (symbolp) (integer 0 *)) "a positive integer")



#|
(defun command-dispatch (cmd) 
  (let ((fn (get cmd 'command))) 
    (if (not (null fn)) 
        (funcall fn)) 
        (error "The command ~S is unrecognized." cmd)))

(command-dispatch 'emergnecy-shutdown)
|#



#|
(setf vals '(-47 30 1 2 3 4))
(let ((nvals (list-length vals))) 
  (unless (= nvals 3) 
    (cond ((< nvals 3) 
           (cerror "Assume missing values are zero." 
                   "Too few values in ~S;~%~ 
                    three are required, ~ 
                    but ~R ~:[were~;was~] supplied." 
                   nvals (= nvals 1)) 
           (setq vals (append vals (subseq '(0 0 0) nvals)))) 
          (t (cerror "Ignore all values after the first three." 
                     "Too many values in ~S;~%~ 
                      three are required, ~ 
                      but ~R were supplied." 
                      nvals) 
             (setq vals (subseq vals 0 3))))))
|#


(defun known-wordp (word)
  (if (member word '("test" "oui" "non") :test #'equalp)
    t nil))

(let ((word (read-line)))
(do () 
    ((known-wordp word) word) 
  (cerror "You will be prompted for a replacement word." 
          "~S is an unknown word (possibly misspelled)." 
          word) 
  (format *query-io* "~&New word: ") 
  (setq word (string (read *query-io*)))))

#|
(constantp 1)
|#