(function and)

(defun and (&rest args)
  (and args))

(and t nil)

(type-of (symbol-function 'and))
(type-of (MACRO-FUNCTION 'and))


(let ((longlist '(1 2 3)))
(flet ((safesqrt (x) (sqrt (abs x)))) 
  ;; The safesqrt function is used in two places. 
  (safesqrt (apply #'+ (map 'list #'safesqrt longlist)))))

(let ((a '(t nil)))
  (flet ((fn () (eval `(and ,.a))))
    (fn)))


(let ((a '(t nil))
      (f 'and))
  (flet ((fn () (eval `((lambda () (symbol-function f) ,.a)))))
    (fn)))

(MACRO-FUNCTION 'and)