;;; =========================================================================
;;; Lisp CODE for translation infix languages to prefix languages

;;; To use with grammar defined in file *music2*.lisp

;;; =========================================================================

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming

;;;; Frederic Voisin aug. 1999 from Peter Norvig (1991)


;;; =========================================================================
;;; goodies features for common lisp (debugging, memoization, fast eval, etc...)
;;; =========================================================================

(proclaim '(inline mappend mklist flatten random-elt member-equal
	    starts-with compose last1 length=1
	    rest2 rest3 symbol old-symbol reuse-cons
	    queue-contents make-queue enqueue dequeue
	    front empty-queue-p queue-nconc
	    #-CLtL2 reduce #-CLtL2 reduce*))

;;; The function REQUIRES is used in subsequent files to state dependencies
;;; between files.  The current definition just loads the required files,
;;; assumming they match the pathname specified in *PAIP-source-files*.
;;; You should change that to match where you have stored the files.
;;; A more sophisticated REQUIRES would only load it if it has not yet
;;; been loaded, and would search in different directories if needed.

;;; First set up read macros for #P"pathname", if they are missing.

#-CLtL2
(eval-when (eval compile load)
  (defun read-pathname (stream char arg-count)
    (declare (ignore char arg-count))
    (parse-namestring
     (read-preserving-whitespace stream t nil t)))

  (set-dispatch-macro-character #\# #\P #'read-pathname)
  (set-dispatch-macro-character #\# #\p #'read-pathname)
  )

(defvar *PAIP-source-files* #P"*.lisp"
  "The location of the source files for this book.
  CHANGE IT TO REFLECT THE LOCATION OF THE FILES ON YOUR COMPUTER.")

(defun requires (&rest files)
  "The arguments are files that are required to run an application."
  (let ((*default-pathname-defaults* *paip-source-files*))
    (mapc #'load files)))

;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)

(eval-when (load eval compile)
  (defmacro once-only (variables &rest body)
    "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
    (assert (every #'symbolp variables))
    (let ((temps nil))
      (dotimes (i (length variables)) (push (gensym) temps))
      `(if (every #'side-effect-free? (list .,variables))
	(progn .,body)
	(list 'let
	 ,`(list ,@(mapcar #'(lambda (tmp var)
			       `(list ',tmp ,var))
			   temps variables))
	 (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
		       variables temps)
	   .,body)))))

  (defun side-effect-free? (exp)
    "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
    (or (atom exp) (constantp exp)
	(starts-with exp 'function)
	(and (starts-with exp 'the)
	     (side-effect-free? (third exp)))))

  (defmacro funcall-if (fn arg)
    (once-only (fn)
	       `(if ,fn (funcall ,fn ,arg) ,arg)))

  (defmacro read-time-case (first-case &rest other-cases)
    "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
    (declare (ignore other-cases))
    first-case)

  (defun rest2 (x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))

  (defun find-anywhere (item tree)
    "Does item occur anywhere in tree?"
    (if (atom tree)
	(if (eql item tree) tree)
	(or (find-anywhere item (first tree))
	    (find-anywhere item (rest tree)))))

  (defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))
  )
;;;; Auxiliary Functions

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

;;; ==============================

#-CLtL2
(defun map-into (result-sequence function &rest sequences)
  "Destructively set elements of RESULT-SEQUENCE to the results
  of applying FUNCTION to respective elements of SEQUENCES."
  (let ((arglist (make-list (length sequences)))
        (n (if (listp result-sequence)
               most-positive-fixnum
               (array-dimension result-sequence 0))))
    ;; arglist is made into a list of args for each call
    ;; n is the length of the longest vector
    (when sequences
      (setf n (min n (loop for seq in sequences
                           minimize (length seq)))))
    ;; Define some shared functions:
    (flet
      ((do-one-call (i)
         (loop for seq on sequences
               for arg on arglist
               do (if (listp (first seq))
                      (setf (first arg)
                            (pop (first seq)))
                      (setf (first arg)
                            (aref (first seq) i))))
         (apply function arglist))
       (do-result (i)
         (if (and (vectorp result-sequence)
                  (array-has-fill-pointer-p result-sequence))
             (setf (fill-pointer result-sequence) 
                   (max i (fill-pointer result-sequence))))))
      (declare (inline do-one-call))
      ;; Decide if the result is a list or vector,
      ;; and loop through each element
      (if (listp result-sequence)
          (loop for i from 0 to (- n 1)
                for r on result-sequence
                do (setf (first r)
                         (do-one-call i))
                finally (do-result i))
          (loop for i from 0 to (- n 1)
                do (setf (aref result-sequence i)
                         (do-one-call i))
                finally (do-result i))))
      result-sequence))

(defun seq-ref (seq index)
  "Return code that indexes into a sequence, using
  the pop-lists/aref-vectors strategy."
  `(if (listp ,seq)
       (prog1 (first ,seq)
              (setq ,seq (the list (rest ,seq))))
       (aref ,seq ,index)))

(defun maybe-set-fill-pointer (array new-length)
  "If this is an array with a fill pointer, set it to
  new-length, if that is longer than the current length."
  (if (and (arrayp array)
           (array-has-fill-pointer-p array))
      (setf (fill-pointer array) 
            (max (fill-pointer array) new-length))))

;;;; Reduce

#-CLtL2
(defun reduce* (fn seq from-end start end key init init-p)
  (funcall (if (listp seq) #'reduce-list #'reduce-vect)
           fn seq from-end (or start 0) end key init init-p))

#-CLtL2
(defun reduce (function sequence &key from-end start end key
               (initial-value nil initial-value-p))
  (reduce* function sequence from-end start end
           key initial-value initial-value-p))

#-CLtL2
(defun reduce-vect (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (assert (<= 0 start end (length seq)) (start end)
          "Illegal subsequence of ~a --- :start ~d :end ~d"
          seq start end)
  (case (- end start)
    (1 (if init-p
           (funcall fn init (funcall-if key (aref seq start)))
           (funcall-if key (aref seq start))))
    (0 (if init-p init (funcall fn)))
    (t (if (not from-end)
           (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (aref seq start)))
                       (funcall
                         fn
                         (funcall-if key (aref seq start))
                         (funcall-if key (aref seq (+ start 1)))))))
             (loop for i from (+ start (if init-p 1 2))
                   to (- end 1)
                   do (setf result
                            (funcall
                              fn result
                              (funcall-if key (aref seq i)))))
             result)
           (let ((result
                   (if init-p
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 1)))
                         init)
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 2)))
                         (funcall-if key (aref seq (- end 1)))))))
             (loop for i from (- end (if init-p 2 3)) downto start
                   do (setf result
                            (funcall
                              fn
                              (funcall-if key (aref seq i))
                              result)))
             result)))))

#-CLtL2
(defun reduce-list (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (cond ((> start 0)
         (reduce-list fn (nthcdr start seq) from-end 0
                      (- end start) key init init-p))
        ((or (null seq) (eql start end))
         (if init-p init (funcall fn)))
        ((= (- end start) 1)
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (from-end
         (reduce-vect fn (coerce seq 'vector) t start end
                      key init init-p))
        ((null (rest seq))
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (t (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (pop seq)))
                       (funcall
                         fn
                         (funcall-if key (pop seq))
                         (funcall-if key (pop seq))))))
             (if end
                 (loop repeat (- end (if init-p 1 2)) while seq
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq)))))
                 (loop while seq
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq))))))
             result))))

;;; ==============================

;;;; Other:

;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function symbol to something else.
;;; This has not been done (for compatibility with the book).  The only near-ANSI
;;; Lisp tested was Franz's Allegro EXCL, for which we allow the definition by
;;; unlocking the excl and common-lisp packages with the following form:

#+EXCL
(dolist (pkg '(excl common-lisp))
  (setf (package-lock-fdefinitions (find-package pkg)) nil))

(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

;;; ==============================

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun mklist (x) 
  "If x is a list return it, otherwise return the list of x"
  (if (listp x) x (list x)))

(defun flatten (exp)
  "Get rid of imbedded lists (to one level only)."
  (mappend #'mklist exp))

(defun random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

;;; ==============================

(defun member-equal (item list)
  (member item list :test #'equal))

;;; The Debugging Output Facility:

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

;;; ==============================

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

;;; ==============================

(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern) (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings)))
        (t fail)))

(defun match-variable (var input bindings)
  "Match a single variable against input in the context of the bindings"
  (if (get-binding var bindings)
      (if (equal input (lookup var bindings)) bindings fail)
      (extend-bindings var input bindings)))

(defun get-binding (var bindings) (assoc var bindings))
(defun lookup (var bindings) (rest (assoc var bindings)))
(defun extend-bindings (var val bindings) (cons (cons var val) bindings)) 

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

;;; ==============================

(defun compose (&rest functions)
  #'(lambda (x)
      (reduce #'funcall functions :from-end t :initial-value x)))

#-CLtL2
(defun complement (fn)
  "If FN returns y, then (complement FN) returns (not y)."
  #'(lambda (&rest args) (not (apply fn args))))

;;; ==============================

;;;; The Memoization facility:

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

;;;; Delayed computation:
(defstruct delay value (computed? nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :value #'(lambda () . ,body)))

(defun force (delay)
  "Do a delayed computation, or fetch its previously-computed value."
  (if (delay-computed? delay)
      (delay-value delay)
      (prog1 (setf (delay-value delay) (funcall (delay-value delay)))
             (setf (delay-computed? delay) t))))

;;;; Defresource:
(defmacro defresource (name &key constructor (initial-copies 0)
                       (size (max initial-copies 10)))
  (let ((resource (symbol '* (symbol name '-resource*)))
        (deallocate (symbol 'deallocate- name))
        (allocate (symbol 'allocate- name)))
    `(progn
       (defparameter ,resource (make-array ,size :fill-pointer 0))
       (defun ,allocate ()
         "Get an element from the resource pool, or make one."
         (if (= (fill-pointer ,resource) 0)
             ,constructor
             (vector-pop ,resource)))
       (defun ,deallocate (,name)
         "Place a no-longer-needed element back in the pool."
         (vector-push-extend ,name ,resource))
       ,(if (> initial-copies 0)
            `(mapc #',deallocate (loop repeat ,initial-copies 
                                       collect (,allocate))))
       ',name)))

(defmacro with-resource ((var resource &optional protect) &rest body)
  "Execute body with VAR bound to an instance of RESOURCE."
  (let ((allocate (symbol 'allocate- resource))
        (deallocate (symbol 'deallocate- resource)))
    (if protect
        `(let ((,var nil))
           (unwind-protect (progn (setf ,var (,allocate)) ,@body)
             (unless (null ,var) (,deallocate ,var))))
        `(let ((,var (,allocate)))
           ,@body
           (,deallocate var)))))

;;;; Queues:

;;; A queue is a (last . contents) pair

(defun queue-contents (q) (cdr q))

(defun make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)

(defun front (q) (first (queue-contents q)))

(defun empty-queue-p (q) (null (queue-contents q)))

(defun queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))

;;;; Other:

(defun sort* (seq pred &key key) 
  "Sort without altering the sequence"
  (sort (copy-seq seq) pred :key key))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

;;; ==============================

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))

;;; ==============================

(defun unique-find-if-anywhere (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-if-anywhere
        predicate
        (first tree)
        (unique-find-if-anywhere predicate (rest tree)
                                 found-so-far))))

(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

#-CLtL2
(defmacro with-compilation-unit (options &body body)
  "Do the body, but delay compiler warnings until the end."
  ;; That way, undefined function warnings that are really
  ;; just forward references will not be printed at all.
  ;; This is defined in Common Lisp the Language, 2nd ed.
  (declare (ignore options))
  `(,(read-time-case
       #+Lispm 'compiler:compiler-warnings-context-bind
       #+Lucid 'with-deferred-warnings
               'progn)
    .,body))

;;; ==============================

(defmacro define-enumerated-type (type &rest elements)
  "Represent an enumerated type with integers 0-n."
  `(progn
     (deftype ,type () '(integer 0 ,(- (length elements) 1)))
     (defun ,(symbol type '->symbol) (,type)
       (elt ',elements ,type))
     (defun ,(symbol 'symbol-> type) (symbol)
       (position symbol ',elements))
     ,@(loop for element in elements
             for i from 0
             collect `(defconstant ,element ,i))))

;;; ==============================

(defun not-null (x) (not (null x)))

(defun first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))

(defun first-or-self (x)
  "The first element of x, if it is a list; else x itself."
  (if (consp x) (first x) x))

;;; =========================================================================

;; grammar functions and structure 

;;; =========================================================================

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; with handling of preferences as described in Norvig's book Section 19.6.

(defvar *grammar* "The grammar used by GENERATE.")

(defstruct (rule (:type list) 
                 (:constructor rule (lhs -> rhs &optional sem score)))
  lhs -> rhs sem score)

(defstruct (tree (:type list) (:include rule) (:copier nil)
                 (:constructor new-tree (lhs sem score rhs))))

(defun use (grammar)
  "Switch to a new grammar."
  (clear-memoize 'rules-starting-with)
  (clear-memoize 'lexical-rules)
  (length (setf *grammar* 
                (mapcar #'(lambda (r) (apply #'rule r))
                        grammar))))

(defstruct (parse) "A parse tree and a remainder." tree rem)

(defun parse-lhs (parse) (tree-lhs (parse-tree parse)))

#|
(defun lexical-rules (word)
  "Return a list of rules with word on the right hand side."
  (find-all word *grammar* :key #'rule-rhs :test #'equal))
|#

(defun rules-starting-with (cat)
  "Return a list of rules where cat starts the rhs."
  (find-all cat *grammar* 
            :key #'(lambda (rule) (first-or-nil (rule-rhs rule)))))

(defun first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))

(defun complete-parses (parses)
  "Those parses that are complete (have no remainder)."
  (find-all-if #'null parses :key #'parse-rem))

(defun append1 (items item)
  "Add item to end of list of items."
  (append items (list item)))

(defun parser (words)
  "Return all complete parses of a list of words."
  (clear-memoize 'parse) ;***
  (mapcar #'parse-tree (complete-parses (parse words))))

(defparameter *open-categories* '(N V A Name)
  "Categories to consider for unknown words")

(defun lexical-rules (word)
  "Return a list of rules with word on the right hand side."
  (or (find-all word *grammar* :key #'rule-rhs :test #'equal)
      (mapcar #'(lambda (cat) `(,cat -> ,word)) *open-categories*)))

(defun apply-semantics (tree)
  "For terminal nodes, just fetch the semantics.
  Otherwise, apply the sem function to its constituents."
  (if (terminal-tree-p tree)
      (tree-sem tree)
      (setf (tree-sem tree)
            (apply (tree-sem tree)
                   (mapcar #'tree-sem (tree-rhs tree))))))

(defun terminal-tree-p (tree)
  "Does this tree have a single word on the rhs?"
  (and (length=1 (tree-rhs tree))
       (atom (first (tree-rhs tree)))))

(defun parse (words)
  "Bottom-up parse, returning all parses of any prefix of words.
  This version has semantics and preference scores."
  (unless (null words)
    (mapcan #'(lambda (rule)
                (extend-parse (rule-lhs rule) (rule-sem rule)
                              (rule-score rule) (list (first words)) ;***
                              (rest words) nil))
            (lexical-rules (first words)))))

(defun extend-parse (lhs sem score rhs rem needed) ;***
  "Look for the categories needed to complete the parse.
  This version has semantics and preference scores."
  (if (null needed)
      ;; If nothing is needed, return this parse and upward extensions,
      ;; unless the semantics fails
      (let ((parse (make-parse :tree (new-tree lhs sem score rhs) ;***
                               :rem rem)))
        (unless (null (apply-semantics (parse-tree parse)))
          (apply-scorer (parse-tree parse)) ;***
          (cons parse
                (mapcan
                  #'(lambda (rule)
                      (extend-parse
                        (rule-lhs rule) (rule-sem rule)
                        (rule-score rule) (list (parse-tree parse)) ;***
                        rem (rest (rule-rhs rule))))
                  (rules-starting-with lhs)))))
      ;; otherwise try to extend rightward
      (mapcan
        #'(lambda (p)
            (if (eq (parse-lhs p) (first needed))
                (extend-parse lhs sem score 
                              (append1 rhs (parse-tree p)) ;***
                              (parse-rem p) (rest needed))))
        (parse rem))))

(defun apply-scorer (tree)
  "Compute the score for this tree."
  (let ((score (or (tree-score tree) 0)))
    (setf (tree-score tree)
          (if (terminal-tree-p tree)
              score
              ;; Add up the constituent's scores,
              ;; along with the tree's score
              (+ (sum (tree-rhs tree) #'tree-score-or-0)
                 (if (numberp score)
                     score
                     (or (apply score (tree-rhs tree)) 0)))))))

(defun tree-score-or-0 (tree)
    (if (numberp (tree-score tree)) (tree-score tree) 0))

(defun all-parses (words)
  (format t "~%Score  Semantics~25T~a" words)
  (format t "~%=====  =========~25T============================~%")
  (loop for tree in (sort (parser words) #'> :key #'tree-score)
    do (format t "~5,1f  ~9a~25T~a~%" (tree-score tree) (tree-sem tree)
               (bracketing tree)))
  (values))

(defun bracketing (tree)
  "Extract the terminals, bracketed with parens."
  (cond ((atom tree) tree)
        ((length=1 (tree-rhs tree))
         (bracketing (first (tree-rhs tree))))
        (t (mapcar #'bracketing (tree-rhs tree)))))

(defun meaning (words &optional (tie-breaker #'query-user))
  "Choose the single top-ranking meaning for the words."
  (let* ((trees (sort (parser words) #'> :key #'tree-score))
         (best-score (if trees (tree-score (first trees)) 0))
         (best-trees (delete best-score trees
                             :key #'tree-score :test-not #'eql))
         (best-sems (delete-duplicates (mapcar #'tree-sem best-trees)
                                       :test #'equal)))
    (case (length best-sems)
      (0 (format t "~&Sorry, I didn't understand that.") nil)
      (1 (first best-sems))
      (t (funcall tie-breaker best-sems)))))

#|
(defun query-user (choices &optional
                           (header-str "~&Ambiguous syntaxe; please select a result:")
                           (footer-str "~&Your choice? "))
  "Ask user to make a choice."
  (format *query-io* header-str)
  (loop for choice in choices for i from 1 do
        (format *query-io* "~&~3d: ~a" i choice))
  (format *query-io* footer-str)
  (nth (- (read) 1) choices))
|#

(defun query-user (choices &optional
                           (header-str "Ambiguous syntaxe :"))
  "Ask user to make a choice."
  (select-item-from-list choices
                         :window-title header-str
                         :selection-type :single
                         :default-button-text "select"))

(memoize 'lexical-rules)
(memoize 'rules-starting-with)
(memoize 'parse :test #'eq)

;;;; Grammar

(defvar *grammar7*
  '((NP -> (NP CONJ NP) infix-funcall  infix-scorer)
    (NP -> (N P N)      infix-funcall  infix-scorer)
    (NP -> (N)          list)
    (NP -> ([ NP ])     arg2)
    (NP -> (NP ADJ)     rev-funcall    rev-scorer)
    (NP -> (NP OP N)    infix-funcall)
    (N  -> (D)          identity)
    (N  -> (N D)        10*N+D)
    (P  -> to           integers       prefer<)
    ([  -> [            [)
    (]  -> ]            ])
    (OP -> repeat       repeatn)
    (CONJ -> and        append         prefer-disjoint)
    (CONJ -> without    set-diff       prefer-subset)
    (ADJ -> reversed    reverse        inv-span)
    (ADJ -> shuffled    permute        prefer-not-singleton)
    (D -> 1 1) (D -> 2 2) (D -> 3 3) (D -> 4 4) (D -> 5 5)
    (D -> 6 6) (D -> 7 7) (D -> 8 8) (D -> 9 9) (D -> 0 0)))

(defun infix-funcall (arg1 function arg2)
  "Apply the function to the two arguments"
  (funcall function arg1 arg2))

(defun set-diff (x y)
  (reverse (set-difference x y)))

(defun 10*N+D (n d) (+ (* 10 N) D))

(defun prefer< (x y) (if (>= (sem x) (sem y)) -1))

(defun prefer-disjoint (x y) (if (intersection (sem x) (sem y)) -1))

(defun prefer-subset (x y)
  (+ (inv-span x) (if (subsetp (sem y) (sem x)) 0 -3)))

(defun prefer-not-singleton (x)
  (+ (inv-span x) (if (< (length (sem x)) 2) -4 0)))

(defun infix-scorer (arg1 scorer arg2)
  (funcall (tree-score scorer) arg1 arg2))

(defun arg2 (a1 a2 &rest a-n) (declare (ignore a1 a-n)) a2)

(defun rev-scorer (arg scorer) (funcall (tree-score scorer) arg))

(defun rev-funcall (arg function) (funcall function arg))

(defun repeatn (list n)
  "Append list n times."
  (if (= n 0)
      nil
      (append list (repeatn list (- n 1)))))

(defun span-length (tree)
  "How many words are in tree?"
  (if (terminal-tree-p tree) 1
      (sum (tree-rhs tree) #'span-length)))

(defun inv-span (tree) (/ 1 (span-length tree)))

(defun sem (tree) (tree-sem tree))

(defun integers (start end)
  "A list of all the integers in the range [start...end] inclusive.
  This version allows start > end."
  (cond ((< start end) (cons start (integers (+ start 1) end)))
        ((> start end) (cons start (integers (- start 1) end)))
        (t (list start))))

(defun sum (numbers &optional fn)
  "Sum the numbers, or sum (mapcar fn numbers)."
  (if fn
      (loop for x in numbers sum (funcall fn x))
      (loop for x in numbers sum x)))

(defun permute (bag)
  "Return a random permutation of the given input list."
  (if (null bag)
      nil
      (let ((e (random-elt bag)))
        (cons e (permute (remove e bag :count 1 :test #'eq))))))


#|
(setf *music1*
  '((NP -> (NP CONJ NP) infix-funcall  infix-scorer)
    (NP -> (N P N)      infix-funcall  infix-scorer)
    (NP -> (N)          list)
    (NP -> ([ NP ])     arg2)
    (NP -> (NP ADJ)     rev-funcall    rev-scorer)
    (NP -> (NP OP N)    infix-funcall)
    (N  -> (D)          identity)
    (N  -> (INT)        identity)
    (N  -> (NOTE)       identity)
    (N  -> (N D)        10*N+D)
    (P  -> to           integers       prefer<)
    ([  -> [            [)
    (]  -> ]            ])
    (OP -> repeat       repeat)
    (CONJ -> and        append         prefer-disjoint)
    (CONJ -> without    set-diff       prefer-subset)
    (ADJ -> reversed    reverse        inv-span)
    (ADJ -> shuffled    permute        prefer-not-singleton)
    (D -> 1 1) (D -> 2 2) (D -> 3 3) (D -> 4 4) (D -> 5 5)
    (D -> 6 6) (D -> 7 7) (D -> 8 8) (D -> 9 9) (D -> 0 0)
    ;;music !!
    (INT -> seconde  200) (INT -> tierce  400) (INT -> quarte  500)
    (INT -> quinte  700) (INT -> sixte  900) (INT -> septieme  1000)
    (INT -> octave  1200)
    (NOTE -> C3 6000) (NOTE -> C#3 61) (NOTE -> D3 62) (NOTE -> D#3 63)
    (NOTE -> E3 64) (NOTE -> F3 65) (NOTE -> F#3 66) (NOTE -> G3 67)
    (NOTE -> G#3 68) (NOTE -> Db3 61) (NOTE -> Eb3 63) (NOTE -> Gb3 66)
    (NOTE -> Ab3 68) (NOTE -> Bb3 70) (NOTE -> A#3 70) (NOTE -> B3 71)
    (CLASS -> do  C)
    (NP -> (CLASS) zeroaire-fn)
    (P  -> <<           integers       prefer<)
    (P  -> >>           integers       prefer<)
    (ADJ -> desc    desc   inv-span)
    (ADJ -> twice   twice  inv-span)
    (ADJ -> minor   minor  inv-span)
    (ADJ -> maj     maj    inv-span)
    (ADJ -> dim     dim    inv-span)
    (ADJ -> aug     aug    inv-span)
    ))



(defun melodie (rules)
  (meaning rules))

(defun unaire-fn (fn x)
  (funcall fn x))

(defun zeroaire-fn (fn)
  (funcall fn))

(defun desc (x)
  (if (atom x)
    (- x)
    (mapcar #'desc x)))

(defun twice (x)
  (repeat x 2))

(defun minor (x)
  (if (atom x)
    (- x 100)
    (mapcar #'minor x)))

(defun dim (x)
  (- x 100))

(defun maj (x)
  x)

(defun aug (x)
  (if (atom x)
    (+ x 100)
    (mapcar #'aug x)))

(defun C ()
  (list 3600 4800 6000 7200 8400 9600))

(defun steps (list s )
  (let ((r '()))
    (dotimes (n (1+ (floor (/ (length list) s))) (reverse r))
      (push (nth (* n s) list) r))))

(defun echant (list n)
  (let ((l (length list))
        (r '()))
    (if (>= n l) 
      (dotimes (i n (reverse r))
        (push (nth (floor (* i (/ l n))) list) r))
      (let ((newn (- n 2)))
        (dotimes (i newn (append (list (car list)) (reverse r) (last list)))
          (push (nth (round (* (1+ i) (/ (- l 2) (if (evenp newn)
                                                   (1+ newn)
                                                   (if (= 1 newn)
                                                     2 newn))))) list) r))))))

#|
(steps 2 14 2)
|#

(meaning '(quarte and seconde desc twice))
(meaning '(quarte and seconde desc))
(meaning '(seconde desc))
(meaning '(c3 and db3 and f#3))
(meaning '(c3 to b3 without f#3 ))
(meaning '([ c3 to b3 without f#3 ] reversed))
(meaning '(c3 to b3 without f#3 twice))
(all-parses '(c3 to b3 without f#3 twice))
(meaning '(do and f#3))
(meaning '(f#3 and do))
(meaning '(do without c3))
(meaning '(quarte and seconde))
(meaning '(8 << 80 step 5))
(meaning '(8 << 80 by 5))
(meaning '(c3 to b3 by seconde))
|#








