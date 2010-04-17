;;
;; The NClosEmacs Project
;;
;; set-unification.el
;;
;; Unifying class patterns in LHSes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE
;; Following up on the idea of using the Lisp evaluator itself as much as possible to match and evaluate LHSes of rules.

;;  Objects are symbols, properties and their values are stored a these symbols plists.  Evaluating a class pattern on an object involves replacing access to properties with accesses scoped to the proper object plist.

;;  Rules compilation needs to keep track of the number of scopes for each class or object appearing in its LHSes.  This size is used at unification time to allocate an array of scopes, initialized with the complete set of instances or subobjects.  These scopes are filtered as evaluation progresses.  The array of reduced scopes is then passed to RHS execution.

;;  As the Lisp evaluator is used in scope, as if the objects were the Lisp environment for variables, simple variables like 'x' are understood as 'ELisp.x', where ELisp is a global system object, rather than 'x.value' as in the original Nexpert Object.  In other words, variables are properties of a system object in this implementation, rather than objects themselves.  This design choice has the interesting consequence of offering a choice of several new gating mechanisms for forward chaining.  For instance (i) gating within objects; (ii) gating within a scope or a set of objects, which can also be combined with inheritance links, are now available.

;; The syntax for the set operators, '[some|all|none|oone]-in', is as follows:
;;            (<set-op> 'SET-NAME PATTERN-INDEX 'LISP-FORM)
;; with the following conventions: SET-NAME are unique symbols; PATTERN-INDEX is a integer > 1 used to distinguish between several conditions bearing on the same set instances; and LISP-FORM is a boolean expression to be evaluated, in the scope of each individual instance.  This mean that each "free" variable in LISP-FORM is understood as a property of the current instance of set SET-NAME. 

;; Both functions are diverted to nclos.el
(defun property-reader (sym prop)
  "Reads a possibly unbound object's property"
  (nclos-slot-reader sym prop)
)

;; This function is advised
(defun property-writer (sym prop val)
  "Writes the value of an object's property into its plist, overwriting the previous value if any."
  (nclos-slot-writer sym prop val)
)

;;

(defun scope-get (sym prop)
  "Getting the value of an object's property has a side-effect of updating the plist if unknown.  The read and write calls are separated to allow for context-oriented extensions."
  (property-writer sym prop (property-reader sym prop))
)

(defun scope-set-expression (form obj)
  "Replaces occurences of property with access calls to its value from the object plist"
  (cond
   ((null form) nil)
   ((atom (car form)) 
    (if (or (stringp (car form)) (numberp (car form))
	    (memq (car form) nclose-global-lhsops))
	(cons (car form) (scope-set-expression (cdr form) obj))
        (cons (list 'scope-get (list 'quote obj) (list 'quote (car form))) 
	      (scope-set-expression (cdr form) obj))))
   ((listp (car form)) (cons (scope-set-expression (car form) obj) 
			     (scope-set-expression (cdr form) obj)))
   )
)

;; This mapcar is costly when scope is a very large set.  Hash table?
(defun scope-eval-expression (scope form)
  "Evals a class expression for all objects in scope"
  (mapcar '(lambda (obj) (eval (scope-set-expression form obj))) scope)
)

;; Same issue of performance for very large sets.
(defun scope-filter (scope filter)
  "Restricts scope to objects filtered to t"
  (cond
   ((null filter) nil)
   ((car filter) (cons (car scope) (scope-filter (cdr scope) (cdr filter))))
   ( t (scope-filter (cdr scope) (cdr filter)))
   )
)

;; Set operators: all, some, only-one, none
(defun set-allp (l)
  "All set members match the pattern"
  (cond
   ((null l) t)
   (t (and (car l) (set-allp (cdr l))))
   )
)

(defun set-somep (l)
  "Some set members match the pattern"
  (cond
   ((null l) nil)
   (t (or (car l) (set-somep (cdr l))))
   )
)

(defun set-nonep (l)
  "No set member does match the pattern"
  (cond
   ((null l) t)
   (t (and (not (car l)) (set-nonep (cdr l))))
   )
)

(defun set-count-matches (l)
  "Recursively counts matches, t symbols, in l"
  (cond
   ((null l) 0)
   ((car l) (+ 1 (set-count-matches (cdr l))))
   (t (set-count-matches (cdr l)))
   )
)

(defun set-oonep (l)
  "Exactly one set member matches the pattern"
  (= 1 (set-count-matches l)))

;; NOTE - Evaluating a transformed class pattern
;; Because we are unifying first, evaluation exhausts the scope even when lazy evaluation would not require it.
(defun scope-all (scope exp)
  "Evaluates the ALL set operator on given expression in scope and returns new scope for further elaboration"
  (let ((filter (scope-eval-expression scope exp)))
    ; TODO: handle the filtered scope properly
    (cons (scope-filter scope filter) (set-allp filter)))
)

(defun scope-some (scope exp)
  "Evaluates the SOME set operator on given expression in scope and returns new scope for futher elaboration"
  (let ((filter (scope-eval-expression scope exp)))
    ; TODO: handle the filtered scope properly
    (cons (scope-filter scope filter) (set-somep filter)))
)

(defun scope-none (scope exp)
  "Evaluates the NONE set operator on given expression in scope and returns new scope for further elaboration"
  (let ((filter (scope-eval-expression scope exp)))
    ; TODO: handle the filtered scope properly
    (cons (scope-filter scope filter) (set-nonep filter)))
)

(defun scope-oone (scope exp)
  "Evaluates the OONE set operator on given expression in scope and returns new scope for futher elaboration"
  (let ((filter (scope-eval-expression scope exp)))
    ; TODO: handle the filtered scope properly
    (cons (scope-filter scope filter) (set-oonep filter)))
)

;; NOTE
;; A convoluted transformation: patterns are compiled into a 'op-in' form with a class and index argument as accessors.  Each op-in macro expands in turn into a funcall to a local 'op-in-local' function which is locally defined in the main unification function.
(defmacro some-in (class index exp)
  (list 'funcall 'some-in-local class index exp))

(defmacro all-in (class index exp)
  (list 'funcall 'all-in-local class index exp))

(defmacro none-in (class index exp)
  (list 'funcall 'none-in-local class index exp))

(defmacro oone-in (class index exp)
  (list 'funcall 'oone-in-local class index exp))


