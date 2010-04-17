;;
;; The NClosEmacs Project
;;
;; unify.el
;;
;; Pattern matching and variable unification
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE
;; Attempt at implementing rule evaluation using the (i) Lisp symbol binding
;; and (ii) aspect-oriented programming via the advice functions of ELisp.

(defun sign-reader (var)
  "Reads an unbound sign or hypothesis"
  (cond
   ;; An hypothesis: backward chain on rules
   ((memq var nclose-global-hypos) (nclose-eval-rules var) (symbol-value var))
   ;; A sign: default to question
   (t
    (if (get var ':collection)
    (completing-read (format "What is the value of %s? [...] " var)
             (get var ':collection))
      (read-minibuffer (format "What is the value of %s? " var)))
    )
   )
)

;; This function is advised
(defun sign-writer (var val)
  "Binds a sign or hypothesis to its value"
  (set var val)
)

;; Ancillary scope handling functions
(defun cadaar (l) (car (cdar (car l))))


(defun default-scope-acc (l env)
  "Accumulates set extensions in symbol plist, based on scope specifications.  Supposes extensions are themselves p-values for ':instances property."
  (cond
   ((null l) (symbol-plist env))
   (t (put env (cadaar l) (make-vector (cdar l) (get (cadaar l) ':instances)))
      (scope-acc (cdr l) env)))
)

(defun scope-acc (l env)
  "Accumulates set extensions in symbol plist according to installed NClos."
  (nclos-scope-acc l env)
)

;; This function has been updated for the more complex evaluation of object patterns.
;; This function is advised
(defun nclose-get-unification (pattern scopes)
  "Interactive unification of pattern"
  (condition-case error
      (let* ((ignore (setplist 'env nil))
	     (ignore (scope-acc scopes 'env))
	     ;; NONE set-operator
 	     (none-in-local
	      '(lambda (class index exp)
		 (let ((unification (scope-none (aref (get 'env class) (- index 1)) exp)))
		   (aset (get 'env class) (- index 1) (car unification))
		   (cdr unification)))
	      )
	     ;; O(nly-)ONE set-operator
	     (oone-in-local
	      '(lambda (class index exp)
		 (let ((unification (scope-oone (aref (get 'env class) (- index 1)) exp)))
		   (aset (get 'env class) (- index 1) (car unification))
		   (cdr unification)))
	      )
	     ;; SOME set-operator
	     (some-in-local 
	      '(lambda (class index exp)
		 (let ((unification (scope-some (aref (get 'env class) (- index 1)) exp)))
		   (aset (get 'env class) (- index 1) (car unification))
		   (cdr unification)))
	      )
	     ;; ALL set-operator
	     (all-in-local 
	      '(lambda (class index exp)
		 (let ((unification (scope-all (aref (get 'env class) (- index 1)) exp)))
		   (aset (get 'env class) (- index 1) (car unification))
		   (cdr unification)))
	      
	      )
	     )
	;;(debug nil (symbol-plist 'env))
	(cons (symbol-plist 'env) (eval pattern))
	)
    (void-variable
     (sign-writer (cadr error) (sign-reader (cadr error)))
     ;; Recurse
     (nclose-get-unification pattern scopes)
    )
    )
)

;; This function is advised
(defun nclose-rule-unify (lhs hypo scopes rhs &optional rule-info)
  "Forces interactive unification of ordered LHS pattern and triggers RHS"
  (let ((unification (nclose-get-unification (bound-sort lhs) scopes)))
    (if (cdr unification) (and-execute-rhs rhs (car unification) rule-info))
    (cdr unification))
)
