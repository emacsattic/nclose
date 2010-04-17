;;
;; The NClosEmacs Project
;;
;; rules.el
;;
;; Management and transformation of rules declarations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REMARK
;; Draws inspiration from the original text syntax of Nexpert Object knowledge
;; bases, found in files with the TKB extension.

;; Keywords in external rule syntax
(defmacro @if (l)
  (cond
   ((or
     (atom l) 
     (not (memq (car l) nclose-global-lhsops-composite)))
    (list 'quote (list 'and l)))
   (t (list 'quote l))
   )
  )
(defmacro @LHS= (l) (list 'quote l))
(defmacro @hypo (l) (list 'quote l))
(defmacro @HYPO= (l) (list 'quote l))
(defmacro @then (&rest l) (list 'quote l))
(defmacro @RHS= (&rest l) (list 'quote l))

;; Globales for pattern language
(defun yes (l) (not (null l)))
(defun no (l) (null l))
(defun Yes (l) (not (null l)))
(defun No (l) (null l))

(defun @rule (lhs hypo &optional rhs long-desc)
  "Builds the form required for evaluation and unification of the rule"
  (cons 'setq (list hypo (list 'nclose-get-unification (list 'quote lhs))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE
;; The rules "compiler" builds a few global lists used for evaluation.
;;   - The forward associations list keeps track, for a given sign, of all the 
;; hypotheses rules mentioning this sign lead to.  It is implemented here as a
;; PList with the sign as key and the list of deferred hypotheses as value.
;;   - The working memory is the list of all signs.
;;   - The production memory is the list of all rules.

;; Querying the rule base.  The rule base is implemented here as a list!  Is it worthwhile to consider hash tables?
(defun caddr (l)
  "Utility"
  (car (cdr (cdr l))))

(defun cadddr (l)
  "Utility cadddr"
  (car (cdr (cdr (cdr l)))))

(defun caddddr (l)
  "Utility caddddr"
  (car (cdr (cdr (cdr (cdr l))))))

;; This function should be advised to order rules differently
(defun find-rules (hypo l)
  "Select rules with given hypothesis. Returns a list of ((LHS . RHS) scopes (# . long-desc))"
  ;; global-pm elements are (# (lhs hypo scopes &optional rhs desc))
  (cond
   ((null l) nil)
   ((eq (cadr (cdar l)) hypo)
    (cons
     (list
      ;; This is (lhs . rhs)
      (cons (car (cdar l)) (cadddr (cdar l))) 
      ;; This is 'scopes'
      (caddr (cdar l))
      ;; This is (# . desc)
      (cons (caar l) (caddddr (cdar l)))
      )
     (find-rules hypo (cdr l))))
   (t (find-rules hypo (cdr l)))
  )
)

;; Hypotheses and signs implementation
(defun find-name-recurse (hypo l)
  "Recursively search hypotheses by name"
  (cond
   ((null l) nil)
   ((string= (car l) hypo) (car l))
   (t (find-name-recurse hypo (cdr l)))
   )
)

(defun find-hypo (hypo)
  "Search hypothesis by name"
  (find-name-recurse hypo nclose-global-hypos))

(defun find-sign (sign)
  (find-name-recurse sign nclose-global-signs))


;; Handling signs found in LHSes and RHSes.

;; This function is advised
(defun sign-compile (sign hypo)
  "A sign has been identified at compile time, update required globales"
  (add-to-list 'nclose-global-signs sign)
  )

(defun sign-update-collection (sign svalue)
  "A string-typed sign keeps tracks of its range of values in a completion collection."
  (let* ((collection (get sign ':collection))
     (match (try-completion svalue collection))
     )
    (unless (and match (not (stringp match)))
      (put sign ':collection
       (add-to-list 'collection (list svalue (+ 1 (length collection))))))
    )
  )

;; Parses LHS  
(defun rule-compile-lhs (l hypo &optional rhs long-desc)
  "Compiles a rule.  The compilation trigger several side-effects, upating various globales: the forward-associations list, the list of signs, and the production memory and its counter.  It returns a list of dotted pairs '(set . ref)' for scoping set operators."
  (cond
   ;; Empty list? Done.
   ((null l) nil)
   ;; Check for form `(string= sign "some-string")' and accumulate completion
   ;; Maybe a registry of patterns would be more interesting here.
   ((and (eq 'string= (car l)) (atom (cadr l)) (stringp (caddr l)))
    (sign-update-collection (cadr l) (caddr l))
    (rule-compile-lhs (cdr l) hypo rhs long-desc)
    )
   ;; Check for grounded atom or operator? Skip.
   ((and (atom (car l)) (or (numberp (car l)) 
			    (stringp (car l))
			    (memq (car l) nclose-global-lhsops)))
    (rule-compile-lhs (cdr l) hypo rhs long-desc))
   ;; Check for sign/variable; create forward association, update signs list
   ((atom (car l)) 
    (sign-compile (car l) hypo)
    (rule-compile-lhs (cdr l) hypo rhs long-desc))
   ;; Check for set operator
   ((and (listp (car l)) (memq (caar l) nclose-global-lhs-setops))
    ;;    (debug nil (car l))
    (cons
     (cons (cadr (car l)) (car (cddr (car l))))
     (rule-compile-lhs (cdr l) hypo rhs long-desc)))
   ;; Recurse
   ((listp (car l))
    (append
     (rule-compile-lhs (car l) hypo rhs long-desc) 
     (rule-compile-lhs (cdr l) hypo rhs long-desc)))
   )
)

;; Parses RHS
(defun rule-compile-rhs (l hypo rhs long-desc)
  "Extracts signs from RHSes.  These are the lvalues of the @set operator"
  (cond
   ((null rhs) t)
   ;; Simple (@set lval rval) form
   ((and (memq (caar rhs) nclose-global-rhsops) (atom (car (cdar rhs))))
    (sign-compile (car (cdar rhs)) nil)
    (rule-compile-rhs l hypo (cdr rhs) long-desc)
    )
   ;; Accessor (@set (access ...) rval) form
   ((and (memq (caar rhs) nclose-global-rhsops)
	 (and (listp (car (cdar rhs))) 
	      (memq (caar (cdar rhs)) nclose-global-set-accessors)))
    (rule-compile-rhs l hypo (cdr rhs) long-desc))
   ;; RHS command as defined in `globales.el'
   ((memq (caar rhs) nclose-global-rhscommands)
    (rule-compile-rhs l hypo (cdr rhs) long-desc))
   ;; Syntax error if RHS is not (rhsops variable expression)
   (t (error (format "Syntax Error: RHS op %s undefined" (caar rhs))))
    )
)

;; Parses a rule definition
(defun scope-defs (class l current)
  "Returns the number of scopes for a class in a LHS"
  (cond
   ((null l) (cons class current))
   ((equal (caar l) class)
    (if (> (cdar l) current) 
	(scope-defs class (cdr l) (cdar l))
      (scope-defs class (cdr l) current)))
   (t (scope-defs class (cdr l) current))
   )
)

(defun all-scope-defs (l)
  "Extracts a compact representation of the scope information for all classes mentioned in the parsed LHS l as a list of dotted pairs ((quote Class) . maxref), where 'maxref' is the largest independent set reference"
  (mapcar '(lambda (class) (scope-defs (list 'quote class) l 0))
	  (remove-duplicates
	   (mapcar '(lambda (pair) (cadr (car pair))) l)))
)

(defun add-to-kb (l hypo &optional rhs long-desc)
  "Adds a rule to knowledge base and compiles it, updating count in the process."
  (let* ((parsed-lhs (rule-compile-lhs l hypo rhs long-desc))
	 (parsed-rhs (rule-compile-rhs l hypo rhs long-desc))
	 )
    (add-to-list 'nclose-global-hypos hypo)
    ;; In production memory, rules RHS and long descriptions are optional
    (setq nclose-global-pm 
	  (cons 
	   (cons nclose-global-pm-count 
		 (list l hypo (all-scope-defs parsed-lhs)
		       rhs long-desc))
	   nclose-global-pm))
    (setq nclose-global-pm-count (+ 1 nclose-global-pm-count))
    )
)

