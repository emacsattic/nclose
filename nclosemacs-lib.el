;;
;; The NClosEmacs Project
;;
;; nclose-prolog.el
;;
;; Main library prolog dependencies
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE
;; Tentative extensible nclos architecture
;; The `nclos-owl-lite.el' file is computed and compiled by the makefile.

(require 'nclos-owl-lite)
(require 'nclos-eieio)
;;
;; The NClosEmacs Project
;;
;; globales.el
;;
;; Declaration and initialization of global forms and constants.
;; See also: reset.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE
;; The globales are either global to the session, to the engine or to the ruleset.  We should really differentiate between these.  An option is to attach globales to an Emacs buffer, in effect instantiating an individual session with each knowledge base buffer.  Another option is to define a proper ruleset metaclass in MOP fashion.

;; This function is advised
(defun nclose-global-init ()
  "Global initialization of engine"
  (interactive)
  nil
)

;; Keywords for operators, set accessors/operators and commands
(defconst nclose-global-lhs-setops
  '(some-in all-in none-in oone-in))
(defconst nclose-global-set-accessors
  '(prop-in member-in))
(defconst nclose-global-lhsops 
  '(string= < > = /= >= <= + - * / and or not null yes no))
(defconst nclose-global-lhsops-composite
  '(and or not null)
  )
(defconst nclose-global-rhsops
  '(@set)
  ) 
(defconst nclose-global-rhscommands
  '(@show)
  )

;; Global list of signs/variables, working memory
(defvar nclose-global-signs nil)
(defvar nclose-global-hypos nil)
;; Used in default object system
(defvar nclose-global-sets nil)


;; Global production memory
(defvar nclose-global-pm-count 0)
(defvar nclose-global-pm nil)

;; Globales for Nclose's agenda
(defvar nclose-global-agenda nil)

;; Globales for ontology-based nclos
;; If the default object system is overriden, this global should be set.
;; Values could be: :OWL-LITE, and later :XTM, :EIEIO, etc.
(defvar nclose-global-nclos nil)
(defvar nclose-global-ontology nil)




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

;;
;; The NClosEmacs Project
;;
;; known.el
;;
;; Ordering patterns for evaluation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REMARK
;; The "known-first" ordering is somewhat characteristic of pattern unification
;; in NClose.  Because ultimately the final source of information about a 
;; sign is the user, to whom the question is in last resort asked, the order in
;; which such questions are popped up is relevant.
;; Patterns are ordered by moving subpatterns governed by a logical operator
;; (and, or) first if all their variables/signs are already known.  This relies
;; on the LISP evaluation of these logical operators which will work out the 
;; minimal set of forms to evaluate.

(defun known-order (u v)
  "known-first order: a bound sign before an unbound one"
  (cond
   ((boundp u) t)
   (t (if (boundp v) nil t)))
)

(defun known-tree (l)
  "Recursively checks if all variables in patters are known and returns true if so, nil otherwise"
  (cond
   ((null l) t)
   ((and (atom (car l)) (or (numberp (car l)) 
			    (stringp (car l))
			    (memq (car l) nclose-global-lhsops)))
    (known-tree (cdr l)))
   ((and (atom (car l))
	 (memq (car l) nclose-global-lhs-setops))
    t)
   ((atom (car l))
    (and (boundp (car l)) (known-tree (cdr l))))
   ((listp (car l))
    (and (known-tree (car l)) (known-tree (cdr l))))
   )
)

(defun known-sort (l)
  "Recursively sort test subpatterns in patterns according to the known-first order"
  (cond
   ((null (car l)) nil)
   ;; An unknown symbol is pushed to the end of the list
   ((and (symbolp (car l)) (not (memq (car l) nclose-global-lhsops)))
    (if (boundp (car l)) (cons (car l) (known-sort (cdr l)))
      (append (known-sort (cdr l)) (list (car l)))))
   ;; A known symbol (number, string)  is left as is
   ((atom (car l))
    (cons (car l) (known-sort (cdr l))))
   ;; A list which starts with a test operator: sort its arguments
   ((listp (car l))
    (if (known-tree (car l))
	(cons (car l) (known-sort (cdr l)))
        (if (memq (caar l) '(and or))
	    (append (known-sort (cdr l))
		    (list (cons (caar l) (known-sort (cdar l)))))
	    (append (known-sort (cdr l)) (list (car l)))
	)
    )
   )
  )
)

(defun known-predicate (l1 l2)
  "Known predicate: any bound form comes first, preserving order"
  (if (known-tree l1) t nil))

(defun bound-sort (l)
  "Sorts subpatterns in LHS pattern according to bound symbols they contain"
  (cond
   ((null l) nil)
   ((memq (car l) '(and or not)) 
    (cons (car l) (sort (mapcar 'bound-sort (cdr l)) 'known-predicate)))
   (t l)
   )
)



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
;;
;; The NClosEmacs Project
;;
;; reset.el
;;
;; Resetting interpreter and globales.
;; See also: globales.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nclose-reset-objlist (l)
  "Reset all properties of objects listed in l"
  (mapcar '(lambda (obj) (setplist obj nil)) l))

(defun nclose-reset-signs ()
  "Unbind all signs/variables"
  (interactive)
  (mapcar '(lambda (l) (makunbound (car (cddr l)))) nclose-global-pm)
  (mapcar 'makunbound nclose-global-signs)
  ;; Reset properties of all objects in all sets in default object system
  (nclos-slot-reset)
  (font-lock-fontify-buffer)
)

;; This function is advised
(defun nclose-reset-globales ()
  "Cleans intepreter to initial state"
  (interactive)
  (setq nclose-global-signs nil)
  (setq nclose-global-hypos nil)
  (setq nclose-global-sets nil)
  (setq nclose-global-nclos nil)
  (setq nclose-global-pm nil)
  (setq nclose-global-pm-count 0)
  (setplist nclose-global-pm nil)
  (setq nclose-global-agenda nil)
)

;; This function is advised
(defun nclose-reset-session ()
  "Cleans session's state"
  (interactive)
  (nclose-reset-signs)
  (setq nclose-global-agenda nil)
)

;;
;; The NClosEmacs Project
;;
;; agenda.el
;;
;; Agenda management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REMARK
;; The agenda is the major differentiating feature of the NClose rule system.
;; It is analogous to the execution stack in the classical machine architecture
;; and it drives the rules engine.  The agenda is implemented as a stack of 
;; hypotheses which may, however, be pushed at the bottom and at the top of 
;; the stack.  The latter are called are postponed hypotheses collected for
;; later evaluation.

;; Agenda abstract data type methods
(defun agenda-suggest (hypo)
  "Suggest hypothesis"
  (add-to-list 'nclose-global-agenda (find-hypo hypo))
)

(defun agenda-volunteer (x val)
  "Volunteers data in the form of a value for a symbol"
  (sign-writer (find-sign x) val)
)
  
(defun agenda-pop-task ()
  "Pops first hypothesis out of the agenda"
  (prog1 (car nclose-global-agenda) (setq nclose-global-agenda
					  (cdr nclose-global-agenda)))
)

(defun agenda-top-task ()
  "Returns top hypothesis in agenda"
  (car nclose-global-agenda)
)


;; Evaluating and unifying rules

(defun or-apply (l)
  (cond
   ((null l) nil)
   (t (or (car l) (or-apply (cdr l))))
  )
)

(defun or-eval-rules (l hypo)
  "Evaluates the ruleset for given hypothesis.  All rules leading to said hypothesis are evaluated unconditionnally.  The value of the hypothesis is the or-ed function of these evaluations."
  (or-apply 
   (mapcar 
    '(lambda (lhsrhs) 
       (nclose-rule-unify 
	(car (car lhsrhs))  ;; LHS
	hypo                ;; Hypothesis
	(cadr lhsrhs)       ;; Scopes
	(cdr (car lhsrhs))  ;; Optional RHS
	(cdr (cdr lhsrhs))  ;; Optional desc
	)) 
    l))
)

;; This function is advised
(defun nclose-eval-rules (hypo)
  "Evaluates all rules leading to the given hypothesis, triggering RHSes.  This is the elementary task posted by the suggest command."
  (sign-writer hypo
	(let ((ruleset (find-rules hypo nclose-global-pm)))
	  (or-eval-rules ruleset hypo))
  )
)

;; Interactive commands

;; This function is advised
(defun nclose-knowcess ()
  "Main NClose engine execution command.  Loops through the agenda and perform individual tasks until empty"
  (interactive)
  (let ((hypo (agenda-pop-task)))
    (while hypo
      (put hypo ':current t)
      (font-lock-fontify-buffer)
      ;; REMARK
      ;; Do not reevaluate if hypothesis is already bound.  This has to be reconsidered when implementing RESET or TMS/DDB aspects.  As is it prevents RHS to be fired more than once.
      (unless (boundp hypo) (nclose-eval-rules hypo))
      (put hypo ':current nil)
      (setq hypo (agenda-pop-task))
      ))
)

(defun nclose-suggest ()
  "Interactive suggest"
  (interactive)
  (agenda-suggest
   ;; (read-minibuffer (format "Suggest hypothesis? ")))
   (completing-read "Suggest hypothesis? "
     (let ((n 0))
       (mapcar (lambda (x)  (setq n (+ n 1)) (cons (symbol-name x) n)) 
	       nclose-global-hypos)))
   )
)

(defun nclose-volunteer ()
  "Interactive volunteer"
  (interactive)
  (agenda-volunteer
   ;;(read-minibuffer (format "Volunteer sign/data? "))
   (completing-read "Volunteer sign/data? "
     (let ((n 0))
       (mapcar (lambda (x) (setq n (+ n 1)) (cons (symbol-name x) n))
	       nclose-global-signs)))
   (read-minibuffer (format "Volunteer value? "))
   )
)
;;
;; The NClosEmacs Project
;;
;; rhs.el
;;
;; RHS execution
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Commands
;; TO DO: external commands need to adapt to OS and environment
(defun @show (str-filename)
  "Display a bitmap file by relying on the OS to call the appropriate viewer"
  (shell-command str-filename)
  )

;; REMARK
;; When the rvalue of the @set RHS operator contains unbound signs, it is evaluated as are LHS patterns and triggers the same side-effects (backward chaining and gating)
;; When the lvalue is already bound and should the rvalue be different from this binding, we could signal a DDB/TMS fail condition.

;; NOTE
;; Issue of the RESET operator still unresolved for now.

;; Some examples of valid @set syntax:
;; (@set a-variable 2)
;; (@set a-variable (+ var1 var2))
;; (@set (prop-in 'O1 'p1) 2)
;; (@set (member-in 'Ca 1 'p1) 2)
;; The macro expands to different forms according to its second argument.  In the case of references to the result of the LHS unification, such as with 'member-in', the list of objects is supposed to be in the 'scope' variable.  This variable must be set locally in the RHS evaluation function.
(defmacro @set (lval rval)
  (if (atom lval)
      (progn (sign-writer lval (cdr (nclose-get-unification rval nil))) t)
    (cond
     ((eq 'member-in (car lval))
      (append (list 'multi-prop-write 'scope rval) (cdr lval)))
     ((eq 'prop-in (car lval))
      (list 'single-prop-write (cadr lval) (cadr (cdr lval)) rval))
     ))
)

;; The core RHS progn-like evaluation function.  Note that it sets the 'scope' local variable.
(defun and-eval-rhs (rhs scopes)
  "Recursively execute the sequential execution of RHS forms"
  (cond
   ((null rhs) t)
   (t (and (let ((scope scopes))
	     (eval (car rhs)))
	   (and-eval-rhs (cdr rhs) scopes)))
   )
)

;; This function is advised
(defun and-execute-rhs (rhs scopes &optional rule-info)
  "High-level call to sequential execution of RHS"
  (and-eval-rhs rhs scopes)
)
;;
;; The NClosEmacs Project
;;
;; encyclopedia.el
;;
;; Text presentation layer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nclose-print-wm ()
  "Simple textual presentation of working memory"
  (interactive)
  (insert (format "%-32s %s\n" 'Sign 'Value))
  (mapcar '(lambda (sign) 
	       (insert (format "%-32s %s\n" sign
		       (if (boundp sign) (eval sign) ":UNKNOWN"))))
	  nclose-global-signs)
  )

(defun nclose-print-rulehypos ()
  "Simple textual presentation of hypotheses"
  (interactive)
  (insert (format "Hypo\tValue\n"))
  (mapcar '(lambda (l)
	     (let ((sign (car (cddr l))))
	       (insert (format "%s:\t%s\n" sign
			       (if (boundp sign) (eval sign) ":UNKNOWN")))
	       )
	     )
	  nclose-global-pm
	  )
)

(defun nclose-print-hypos ()
  "Simple textual presentation of hypotheses"
  (interactive)
  (insert (format "%-32s %s\n" 'Hypothesis 'Value))
  (mapcar '(lambda (hyp)
	       (insert (format "%-32s %s\n" hyp
			       (if (boundp hyp) (eval hyp) ":UNKNOWN")))
	       )
	  nclose-global-hypos
	  )
  )



;; Additional functions to report on object system
(defun nclose-print-instances (class)
  "Simple textual representation of (recursively) all instances of class."
  (interactive
   (list (intern-soft 
	  (completing-read "Select class? " (nclos-classes-completing))))
   )
  ;; class has to be a symbol
  (insert (format "Class: %s\n" class))
  (mapcar '(lambda (obj) (insert (format "%s\n" obj)))
	  (nclos-find-instances class))
)


;; In certain nclos ontologies objects are symbols (OWL-LITE), in others (EIEIO) the string names are used instead.
(defun nclose-print-object (obj)
  "Simple textual representation of (recursively) all instances of class."
  (interactive
   (let ((sym (completing-read "Select object? " (nclos-objects-completing)))
)
     (list (or (intern-soft sym) sym))
     )
   )
  ;; `obj' has to be a symbol or its name as a string
  (insert (format "Object Individual: %s\n" obj))
   (mapcar '(lambda (ind) (insert (format "%s: %s\n" (car ind) (cdr ind))))
 (nclos-find-slots obj))
)
   ;;
;; The NClosEmacs Project
;;
;; set-instances.el
;;
;; NClosEmacs (simplified) object system
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REMARK - Underspecified as of today Fri Aug 15 18:09:51 2008
;; This is a barebone default implementation of the minimum set concepts required in Nexpert Object, namely object with scalar properties organized in sets ordered by a hierarchy of inclusions.  To this base structure, NO added an object only hierarchy (partOf/whole) and inheritability of scalar values in the set/class hierarchy, optionally in the object hierarchy.

;; NOTE
;; In this implementation which we look to override with the ontology implementations (OWL Lite or Topic Maps), the knowledge base file declares individual sets of objects, which are kept in the global variable 'nclose-global-sets'.  Each set symbol has a distinguished property ':instances' which is a list of its member objects; each object symbol keeps its properties and their values in its symbol's plist.

;; The articulation between the object system and the inference engine is limited to a few functions which act as an API.

;; We have one unique relation, ':instances, between sets and member objects.  Should we allow users to define their own 1-n relations?  In Nexpert Object we basically had two distinguished relations: class-to-instances and object-to-subobjects.  Look again into RDF/OWL for inspiration possibly.

;; Accessing individual named objects and their properties
(defun prop-in (instance prop)
  "External access to object properties"
  (property-reader instance prop)
)

;; These are functions resulting from RHS expansions: they return t or nil
(defun single-prop-write (obj prop rval)
  "RHS assign of a value to an individual object's property"
  (property-writer obj prop rval)
  t
)

(defun multi-prop-write (scopes rval class index prop)
  "RHS multiple assignment of value 'rval' to the property 'prop' of all objects in set 'class' number 'index' in scopes."
  ;;(debug nil scopes)
  (let* ((env 'env)
	 (ignore (setplist 'env scopes))
	 )
    (mapcar '(lambda (obj) (property-writer obj prop rval))
	    (aref (get 'env class) (- index 1)))
    t
    )
)

;; Used for encyclopedia and trace/listing functions
(defun find-all-objects (l)
  "Default object enumerator from list 'l' of classes."
  (cond
   ((null l) nil)
   (t (append (get (car l) ':instances) (find-all-objects (cdr l))))
   )
)

(defun enumerate-plist (l)
  "Prepares consed list of properties and values for trace/listing."
  (cond
   ((null l) nil)
   (t (append (list (cons (car l) (cadr l))) (enumerate-plist (cddr l))))
   )
)

(defun enumerate-slots (obj)
  "Enumerates conses of properties and values for passed object."
  (enumerate-plist (symbol-plist obj))
)

;; NOTE
;; On the RHS we need a set accessor, an individual object property accessor which can be used as lvalues. Suggestions:
;; (@SET (prop-in 'OBJECT 'PROPERTY) expression)
;; (@SET lvalue expression)
;; (@SET (member-in 'CLASS PATTERN-INDEX 'PROPERTY)  expression)
;; (@LINK/UNLINK (member-in 'CLASS PATTERN-INDEX) 'CLASS)

(defun add-to-kb-sets (class instances)
  "Defines a new set with the provided instances"
  (put class ':instances instances)
  (add-to-list 'nclose-global-sets class)
);;
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


;;
;; The NClosEmacs Project
;;
;; nclos.el
;;
;; Simple API to articulate the object and the rule systems
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE - Sat Nov 15 10:29:42 2008
;; We've switched to a still experimental modular architecture for nclos.  The default nclos is still part of the core nclosemacs library and implemented in 'set-instances.el' and 'set-unification.el'.  This file, 'nclos.el', adapts an ontology model to the simpler set-theoretic requirement of the NClose inference engine.  It is part of the core library but may refer to self-contained ontology model implementations such as 'nclos-owl-lite'.
;; In this preliminary version it takes care of:
;; - installing a given nclos and concrete ontology
;; - handling a cache of initial slots (valued properties) which may be part of the concrete ontology
;; - querying and updating the concrete ontology for set of elements (classes, objects...) and values of slots (property readers and writers)
;; Although not yet defined with a clean API, most of the implementation of these services are ontology-specific and are expected to be found in the respective 'nclos-<ONTO>.el' files that are required at the top of this file.  (The ontology-specific files may in turn require the generic 'ontology.el' module which contains macros for the specification of ontologies as 4 sets (classes, objects, properties and slots).

;; REMARK
;; In Nexpert Object we also had the "Order Of Sources" and "If Change Actions" that were similar to the before- and after- advising functions in Emacs, for instance.  They are of course reminiscent of the Flavors system. (See: http://p-cos.blogspot.com/2007/12/origin-of-advice.html for a historical review.)

;(require 'nclos-owl-lite)
;(require 'nclos-eieio)

;; Reinstalls the cached (if any) slots when resetting a session.
(defun nclos-slot-reset ()
  "Resets all slots in the object system."
    (cond
     ((equal ':OWL-LITE nclose-global-nclos)
      (setf (onto-slots nclose-global-ontology) 
	    (get 'nclose-global-ontology ':ONTO-INIT-SLOTS)
	    ))
     ((equal ':EIEIO nclose-global-nclos)
      (nclos-eieio-reset nclose-global-ontology))
     (t (mapcar '(lambda (set) (nclose-reset-objlist (get set ':instances)))
		nclose-global-sets))
     )
)

;; In nclos, a knowledge base may specify initial slots (i.e. valued properties).  The slots and objects may change during a session, hence this initial state of affairs needs to be cached for use in later session resets.
(defun nclos-merge-initial-slots (ontology)
  "Merges the newly imported slots in ontology with the initial set of slots stored in the ontology cache."
  ;; For now, radically replaces the cache
  (put 'nclose-global-ontology
       ':ONTO-INIT-SLOTS
       (onto-slots ontology))
)

;; Simple object system API to get/set slots (i.e. object's properties)
(defun nclos-slot-reader (sym prop)
  "Get value of the slot identified by sym.prop, which may be unbound."
  (cond
   ((equal ':OWL-LITE nclose-global-nclos)
    (owl-property-reader sym prop)  ;; See `owl-instances.el'
    )
   ((equal ':EIEIO nclose-global-nclos)
    (eieio-property-reader sym prop) ;; See `nclos-eieio.el'
    )
   (t
    (let ((val (get sym prop)))
      (if val val
	(read-minibuffer (format "What is the value of the %s of %s? " prop sym)))))
   )
)

(defun nclos-slot-writer (sym prop val)
  "Assigns value 'val' to slot 'sym.prop', according to currently installed NClos.  Returns value."
  (cond
   ((equal ':OWL-LITE nclose-global-nclos)
    (owl-property-writer sym prop val)
    val
    )
   ((equal ':EIEIO nclose-global-nclos)
    (eieio-property-writer sym prop val)
    val
    )
   (t (put sym prop val))
   )
)

;; Used in interactive commands and in unification to access objects
(defun nclos-scope-acc (l env)
  "Called before rule evaluation this function accumulates instances of classes, passed in the scope specifications 'l', into the plist of 'env', according to the currently installed NClos."
  (cond
   ((equal ':OWL-LITE nclose-global-nclos) (owl-scope-acc l env))
   ((equal ':EIEIO nclose-global-nclos) (eieio-scope-acc l env))
   (t (default-scope-acc l env))
   )
)

(defun nclos-find-instances (class)
  "Returns list of instances of class."
  (cond
   ((equal ':OWL-LITE nclose-global-nclos) 
    (mapcar 'owl-individual-id 
	    (onto-owl-instances class nclose-global-ontology)))
   ((equal ':EIEIO nclose-global-nclos) 
    (eieio-find-instances class)
    )
   (t (get class ':instances))
   )
)

(defun nclos-completing (l)
  (let* ((n 0))
    (mapcar '(lambda (class) (setq n (+ 1 n)) (cons (format "%s" class) n))
	    l))
)

(defun nclos-find-all-classes ()
  "Retrieves the list of instances according to the currently installed NClos"
  (cond
   ((equal nclose-global-nclos ':OWL-LITE)
    (mapcar 'owl-class-id (onto-classes nclose-global-ontology)))
   ((equal nclose-global-nclos ':EIEIO)
    (eieio-find-all-classes nclose-global-ontology)
    )
   (t nclose-global-sets)
   )
)

(defun nclos-classes-completing ()
  (nclos-completing (nclos-find-all-classes))
)

(defun nclos-find-all-objects ()
  "Retrieves the list of individual objects (used mostly in completion)"
  (cond
   ((equal nclose-global-nclos ':OWL-LITE)
    (mapcar 'owl-individual-id (onto-individuals nclose-global-ontology))
    )
   ((equal nclose-global-nclos ':EIEIO)
    (eieio-find-all-objects nclose-global-ontology)
    )
   (t (find-all-objects nclose-global-sets))
   )
)

(defun nclos-objects-completing ()
  (nclos-completing (nclos-find-all-objects))
)

(defun nclos-find-slots (obj)
  "Retrieves slots of given object, and returns a list of consed prop . value."
  (cond
   ((equal nclose-global-nclos ':OWL-LITE)
    (owl-enumerate-slots obj (onto-slots nclose-global-ontology))
    )
   ((equal nclose-global-nclos ':EIEIO)
    (eieio-find-slots obj)
    )
   (t (enumerate-slots obj))
   )
)

;; Basic setup function for the chosen nclos object system, nil (default) installs a default implementation which misses most of the original NxpObj object system features -- it has no inheritance, no slot per se, no specific RHS operators at this stage -- but offers however some additional facilities in the closure of set expressions.
  (defun nclose-use-nclos (nclos &optional ontology)
    "Installs an object system, usually nil, ':OWL-LITE or ':TOPIC-MAPS with given ontology."
    (setq nclose-global-nclos nclos)
    (setq nclose-global-ontology ontology)
    ;; Side effects and initializations
  (cond
   ((equal ':OWL-LITE nclose-global-nclos)
    (nclos-merge-initial-slots ontology)
    )
   ((equal ':EIEIO nclose-global-nclos)
    (nclos-eieio-post-install ontology))
   ;; No default post-install
   (t nil)
   )
    )

;;
;; The NClosEmacs Project
;;
;; nclose-mode.el
;;
;; A major mode for NClosEmacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fontlocking
;; Define different faces for hypotheses according to their status: under evaluation, true, false or unknown (which defaults to default face)

(defface nclose-font-bold-current-hypo-face
    '((((class grayscale) (background light))
       :foreground "yellow" :weight bold)
      (((class grayscale) (background dark))
       :foreground "yellow" :weight bold)
      (((class color) (background light))
       :foreground "yellow" :weight bold)
      (((class color) (background dark))
       :foreground "yellow" :weight bold)
      (t :foreground "yellow" :weight bold))
  "Face used to highlight hypotheses under evaluation."
  :group 'nclose-font-highlighting-faces)

(defface nclose-font-bold-true-hypo-face
    '((((class grayscale) (background light))
       :foreground "green" :weight bold)
      (((class grayscale) (background dark))
       :foreground "green" :weight bold)
      (((class color) (background light))
       :foreground "green" :weight bold)
      (((class color) (background dark))
       :foreground "green" :weight bold)
      (t :foreground "green" :weight bold))
  "Face used to highlight hypotheses known true."
  :group 'nclose-font-highlighting-faces)

(defface nclose-font-bold-false-hypo-face
    '((((class grayscale) (background light))
       :foreground "red" :weight bold)
      (((class grayscale) (background dark))
       :foreground "red" :weight bold)
      (((class color) (background light))
       :foreground "red" :weight bold)
      (((class color) (background dark))
       :foreground "red" :weight bold)
      (t :foreground "red" :weight bold))
  "Face used to highlight hypotheses known false."
  :group 'nclose-font-highlighting-faces)

(defvar nclose-font-bold-true-hypo-face 'nclose-font-bold-true-hypo-face)
(defvar nclose-font-bold-false-hypo-face 'nclose-font-bold-false-hypo-face)
(defvar nclose-font-bold-current-hypo-face 'nclose-font-bold-current-hypo-face)

;; Fontification of hypotheses
(defun hypo-facespec (hypo-name)
  "Select font face according to hypothesis status/value"
  (let ((hypo (find-hypo hypo-name)))
    (if hypo
	(cond
	 ((and (boundp hypo) (symbol-value hypo))
	  nclose-font-bold-true-hypo-face)
	 ((and (boundp hypo) (null (symbol-value hypo)))
	  nclose-font-bold-false-hypo-face)
	 ((and (not (boundp hypo)) (get hypo ':current))
	  nclose-font-bold-current-hypo-face)
	 (t 'default)
	 )
      'default
    ))
)

(defun local-fontify-LHS ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (forward-list)
    (backward-list)
    (down-list)
    (if (looking-at "add-to-kb")
	(let* ((ignore (forward-list))
	       (end (point))
	       (ignore (backward-list))
	       (start (point))
	       )
	  (y-or-n-p
	   (format "%s " (add-text-properties start end
			       '(face font-lock-keyword-face))))
	
	  )
      (up-list)
      )
    )
)

;; Major mode
;; (regexp-opt '("add-to-kb" "add-to-kb-sets"))
;; (regexp-opt '("owl-class!" "owl-individual!" "owl-property!" "owl-slot!"))
;; (insert (regexp-opt '(":id" ":individuals" ":classes")))
;; (insert (regexp-opt '("nclose-defclass" "nclose-defindividuals")))



(defvar nclose-font-lock-keywords
  '(("\\(add-to-kb\\(?:-sets\\)?\\)" . font-lock-keyword-face)
    ("(@hypo[ ]+\\([a-zA-Z0-9\-]+\\))" 0 (hypo-facespec (match-string 1)))
    ;; OWL-LITE
    ("\\(:OWL-LITE\\)" 1 font-lock-constant-face t)
    ("\\(onto![ ]+\\)\'\\([a-zA-Z0-9\-]+\\)" 
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)) 
    ("owl-\\(?:\\(?:class\\|individual\\|property\\|slot\\)!\\)"
     . font-lock-keyword-face) 
    ;; EIEIO
    ("\\(:EIEIO\\)" 1 font-lock-constant-face t)
    ("\\(:id\\)" 1 font-lock-constant-face t)
    ("\\(:individuals\\)" 1 font-lock-constant-face t)
    ("\\(:classes\\)" 1 font-lock-constant-face t)
    ("nclose-defclass" . font-lock-keyword-face)
    ("nclose-defindividuals" . font-lock-keyword-face)
    )
  
  "Default expressions to highlight in nclose knowledge bases."
)

(define-derived-mode nclose-mode lisp-mode "NClose" 
  "A major mode for NClosEmacs."
  (setq font-lock-defaults '(nclose-font-lock-keywords))
  ;; Update keymap
  (define-key nclose-mode-map "\C-cs" 'nclose-suggest)
  (define-key nclose-mode-map "\C-cv" 'nclose-volunteer)
  (define-key nclose-mode-map "\C-ck" 'nclose-knowcess)
  (define-key nclose-mode-map "\C-cr" 'nclose-reset-session)
 )



;;
;; The NClosEmacs Project
;;
;; nclose-epilog.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'nclosemacs-lib)
