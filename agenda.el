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
