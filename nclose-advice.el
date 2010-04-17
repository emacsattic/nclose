;;
;; The NClosEmacs Project
;;
;; nclose-advice.el
;;
;; AOP approach to implementation of variants of the rule engine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE
;; Attempt at implementing rule evaluation using the (i) Lisp symbol binding
;; and (ii) aspect-oriented programming via the advising functions of ELisp.

  (defvar nclose-global-buffer-log nil)
  (defvar nclose-global-fwrd nil)

;; The log layer manages a simple trace buffer
(defun nclose-buffer-empty-log ()
  "Delete contents of log buffer"
  (save-current-buffer
    (set-buffer nclose-global-buffer-log)
    (erase-buffer)
    )
)

(defadvice nclose-use-nclos (after log-layer (nclos &optional ontology))
  "Logs the installation of new object system."
  (save-current-buffer
    (set-buffer nclose-global-buffer-log)
    (insert (format "[%s] Installing %s" (current-time-string) 
		    (if nclos nclos "default NClosEmacs object system")))
    (newline)
    )
)

(defadvice nclose-global-init (after log-layer ())
  "Creates a log buffer"
  (setq nclose-global-buffer-log (get-buffer-create "*nclose-log*"))
  (nclose-buffer-empty-log)
)

(defadvice nclose-reset-globales (after log-layer ())
  "Empty the log buffer"
  (nclose-buffer-empty-log)
)

(defadvice nclose-reset-session (after log-layer ())
  "Empty the log buffer"
  (nclose-buffer-empty-log)
)

(defadvice sign-writer (after log-layer (var val))
  "Logs a value assignment"
  (save-current-buffer
    (set-buffer nclose-global-buffer-log)
    (insert (format "[%s] Sign %s is set to %s" (current-time-string) var val))
    (newline)
    )
)

(defadvice property-writer (after log-layer (sym prop val))
  "Logs a property value assignment"
  (save-current-buffer
    (set-buffer nclose-global-buffer-log)
    (insert (format "[%s] Property %s of %s is set to %s"
		    (current-time-string) prop sym val))
    (newline))
)

(defadvice nclose-get-unification (after log-layer (pattern scopes))
  "Logs a LHS unification"
  (save-current-buffer
    (set-buffer nclose-global-buffer-log)
    (insert (format "[%s] Unification: %s"
		    (current-time-string) (car ad-return-value)))
    (newline))
)

(defadvice nclose-knowcess (before log-layer ())
  "Logs a session run"
  (save-current-buffer
    (set-buffer nclose-global-buffer-log)
    (insert (format "[%s] Starting Knowcess" (current-time-string)))
    (newline)
    )
)  

(defadvice nclose-eval-rules (before log-layer (hypo))
  "Logs evaluation of an hypothesis"
  (save-current-buffer
    (set-buffer nclose-global-buffer-log)
    (insert (format "[%s] Evaluating hypothesis %s" (current-time-string) 
		    hypo))
    (newline)
    )
)  

(defadvice nclose-rule-unify (before log-layer (lhs hypo scopes rhs &optional rule-info))
  "Logs evaluation of a rule"
  (save-current-buffer
    (set-buffer nclose-global-buffer-log)
    (insert (format "[%s] Unifying rule %s: %s" (current-time-string) 
		    (car rule-info) (cdr rule-info)))
    (newline)
    )
)  

(defadvice and-execute-rhs (before log-layer (rhs scopes &optional rule-info))
  "Logs execution of RHS"
  (save-current-buffer
    (set-buffer nclose-global-buffer-log)
    (insert (format "[%s] Firing RHS of rule %s: %s" (current-time-string) 
		    (car rule-info) (cdr rule-info)))
    (newline)
    )
)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; NOTE
;; Gating is implemented as an aspect on a base backward chaining process

(defun postpone-tasks (x)
  "Push tasks to be later executed based on the forward associations"
  (mapcar '(lambda (hypo) (add-to-list 'nclose-global-agenda hypo t))
	  (get nclose-global-fwrd x))
)

(defun plist-push (prop val)
  "Pushes a value onto a property list if not already present"
  (let ((stack (get nclose-global-fwrd prop)))
    (if (not (memq val stack)) 
	(put nclose-global-fwrd prop (cons val stack)) stack)
    )
)

(defadvice nclose-global-init (after gating-layer ())
  "Creates global forward association list"
  (setq nclose-global-fwrd nil)
)

(defadvice nclose-reset-globales (after gating-layer ())
  "Resets global forward association list"
  (setq nclose-global-fwrd nil)
  (setplist nclose-global-fwrd nil)
)

(defadvice sign-writer (after gating-layer (var val))
  "Post deferred evaluation tasks to the global agenda"
  (postpone-tasks var)
)

(defadvice sign-compile (after gating-layer (sign hypo))
  "Builds the forward association list, hypo is nil when RHS are compiled"
  (if hypo (plist-push sign hypo))
)

;;
(defun nclose-init-advices ()
  "Initializes all advising functions."
  (interactive)
;; Enable/Disable layers as required
  (ad-enable-regexp "gating-layer")
  (ad-enable-regexp "log-layer")

;; Activate advising functions
  (ad-activate-regexp "gating-layer")
  (ad-activate-regexp "log-layer")
)

(provide 'nclose-advice)



