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
