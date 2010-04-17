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

