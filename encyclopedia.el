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
   