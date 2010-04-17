;;
;; The NClosEmacs Project
;;
;; muse-helper.el
;;
;; Support for Emacs Muse integration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *muse-kbs-loaded* nil)

(defun nclose-loadkb (kb-file)
  "Loads a knowledge base and keeps track of files."
  (cond
   ((member kb-file *muse-kbs-loaded*) *muse-kbs-loaded*)
   (t (if (load kb-file) 
	  (add-to-list '*muse-kbs-loaded* kb-file) 
	*muse-kbs-loaded*)
      ))
  )

(defun nclose-slot-value (obj &optional prop)
  (if prop (nclos-slot-reader obj prop)
    (if (boundp obj) (eval obj) (sign-reader obj)))
  )

(defun nclose-hypo-value (hypo)
  (if (sign-reader hypo) "true" "false"))

(provide 'nclosemacs-muse-helper)
