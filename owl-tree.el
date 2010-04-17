;;
;; The NClosEmacs Project
;;
;; owl-tree.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Experimental work using the EIEIO libraries.
;; See: http://cedet.sourceforge.net/eieio.shtml

(require 'tree)

;; In fact, need to subclass the tree class provided by EIEIO.

(defun owl-tree-build (parent l)
  "Recursively parses a listed hierarchy, creating EIEIO trees."
  (cond
   ((null l) nil)
   (t
    (let* ((node (car l))
	   (child (car (cdr l)))
	   (temp (tree-new-node (format "%s" node)))
	   )
      (tree-add-child parent temp)
      (if child (tr temp child))
      (tr parent (cdr (cdr l)))
      )
    )
   )
)

(defun owl-tree-class (class)
  "Displays the ontology tree rooted in passed class."
  ;; create a new buffer
  (switch-to-buffer (tree-new-buffer "*nclose-owl-tree*"))
  (erase-buffer)
  (let* ((ckb (onto-classes nclose-global-ontology))
	 (full-tree (onto-subClassOf-inverse-hiterator class ckb ckb
	    '(lambda (node subt restt) (append (list node subt) restt))))
	 (nroot (tree-set-root (tree-new-node (format "%s" class))))
	 )
    (owl-tree-build nroot full-tree)
    
    )
  (tree-refresh-tree)
)

(defun nclose-owl-tree ()
  (interactive)
  "Simply displays the whole rooted ontology."
  (owl-tree-class 'Thing)
)

