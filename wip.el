(defun treetest ()
  (interactive)
  ;; create a new buffer
  (switch-to-buffer (tree-new-buffer "*TREE DEMO*"))
  (erase-buffer)
  (let* ((ckb (onto-classes nclose-global-ontology))
	 (parent (tree-set-root (tree-new-node "Thing")))
	 (full-tree (onto-subClassOf-inverse-hiterator 'Thing ckb ckb
	    '(lambda (node subt restt) (append (list node subt) restt))))
	 (nroot (tree-set-root (tree-new-node "Thing")))
	 )
    (tr nroot full-tree)
    
    )
  (tree-refresh-tree)
)


(setq ckb (onto-classes nclose-global-ontology))
(setq foo
  (onto-subClassOf-inverse-hiterator 'Thing ckb ckb
	'(lambda (node subt restt)
	   (append (list node subt) restt))))
foo
(Ca (Cb (Cc nil)) Cd (Ce nil Cf nil) Cg nil)

(defun tr (parent l)
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

(tr 'thing foo)
		 


	


