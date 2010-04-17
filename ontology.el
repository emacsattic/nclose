;;
;; The NClosEmacs Project
;;
;; ontology.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE
;;  The infrastructure layer for concrete ontology implementations such as OWL Lite and Topic Maps.  Inspired from the basic CL and other object systems in Lisp, we start by macros for the creation, update and access of a data structure used to store objects and related information.
;; In this exploratory implementation, we simply use lists as storage.  (Considerations of size and performance may drive to hash-tables in the future.)
;; See:  dorophone.blogspot.com/2008/08/struquine-useful-lisp-trick.html

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros

;; Create various symbols from root strings
(eval-and-compile
  (defun onto-s-bang (sym) (intern (format "%s!" sym)))

  (defun onto-s-cat (sym1 sym2) (intern (format "%s-%s" sym1 sym2)))

  (defun onto-s-update (sym1 sym2) (intern (format "update-%s-%s" sym1 sym2)))

  (defun onto-s-ques (sym) (intern (format "%s?" sym)))

  (defun onto-s-finder (sym) (intern (format "onto-find-%s" sym)))
)

;; Creates the definition functions, accessors and setters
(defmacro onto-defstruct (name specs &rest slots)
  "Builds structure definitions from specifications.  Also makes finder, accessor and setter functions for each listed slot.  'specs' is an a-list with additional meta-information; for now it is either 'nil' or '((:finder . slot))' to indicate on which (unique) slot find/search should operate."
  (let*
      ((n-fields (length slots))
       (i 1)
       (out `(progn
	       ;; The 'struct!' definition function
	       (defun ,(onto-s-bang name) ,slots
		 (list ',(onto-s-bang name) ,@slots))
	       ;; The 'struct?' type check function
	       (defun ,(onto-s-ques name) (item)
		 (eq (car item) ',(onto-s-bang name)))))
       )
    ;; Makes 'onto-find-struct' functions on the index given in the specs a-list
    (let ((index (cdr (assq ':finder specs))))
      (if index
	  (setf out (append out
			    (list `(defun ,(onto-s-finder name) (item l)
				     (cond
				      ((null l) nil)
				      ((equal item (,(onto-s-cat name index) (car l))) (car l))
				      (t (,(onto-s-finder name) item (cdr l)))
				      )
				     )
				  )))))
    (loop for slot in slots do
	  ;; Make property accessors and setters
	  (setf out
		;; The 'struct-slot' accessor/getter function 
		(append out
			(list `(defun ,(onto-s-cat name slot) (item)
				 (elt item ,i)))))
	  (setf out
		;; The 'update-struct-slot' low-level setter function
		(append out
			(list `(defun ,(onto-s-update name slot) (item val)
				 (setf (elt item ,i) val)))))
	  (setf out
		;; The association of getter/setter for CL setf
		(append out
			(list `(defsetf ,(onto-s-cat name slot)
				 ,(onto-s-update name slot)))))
	  (setf i (+ 1 i)))
    (append out (list nil))
    )
)

;; Generic iterators
;;
(defun onto-s-iterator (mode hlink)
  (intern (format "onto-%s-%s-hiterator" hlink mode))
)

;; NOTE
;; An admittedly complex macro that builds a hirarchical iterator to navigate list of structures in depth-first fashion, funcalling an expression on each node along the way.

;; For instance, in order to implement a class/subclass hierarchy on the "owl-class" structure, we state first that 'id' holds the identity of owl-classes, and 'subClassOf' holds the identity of the parent class.  We need an iterator on a list of classes, say l, that recursively enumerates all inversely related classes in the list to a particular one.

;; Hence, enumeration of all subclasses of an owl-class in list l would be invoked with:
;;       (onto-subClassOf-direct/inverse-hiterator 'CLASS-ID l l expr)
;; where 'expr' is a lambda form called on each (recursively) matching class:
;;       (expr (node subtree-rooted-in-node rest-of-tree) ...)
;;
(defmacro onto-defhiterator (struct finder hlink)
  "Makes two so-called hierachical iterators for a tree of structures.  The 'finder' argument tells which slot of the structure holds its unique id for reference.  The 'hlink' argument tells which slot of the structure holds the id of its parent in the hierarchy.  Note that this simplistic specification may be used for all sorts of hierarchies such as subclass/class, child/parent or part/whole.

  The 'inverse' iterator work down (in the hierarchy) from a given parent id.  Its form is as follows:

    Function:  onto-<HLINK>-inverse-hiterator id struct-list struct-list expr

      The 'id' argument is the value of the finder of the parent from which the iteration should start.

      The two 'struct-list' arguments are derived from the environment ontology.  Both are copies of the list of all individuals over which iteration should take place.  It is understood that some individuals in this list are in the enumeration; others are not.  Recursion happens on the first of this list while the second one in untouched during recursion.

      The 'expr' is a Lisp form which is called for each enumerated individual and is passed three arguments:

        Function: iterated-expr match-id iteration-children iteration-siblings

          The 'match-id' is the finder slot of the enumerated individual.

          The 'iteration-children' is the result of calling recursively the hierarchical iterator on this enumerated individual with a new copy of the struct-list.

          The 'iteration-siblings' is the result of the tail recursive call of the hierarchical iterator on the rest of the current struct-list.

  The 'direct' iterator works up (in the hierarchy) from a given child, stopping when the hlink slot of the node is nil. (So, by convention, all hierarchies are actually rooted in 'nil'.)
  Neither the direct nor the inverse hierarchical iterator include the given start node in the result."
  `(progn
     (defun ,(onto-s-iterator "inverse" hlink) (finder-id l root expr)
       (cond
	((null l) nil)
	((eq finder-id (,(onto-s-cat struct hlink) (car l)))
	 (funcall expr
		  (,(onto-s-cat struct finder) (car l))
		  (,(onto-s-iterator "inverse" hlink)
		   (,(onto-s-cat struct finder) (car l))
		   root root expr)
		  (,(onto-s-iterator "inverse" hlink) 
		   finder-id (cdr l) root expr)
		  ))
	(t (,(onto-s-iterator "inverse" hlink) finder-id (cdr l) root expr))
	)
       )
     (defun ,(onto-s-iterator "direct" hlink) (finder-id l root expr)
       (cond
	((null finder-id) nil) ;; nil indicates root node
	((null l) nil)
	((eq finder-id (,(onto-s-cat struct finder) (car l)))
	 (funcall expr
		  (,(onto-s-cat struct hlink) (car l))
		  (,(onto-s-iterator "direct" hlink)
		   (,(onto-s-cat struct hlink) (car l))
		   root root expr)
		  (,(onto-s-iterator "direct" hlink) 
		   finder-id (cdr l) root expr)
		  ))
	(t (,(onto-s-iterator "direct" hlink) finder-id (cdr l) root expr))
	)
       )
     )
)

;; NOTE
;; We define a generic ontology concept as four collections of classes, instances, properties and property statements, or slots.  Most of the concrete ontology functions, such as queries, require these list as environment or argument.

;; What about nested/inherited ontologies?

(onto-defstruct onto ((:finder . id)) id classes individuals properties slots)

(provide 'ontology)