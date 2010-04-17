;;
;; The NClosEmacs Project
;;
;; nclos-eieio.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Investigations around EIEIO for NClosEmacs
;; Sun Feb 22 10:44:14 2009
;; See: cedet.sourceforge.net/info/eieio.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The nature of the integration of CLOS, and CLOS-inspired object systems
;; into NClose differs from the embedding into Semantic Web based ontologies.
;; (See: The Art of the Metaobject Protocol, by Kiczales, Desrivieres and
;; Bobrow.)
;; The original system object basis in NXP relies on basic frame systems 
;; ideas that were popular in AI in the early '80s.  These ideas, in turn, 
;; fanned into object-oriented programming languages and environments, and
;; into ontology/knowledge representation systems -- both having different
;; purposes and ultimately serving different needs.
;; As far as NClose is concerned, the requirement is a simple mapping to a 
;; set theoretical view of classes and objects plus inheritance.  This is 
;; all the more evident in integrating CLOS-inspired object systems where
;; we need to enclose the CLOS classes and objects that constitute the basic
;; operational set-theoretical view into a NClose "namespace", a trivial
;; ontology.  The external integration of the CLOS extension to the Lisp
;; workspace also allows CLOS generic functions and slots to be used inside
;; rules LHS and RHS expressions, as NClose sill relies on the Lisp eval.
;; (Hence the EIEIO integration should be similar to Emacs' cl integration.)

;; Implementation of NClose basic ontology as a class/object in CLOS a la 
;; MOP?

;; Hookup functions (ELisp terminology)
;; Definition macros and representation
;; Slot functions
;; Pattern-matching functions
;; Query functions
;; Major mode functions
;; Display functions (EIEIO-only)

(require 'eieio-base)
(require 'tree)

;; Ancillary pattern infrastructure for cloning
(defclass prototype-flavor () ()
  "This is an abstract class for clonable objects. See: Gamma, Design Patterns, PROTOTYPE, pp. 110 et sq."
  )

(defmethod clone ((proto prototype-flavor))
  "Abstract clone method"
  (message "Cloning %s" (object-name proto))
  )

;; This is our highly dangerous cloning method, which is an instantiating function rather than really a cloning function.  The pattern is closer to FACTORY than to PROTOTYPE in fact.
(defun spec-clone (form)
  "Evaluates form which should be an object definition `(class name attributes-value)'"
  (eval form))

(defclass prototype-objlist-flavor (prototype-flavor)
  ((objlist :initarg :working-memory 
	   :documentation "The list of clonable objects")
   )
  "Returns a list of clones of its list of prototypes"
  )

(defmethod clone ((proto prototype-objlist-flavor))
  "Returns a list of clones of its objlist"
  (mapcar 'spec-clone (oref proto :working-memory))
  )

;;  Install proper subclassed slot for working memory specification
;;  The `:working-memory' slot contains a list of object definitions to be evaluated at clone time, to properly update the instance tracker
(defmacro nclose-defindividuals (&rest objlist)
  "External keyword for specifying a list of individual objects in working memory.  Inserts proper clonable list flavor."
  (prototype-objlist-flavor "Working-Memory" :working-memory objlist)
  )

(defun nclos-eieio-reset (onto)
  "Resets all slots in knowledge base to their original ontological defs."
  (mapcar 'delete-instance (oref onto :objects))
  (nclos-eieio-post-install onto)
  )

(defun nclos-eieio-post-install (onto)
  "Set up working memory as a clone of prototype objects. Also used at reset."
  ;; TO DO: some type checking
  (oset onto :objects (clone (oref onto :individuals)))
  )


;; Some utilities.  Maybe redundant with `ontology.el'
(defun util-s-cat (sym1 sym2) (intern (format "%s-%s" sym1 sym2)))

;; This specific `defclass' macro attach a nclos class to `eieio-instance-tracker' in order to automatically track instances when they are creates/deleted.  (This looks like the `metaclass' attribute in CLOS.)  It uses the low-level function which defines classes in EIEIO.
;; For each nclos class, a specific variable `class-instances' is created in the global environment to keep track of the current set of instances.  This variable is the `tracking-symbol' of the subclass of `instance-tracker'.
;; TODO: Think of a metaprotocol to do this in a generic way when adding other aspects (e.g. persistent storage)
;; NClos classes are otherwise handled as are EIEIO classes (slots, inheritance, generic/methods, etc.)
(defmacro nclose-defclass (name superclass fields &rest options-and-doc)
  "Defines a NClose class for use in the knowledge base.  These classes are otherwise normal EIEIO classes, with additional structure to integrate with the inference engine LHS patterns and RHS commands."
  `(eval-and-compile
     (defvar ,(util-s-cat name "instances") nil)
     (eieio-defclass ',name 
		     ;; Attach to a different metaclass
		     ',(append superclass (list 'eieio-instance-tracker
						))
		     ;; Add the required init forms for metaclass-inherited
		     ',(append fields 
			       (list
				(list 'tracking-symbol 
				      ':initform (util-s-cat name "instances"))))
		     ',options-and-doc)
       )
  )

;; Access and introspection functions
(defun nclos-eieio-instances (class)
  "Returns a list of the class instances, one level deep."
  (symbol-value (util-s-cat class "instances")))

;; The following pair of functions recursively call each other to produce a list of all subclasses of a given class, including itself.
(defun nclos-eieio-list-children (class-list)
  "Add the list of subclasses of first class in `class-list' to the list of remaining classes (breadth-first)."
  (cond
   ((null class-list) nil)
   (t (append (nclos-eieio-list-subclasses (car class-list)) 
	      (nclos-eieio-list-children (cdr class-list))))
   )
  )

(defun nclos-eieio-list-subclasses (class)
  "Produces the list of subclasses of passed class in depth-first search."
  (append (list class) (nclos-eieio-list-children (class-children class)))
  )

;; Now ready to access all instances of a class including instances od its subclasses
(defun nclos-eieio-all-instances (class)
  "Returns the list of all instances of passed class, recursively if needed."
  (mapcar 'nclos-eieio-instances
	  (nclos-eieio-list-subclasses class))
  )

(defun map-append (l)
  "Appends all sublists in list."
  (cond
   ((null l) nil)
   (t (append (car l) (map-append (cdr l))))
   )
  )

;; Experimental representation syntax, inspired loosely from OWL-Lite.
(defclass eieio-ontology ()
  ((onto-id
    :initarg :id
    :documentation "The unique name of the NClose class."
    )
   (onto-classes
    :initarg :classes
    :documentation "The list of CLOS/EIEIO class in the NClose namespace."
    )
   (onto-prototype-objects
    :initarg :individuals
    :documentation "The list of CLOS/EIEIO object prototype specifications in the Nclose namespace."
    )
   (onto-objects
    :initarg :objects
    :documentation "The list of CLOS/EIEIO object in the Nclose namespace."
    )
   )
  :documentation "The ontology (super)class."
  )



;; Implementation of the nclos interface

;; Reader and writer may be called with the object symbol, i.e. its EIEIO vector -- in the case of a class pattern -- or with its name -- in the case of a RHS `@SET' for instance.
(defun find-vector-object (name)
  "Return vectorized EIEIO object from its name field"
  (let ((objl (slot-value nclose-global-ontology :objects))
	(objname (format "%s" name))
	)
    (while (and objl (not (equal objname (aref (car objl) object-name))))
      (setq objl (cdr objl))
      )
    (car objl)
    )
  )

(defun eieio-prop-reader (sym prop)
  "Ancillary property reader with completion"
  (if (slot-boundp sym prop)
      (slot-value sym prop)
    (read-minibuffer 
     (format "What is the value of %s of %s? " prop (object-name sym)))
    )
  )
  
(defun eieio-property-reader (sym prop)
  "Reads in the scalar value of `prop' in object `sym' in the current EIEIO ontology."
  (if (object-p sym) (eieio-prop-reader sym prop)
    (eieio-prop-reader (find-vector-object sym) prop))
  )
      

(defun eieio-property-writer (sym prop val)
  "Writes value `val' to `prop' in object `sym'."
  (if (object-p sym)
      (set-slot-value sym prop val)
    (set-slot-value (find-vector-object sym) prop val))
  )

;; Scoping initial environments for rule evaluation.
;; `specs' is a list of ((quote class-name) . n) forms.
(defun extract-class (spec)
  "Extracts the symbol for class name from individual specification."
  (cadar spec)
  )

;; Tail recursive implementation of initial extensions environment
(defun eieio-scope-acc (specs env)
  "Initializes the initial extensions for rules evaluation in EIEIO."
  (cond
   ((null specs) env)
   (t 
    (let ((ext (map-append 
		(nclos-eieio-all-instances (extract-class (car specs)))))
	    )
	(put env (extract-class (car specs)) (make-vector (cdar specs) ext)))
    (eieio-scope-acc (cdr specs) env)
    )
   )
  )

;; Encyclopedia functions
(defun eieio-find-all-classes (onto)
  "List all class symbols defined in current ontology."
  (mapcar '(lambda (class-spec) (elt class-spec 1)) (slot-value onto :classes))
  )

(defun eieio-find-all-objects (onto)
  "List all individual names (objects) defined in ontology."
  (mapcar 'object-name (slot-value onto :objects))
  )

(defun eieio-find-instances (class)
  "List all instances (recursively) of an EIEIO class. `class' is a symbol."
  (mapcar 'object-name (map-append (nclos-eieio-all-instances class)))
  )

;; The following function would benefit from slots of `ontology' class being hashed.
(defun find-object (objname onto)
  "Returns EIEIO object vector from name."
  (let ((objl (slot-value onto :objects))
	)
    (while (and objl (not (equal objname (object-name (car objl)))))
      (setq objl (cdr objl))
      )
    (car objl)
    )
  )

(defun eieio-find-slots (objname)
  "List conses of prop . values for slots of `obj'"
  (let ((obj (find-object objname nclose-global-ontology))
	)
    (mapcar '(lambda (x) 
	       (cons x (if (slot-boundp obj x) (slot-value obj x) ':UNKNOWN ))
	       )
	    (object-slots obj))
    )
  )

;; Experimental visual browser
;; Tue Mar 17 15:01:16 2009 -- First pass no-thrill implementation
;; Relies on the eieio class browser in `tree.el'.  We simply attach instances to the `x' command
(defmethod change-scope ((etn eieio-tree-node))
  "Experimental. Show instances in separate sub-buffer."
  (describe-instances (oref etn class))
  )

(defun describe-instances (class)
  "Experimental. Show instances in separate sub-buffer."
  (interactive
   (list (intern-soft 
	  (completing-read "Select class? " (nclos-classes-completing))))
   )
  (with-output-to-temp-buffer "*nclose-instances*"
    (princ "Instances of ") (prin1 class) (terpri)
    (mapcar 
     '(lambda (obj) (prin1 obj) (terpri))
     (eieio-find-instances class)
     )
    )
  )

(defun nclose-browse-class (class)
  "Browse class network in a separate buffer. `ENTER' to select, `e' to edit, and `x' to change scope."
  (interactive
   (list (intern-soft 
	  (completing-read "Select class? " (nclos-classes-completing))))
   )
  (eieio-class-tree class)
)



;;
(provide 'nclos-eieio)


   
  




