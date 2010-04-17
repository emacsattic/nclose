;;
;; The NClosEmacs Project
;;
;; set-instances.el
;;
;; NClosEmacs (simplified) object system
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REMARK - Underspecified as of today Fri Aug 15 18:09:51 2008
;; This is a barebone default implementation of the minimum set concepts required in Nexpert Object, namely object with scalar properties organized in sets ordered by a hierarchy of inclusions.  To this base structure, NO added an object only hierarchy (partOf/whole) and inheritability of scalar values in the set/class hierarchy, optionally in the object hierarchy.

;; NOTE
;; In this implementation which we look to override with the ontology implementations (OWL Lite or Topic Maps), the knowledge base file declares individual sets of objects, which are kept in the global variable 'nclose-global-sets'.  Each set symbol has a distinguished property ':instances' which is a list of its member objects; each object symbol keeps its properties and their values in its symbol's plist.

;; The articulation between the object system and the inference engine is limited to a few functions which act as an API.

;; We have one unique relation, ':instances, between sets and member objects.  Should we allow users to define their own 1-n relations?  In Nexpert Object we basically had two distinguished relations: class-to-instances and object-to-subobjects.  Look again into RDF/OWL for inspiration possibly.

;; Accessing individual named objects and their properties
(defun prop-in (instance prop)
  "External access to object properties"
  (property-reader instance prop)
)

;; These are functions resulting from RHS expansions: they return t or nil
(defun single-prop-write (obj prop rval)
  "RHS assign of a value to an individual object's property"
  (property-writer obj prop rval)
  t
)

(defun multi-prop-write (scopes rval class index prop)
  "RHS multiple assignment of value 'rval' to the property 'prop' of all objects in set 'class' number 'index' in scopes."
  ;;(debug nil scopes)
  (let* ((env 'env)
	 (ignore (setplist 'env scopes))
	 )
    (mapcar '(lambda (obj) (property-writer obj prop rval))
	    (aref (get 'env class) (- index 1)))
    t
    )
)

;; Used for encyclopedia and trace/listing functions
(defun find-all-objects (l)
  "Default object enumerator from list 'l' of classes."
  (cond
   ((null l) nil)
   (t (append (get (car l) ':instances) (find-all-objects (cdr l))))
   )
)

(defun enumerate-plist (l)
  "Prepares consed list of properties and values for trace/listing."
  (cond
   ((null l) nil)
   (t (append (list (cons (car l) (cadr l))) (enumerate-plist (cddr l))))
   )
)

(defun enumerate-slots (obj)
  "Enumerates conses of properties and values for passed object."
  (enumerate-plist (symbol-plist obj))
)

;; NOTE
;; On the RHS we need a set accessor, an individual object property accessor which can be used as lvalues. Suggestions:
;; (@SET (prop-in 'OBJECT 'PROPERTY) expression)
;; (@SET lvalue expression)
;; (@SET (member-in 'CLASS PATTERN-INDEX 'PROPERTY)  expression)
;; (@LINK/UNLINK (member-in 'CLASS PATTERN-INDEX) 'CLASS)

(defun add-to-kb-sets (class instances)
  "Defines a new set with the provided instances"
  (put class ':instances instances)
  (add-to-list 'nclose-global-sets class)
)