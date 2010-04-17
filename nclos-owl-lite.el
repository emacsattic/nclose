;;
;; The NClosEmacs Project
;;
;; owl-lite.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Investigations around OWL Lite for NClosEmacs
;; Tue Aug 19 11:28:59 2008
;; required ontology.el

;; See: www.w3.org/TR/owl-guide

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ontology Musings
(require 'ontology)

;; REMARK
;; The object system here is quite different from the original object system in Nexpert Object.  Do we need 2 implementations for NClosEmacs : (i) one to emulate the original object system, and (ii) another for OWL Lite -- or possibly make the first one a variant of the second one in a MOP-way?  Which seems possible as the main difference resides in the notion of "property" which is first-class in OWL, and by far the more complex concept in the specification.

;; NOTE
;; The pivot functions in NClosEmacs which need an interface to the ontology are
;;  'property-reader' in set-unification.el,
;;  'property-witer' in same,
;;  'scope-acc' in unify.el, which supposes proper use of symbol's plist in
;;  'add-to-kb-sets' in set-instances.el
;;  Reset functions in reset.el
;;
;; The default implementation uses symbol's plist for most of the collection of information.  We simply need to replace 'add-to-kb-sets' and map ontology queries results onto the appropriate plist.

;; An ontology à la OWL Lite would then consist of several lists: classes and their related individuals, properties (both object and datatypes) and their attachments to individuals, which are called here "slots" for lack of a better name.

;; Another line of investigation would be to use Topic Maps (and XTM) concepts as the base for the NClosEmacs ontology system (nclos).  This would warrant a precise definition of the projection of these larger systems (OWL/RDF and XTM) actually required by the relatively low expressive power of the production rule system.(NERS, NEXS or NEPS?).

;; Introduce non-scalar range in the inference engine. [TODO]

;; There are 2 Faceted Classification relationship types: (i) semantic and (ii) syntactic.  A semantic relationship is independent of context and permanent.  They are further divided in 3 types:
;; - equivalence: synonyms, under the same category
;; - two types of hierarchical: superclass/subclass and whole/part, or mereological. (Both were present in Nexpert Object.)  Usual keywords "kind-of", "is-a" and "part-of", for instance.
;; - affinitive or aasociative: which usually are domain-specific (cause and effect, genetic, coordination, sequence, etc.) (In Nexpert Object we had the "context links".)
;; Syntactic relationships are ad hoc conjonctions, like keywords in a search box.

;; RDF statements as a bipartite graph: SUBJECT <-- PREDICATE --> OBJECT

;; Properties
;;   A domain of a property limits the individuals to which the property can be applied. If a property relates an individual to another individual, and the property has a class as one of its domains, then the individual must belong to the class. For example, the property hasChild may be stated to have the domain of Mammal. From this a reasoner can deduce that if Frank hasChild Anna, then Frank must be a Mammal. Note that rdfs:domain is called a global restriction since the restriction is stated on the property and not just on the property when it is associated with a particular class.
;;  The range of a property limits the individuals that the property may have as its value. If a property relates an individual to another individual, and the property has a class as its range, then the other individual must belong to the range class. For example, the property hasChild may be stated to have the range of Mammal. From this a reasoner can deduce that if Louise is related to Deborah by the hasChild property, (i.e., Deborah is the child of Louise), then Deborah is a Mammal. Range is also a global restriction as is domain above.

(onto-defstruct owl-property ((:finder . id)) id domain range)

;; This expands in a few functions
;;   - Function: owl-property! id-qname domain-qname range-qname
;;   - Function: owl-property? item
;;   - Function: owl-property-id item
;;   - Function: owl-property-domain item
;;   - Function: owl-property-range item
;;   - Function: onto-find-owl-property id-qname items-list
;;   - Function: update-owl-id item value
;;   - Function: update-owl-domain item value
;;   - Function: update-owl-range item value
;; and similarly for the other structures herein defined.  The updates functions are setf-associated to the access functions so that the following invocation works as expected:
;;  (setf (owl-property-domain prop) 'Thing)

; A property is a binary relation. Two types of properties are distinguished:
;;    * datatype properties, relations between instances of classes and RDF literals and XML Schema datatypes (range is scalar)
;;    * object properties, relations between instances of two classes. 

(onto-defstruct owl-slot ((:finder . id)) id property domain-ind range-ind)

;; Scalars.  Is this required?
(onto-defstruct owl-scalar nil value)

;; Classes
;;  A class defines a group of individuals that belong together because they share some properties. For example, Deborah and Frank are both members of the class Person. Classes can be organized in a specialization hierarchy using  subClassOf. There is a built-in most general class named  Thing that is the class of all individuals and is a superclass of all OWL classes. There is also a built-in most specific class named  Nothing that is the class that has no instances and a subclass of all OWL classes.

(onto-defstruct owl-class ((:finder . id)) id subClassOf)

;; Individuals
;;  In addition to classes, we want to be able to describe their members. We normally think of these as individuals in our universe of things. An individual is minimally introduced by declaring it to be a member of a class.

(onto-defstruct owl-individual ((:finder . id)) id class)

;; Defines the class/subclass iterator for owl-class
(onto-defhiterator owl-class id subClassOf)
;; Now (onto-subClassOf-[direct|inverse]-hiterator 'Thing ckb ckb '(lambda (a b c) (list a b c))) is valid

;; TODOs
;; Namespace notion for class-, individual-, property- and slot-lists.  In OWL a object property is a relation between 2 individual instances of classes, while in NO it is a function from an object to a scalar value, a datatype property in OWL.

;; Examples: an ontology
(setq owl 
      (onto! 'owl
	     ;; OWL classes
	     (list
	      (owl-class! 'Winery 'Thing)
	      (owl-class! 'Region 'Thing)
	      (owl-class! 'ConsumableThing 'Thing)
	      (owl-class! 'Thing nil)
	      (owl-class! 'Glen 'Winery)
	      )
	     ;; OWL individuals
	     (list
	      (owl-individual! 'CentralCoastRegion 'Region)
	      (owl-individual! 'WestCoastRegion 'Region)
	      (owl-individual! 'EastCoastRegion 'Region)
	      (owl-individual! 'EagleCastle 'Winery)
	      (owl-individual! 'LaurelGlen 'Winery)
	      )
	     ;; OWL properties
	     (list
	      (owl-property! 'locatedIn 'Winery 'Region)
	      (owl-property! 'partOf 'Thing 'Thing)
	      )
	     ;; OWL statements or slots
	     (list
	      (owl-slot! 'f0 'locatedIn 'EagleCastle 'WestCoastRegion)
	      (owl-slot! 'f1 'partOf 'LaurelGlen 'EagleCastle)
	      )
	     )
      )

;; Some external functions
;;	   

;; An example of the use of an inverse hierachical iterator
(defun owl-instancep (ind-id class-id ilist clist)
  "Tests whether individual, identified by its id, is a class member in given ontology."
  (let ((ind-class-id (owl-individual-class (onto-find-owl-individual ind-id ilist))))
    (or (eq ind-class-id class-id)
	(onto-subClassOf-inverse-hiterator class-id clist clist
	  '(lambda (node subtree resttree) (or (eq ind-class-id node)
					       subtree
					       resttree
					       ))))
    )
)

(defun onto-owl-instancep (ind-id class-id ontology)
  "User-level call to class membership testing.  Both the individual and the class are identified by their 'id'."
  (owl-instancep ind-id class-id 
		 (onto-individuals ontology) (onto-classes ontology))
)

;;
;;
;;
(defun owl-ind-instancep (ind class-id ilist clist)
  "Tests whether this individual is a member of given class in given ontology."
  (let ((ind-class-id (owl-individual-class ind)))
    (or (eq ind-class-id class-id)
	(onto-subClassOf-inverse-hiterator class-id clist clist
	  '(lambda (node subtree resttree) (or (eq ind-class-id node)
					       subtree
					       resttree
					       ))))
    )
)

(defun onto-owl-ind-instancep (ind class-id ontology)
  "User-level call to class membership testing.  The individual is the full form but the class is identified by its id."
  (owl-ind-instancep ind class-id
		     (onto-individuals ontology) (onto-classes ontology))
)


;; Finding and listing instances of a given OWL class
(defun owl-instances (class-id l ilist clist)
  "Selects individuals in l which are member of class class-id from clist."
  (cond
   ((null l) nil)
   ((owl-ind-instancep (car l) class-id ilist clist)
    (cons (car l) (owl-instances class-id (cdr l) ilist clist)))
   (t (owl-instances class-id (cdr l) ilist clist))
   )
)

(defun onto-owl-instances (class-id ontology)
  "User-level enumerator of individuals (recursively) in a class."
  (owl-instances class-id
		 (onto-individuals ontology) (onto-individuals ontology)
		 (onto-classes ontology))
)

;; Property and slot functions: validators
(defun owl-valid-slot-domainp (slot plist ilist clist)
  "Validates the domain individual of given slot in ontology."
  (owl-instancep (owl-slot-domain-ind slot)
		 (owl-property-domain
		  (onto-find-owl-property (owl-slot-property slot) plist))
		 ilist clist)
)

(defun onto-owl-valid-slot-domainp (slot ontology)
  "User-level validation of domain individual for given slot."
  (owl-valid-slot-domainp slot
     (onto-properties ontology) (onto-individuals ontology)
     (onto-classes ontology))
)

(defun owl-valid-datatypep (slot)
  "Tests whether slot is a statement of a datatype property."
  (and (listp (owl-slot-range-ind slot))
	(owl-scalar? (owl-slot-range-ind slot)))
)

(defun owl-enumerate-slots (obj l)
  "Recursive enumeration of all slots the range of which is 'obj'."
  (cond
   ((null l) nil)
   ((equal obj (owl-slot-domain-ind (car l)))
    (cons
     (cons (owl-slot-property (car l)) 
	   (owl-scalar-value (owl-slot-range-ind (car l))))
     (owl-enumerate-slots obj (cdr l))))
   (t (owl-enumerate-slots obj (cdr l)))
   )
)



;;
;; The NClosEmacs Project
;;
;; owl-instances.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE
;; This file implements the articulation of OWL Lite with the NClosEmacs object system.

;; The required object system is a slanted view of the OWL (Lite) features.  In particular, in NClos an object inherits all properties defined in its hierarchy of classes, whereas OWL Lite separates property definitions and declarations (which we called slots) from individuals and classes.  This is another intresting source of variations (metaprotocol) for this NClosEmacs investigation: for instance what if NClos properties are no longer restricted to being datatype properties?  (Tracing back these ideas to Landin, Kiczales.)
;; Formally the typical NClos form 'O.p' then requires an individual 'O' attached to a class 'Co', both present in the ontology, and a datatype property with domain 'Co' and range scalar.  The effect of reading 'O.p' is to extract that scalar value from the list of slots in the current ontology -- creating one if absent, in accordance with the previous remark.  The effect of writing to 'O.p' is to replace the slot with the new one. (Note that all this mechanism also relies on the unicity of the slots in a given ontology.)

;; This calls for refactoring into onto-property-reader, onto-property-writer and onto-scope-acc functions subsuming the various NClos installations. (See: unify.el and set-unification.el

;; Think about performance issues.


(defun owl-set-slot-scalar (slot val)
  "Assigns scalar value to an OWL datatype property in the declaration."
  (if (null (owl-slot-range-ind slot))
      (setf (owl-slot-range-ind slot) (owl-scalar! val))
    ;; Should check for a datatype property here
    (setf (owl-scalar-value (owl-slot-range-ind slot)) val))
)

(defun owl-get-slot-scalar (slot)
  "Returns the scala value of the datatype OWL property in declaration."
  (owl-scalar-value (owl-slot-range-ind slot))
)

;; Tentatively, ids in the list of slots of the global ontology are composed of the object name and the property name, as both uniquely identify the actual slot in NClos.
(defun owl-property-reader (sym prop)
  "Reads the (scalar) value of the property of given 'sym' object in current ontology."
  (let* ((index (format "%s-%s" sym prop))
	 (slot (onto-find-owl-slot index
				  (onto-slots nclose-global-ontology))))
    (if slot
	(owl-get-slot-scalar slot)
      (read-minibuffer (format "What is the value of the %s of %s? " prop sym)))
    )
)

(defun owl-property-writer (sym prop val)
  "Assigns the (scalar) value of the property of given 'sym' object in current ontology."
  (let* 
      ((index (format "%s-%s" sym prop))
       (slot (onto-find-owl-slot index (onto-slots nclose-global-ontology))))
    (if slot (owl-set-slot-scalar slot val)
      (setf (onto-slots nclose-global-ontology)
	    (append (onto-slots nclose-global-ontology)
		    (list (owl-slot! index prop sym (owl-scalar! val)))))
      )
    )
)

;; Scoping initial environments for rules unification
;; specs is a list of ((quote class-name) . n)
(defun owl-scope-acc (specs env)
  "In OWL-Lite NClos, computes instance extensions of each class in specs."
  (loop for spec in specs do
	(let ((ext (mapcar 'owl-individual-id
	       (onto-owl-instances (cadar spec) nclose-global-ontology))))
	  (put env (cadar spec) (make-vector (cdr spec) ext)))
	)
)
;;
;; The NClosEmacs Project
;;
;; nclos-owl-epilog.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'nclos-owl-lite)
