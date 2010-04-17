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
