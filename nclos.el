;;
;; The NClosEmacs Project
;;
;; nclos.el
;;
;; Simple API to articulate the object and the rule systems
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE - Sat Nov 15 10:29:42 2008
;; We've switched to a still experimental modular architecture for nclos.  The default nclos is still part of the core nclosemacs library and implemented in 'set-instances.el' and 'set-unification.el'.  This file, 'nclos.el', adapts an ontology model to the simpler set-theoretic requirement of the NClose inference engine.  It is part of the core library but may refer to self-contained ontology model implementations such as 'nclos-owl-lite'.
;; In this preliminary version it takes care of:
;; - installing a given nclos and concrete ontology
;; - handling a cache of initial slots (valued properties) which may be part of the concrete ontology
;; - querying and updating the concrete ontology for set of elements (classes, objects...) and values of slots (property readers and writers)
;; Although not yet defined with a clean API, most of the implementation of these services are ontology-specific and are expected to be found in the respective 'nclos-<ONTO>.el' files that are required at the top of this file.  (The ontology-specific files may in turn require the generic 'ontology.el' module which contains macros for the specification of ontologies as 4 sets (classes, objects, properties and slots).

;; REMARK
;; In Nexpert Object we also had the "Order Of Sources" and "If Change Actions" that were similar to the before- and after- advising functions in Emacs, for instance.  They are of course reminiscent of the Flavors system. (See: http://p-cos.blogspot.com/2007/12/origin-of-advice.html for a historical review.)

;(require 'nclos-owl-lite)
;(require 'nclos-eieio)

;; Reinstalls the cached (if any) slots when resetting a session.
(defun nclos-slot-reset ()
  "Resets all slots in the object system."
    (cond
     ((equal ':OWL-LITE nclose-global-nclos)
      (setf (onto-slots nclose-global-ontology) 
	    (get 'nclose-global-ontology ':ONTO-INIT-SLOTS)
	    ))
     ((equal ':EIEIO nclose-global-nclos)
      (nclos-eieio-reset nclose-global-ontology))
     (t (mapcar '(lambda (set) (nclose-reset-objlist (get set ':instances)))
		nclose-global-sets))
     )
)

;; In nclos, a knowledge base may specify initial slots (i.e. valued properties).  The slots and objects may change during a session, hence this initial state of affairs needs to be cached for use in later session resets.
(defun nclos-merge-initial-slots (ontology)
  "Merges the newly imported slots in ontology with the initial set of slots stored in the ontology cache."
  ;; For now, radically replaces the cache
  (put 'nclose-global-ontology
       ':ONTO-INIT-SLOTS
       (onto-slots ontology))
)

;; Simple object system API to get/set slots (i.e. object's properties)
(defun nclos-slot-reader (sym prop)
  "Get value of the slot identified by sym.prop, which may be unbound."
  (cond
   ((equal ':OWL-LITE nclose-global-nclos)
    (owl-property-reader sym prop)  ;; See `owl-instances.el'
    )
   ((equal ':EIEIO nclose-global-nclos)
    (eieio-property-reader sym prop) ;; See `nclos-eieio.el'
    )
   (t
    (let ((val (get sym prop)))
      (if val val
	(read-minibuffer (format "What is the value of the %s of %s? " prop sym)))))
   )
)

(defun nclos-slot-writer (sym prop val)
  "Assigns value 'val' to slot 'sym.prop', according to currently installed NClos.  Returns value."
  (cond
   ((equal ':OWL-LITE nclose-global-nclos)
    (owl-property-writer sym prop val)
    val
    )
   ((equal ':EIEIO nclose-global-nclos)
    (eieio-property-writer sym prop val)
    val
    )
   (t (put sym prop val))
   )
)

;; Used in interactive commands and in unification to access objects
(defun nclos-scope-acc (l env)
  "Called before rule evaluation this function accumulates instances of classes, passed in the scope specifications 'l', into the plist of 'env', according to the currently installed NClos."
  (cond
   ((equal ':OWL-LITE nclose-global-nclos) (owl-scope-acc l env))
   ((equal ':EIEIO nclose-global-nclos) (eieio-scope-acc l env))
   (t (default-scope-acc l env))
   )
)

(defun nclos-find-instances (class)
  "Returns list of instances of class."
  (cond
   ((equal ':OWL-LITE nclose-global-nclos) 
    (mapcar 'owl-individual-id 
	    (onto-owl-instances class nclose-global-ontology)))
   ((equal ':EIEIO nclose-global-nclos) 
    (eieio-find-instances class)
    )
   (t (get class ':instances))
   )
)

(defun nclos-completing (l)
  (let* ((n 0))
    (mapcar '(lambda (class) (setq n (+ 1 n)) (cons (format "%s" class) n))
	    l))
)

(defun nclos-find-all-classes ()
  "Retrieves the list of instances according to the currently installed NClos"
  (cond
   ((equal nclose-global-nclos ':OWL-LITE)
    (mapcar 'owl-class-id (onto-classes nclose-global-ontology)))
   ((equal nclose-global-nclos ':EIEIO)
    (eieio-find-all-classes nclose-global-ontology)
    )
   (t nclose-global-sets)
   )
)

(defun nclos-classes-completing ()
  (nclos-completing (nclos-find-all-classes))
)

(defun nclos-find-all-objects ()
  "Retrieves the list of individual objects (used mostly in completion)"
  (cond
   ((equal nclose-global-nclos ':OWL-LITE)
    (mapcar 'owl-individual-id (onto-individuals nclose-global-ontology))
    )
   ((equal nclose-global-nclos ':EIEIO)
    (eieio-find-all-objects nclose-global-ontology)
    )
   (t (find-all-objects nclose-global-sets))
   )
)

(defun nclos-objects-completing ()
  (nclos-completing (nclos-find-all-objects))
)

(defun nclos-find-slots (obj)
  "Retrieves slots of given object, and returns a list of consed prop . value."
  (cond
   ((equal nclose-global-nclos ':OWL-LITE)
    (owl-enumerate-slots obj (onto-slots nclose-global-ontology))
    )
   ((equal nclose-global-nclos ':EIEIO)
    (eieio-find-slots obj)
    )
   (t (enumerate-slots obj))
   )
)

;; Basic setup function for the chosen nclos object system, nil (default) installs a default implementation which misses most of the original NxpObj object system features -- it has no inheritance, no slot per se, no specific RHS operators at this stage -- but offers however some additional facilities in the closure of set expressions.
  (defun nclose-use-nclos (nclos &optional ontology)
    "Installs an object system, usually nil, ':OWL-LITE or ':TOPIC-MAPS with given ontology."
    (setq nclose-global-nclos nclos)
    (setq nclose-global-ontology ontology)
    ;; Side effects and initializations
  (cond
   ((equal ':OWL-LITE nclose-global-nclos)
    (nclos-merge-initial-slots ontology)
    )
   ((equal ':EIEIO nclose-global-nclos)
    (nclos-eieio-post-install ontology))
   ;; No default post-install
   (t nil)
   )
    )

