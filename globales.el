;;
;; The NClosEmacs Project
;;
;; globales.el
;;
;; Declaration and initialization of global forms and constants.
;; See also: reset.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE
;; The globales are either global to the session, to the engine or to the ruleset.  We should really differentiate between these.  An option is to attach globales to an Emacs buffer, in effect instantiating an individual session with each knowledge base buffer.  Another option is to define a proper ruleset metaclass in MOP fashion.

;; This function is advised
(defun nclose-global-init ()
  "Global initialization of engine"
  (interactive)
  nil
)

;; Keywords for operators, set accessors/operators and commands
(defconst nclose-global-lhs-setops
  '(some-in all-in none-in oone-in))
(defconst nclose-global-set-accessors
  '(prop-in member-in))
(defconst nclose-global-lhsops 
  '(string= < > = /= >= <= + - * / and or not null yes no))
(defconst nclose-global-lhsops-composite
  '(and or not null)
  )
(defconst nclose-global-rhsops
  '(@set)
  ) 
(defconst nclose-global-rhscommands
  '(@show)
  )

;; Global list of signs/variables, working memory
(defvar nclose-global-signs nil)
(defvar nclose-global-hypos nil)
;; Used in default object system
(defvar nclose-global-sets nil)


;; Global production memory
(defvar nclose-global-pm-count 0)
(defvar nclose-global-pm nil)

;; Globales for Nclose's agenda
(defvar nclose-global-agenda nil)

;; Globales for ontology-based nclos
;; If the default object system is overriden, this global should be set.
;; Values could be: :OWL-LITE, and later :XTM, :EIEIO, etc.
(defvar nclose-global-nclos nil)
(defvar nclose-global-ontology nil)




