;;
;; The NClosEmacs Project
;;
;; nclose.el
;;
;; Main library
;; Sun Jun 01 16:32:42 2008  Started work (Paris)
;; Tue Aug  5 22:07:45 2008  Branched out "objects" (Winchester)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An experimental implementation of NClose in ELisp for Emacs

;; This implementation relies on the now well understood formalization of the agenda, the Nclose control structure for goal evaluation tasks, as a bidirectional stack.  This implementation is also a first experiment with aspect-oriented programming concepts developed during the 90s.  Interestingly, these intervening ideas although not pertaining to AI research per se, originally stemmed from practical work on Common Lisp and, more specifically, on CLOS, its object-oriented extension.

;; Being based on Lisp, the implementation relies heavily on the underlying Lisp evaluator.  All signs, including hypotheses, are Lisp symbols: bound symbols are known, unbound symbols are unknown.  Rules' LHSes are more general patterns than the original NClose AND forms, as they are simply expressed as Lisp forms mixing signs and a defined set of operators on a few admissible types: boolean, number and strings.

;; The AOP experiments are done through the ELisp advising functions which offer the basic "before", "after" and "around" modifiers.  This is still pretty much work in progress.

;; On a personal note, this is probably my sixth implementation of the NClose project.  The first (rudimentary) one was hidden in the Philips Scientific award memo; the second one was created at CMU in 1982 and 1983 for the Technical Report and ran on DEC-20 Maclisp ; a third one was developped on the LeLisp system at Polytechnique in 1982-83, together with a fourth one on Symbolics LM2 Lisp Machines at the Centre Mondial ; the fifth one, of course, constituted the core (in Pascal/Classcal) of the original Mac-based Nexpert in 1984-85.

;; It is also an exercise in "nostalgic programming", a statement to be understood in two different, complementary meanings.  On the one hand, with an objective of revisiting previous program developments which left lasting memories, it is a welcome opportunity to investigate modern methods that were unavailable or uncommon at the time.  It caters, on the other hand, to the self-serving feeling that older programming skills are still at hand -- a point which is most likely to be derided by a lucid reading of the following implementation!

;;(add-to-list 'load-path "C:/Documents and Settings/jmc/Mes documents/plans/elisp")
;;(add-to-list 'load-path "/cygdrive/c/Documents and Settings/jmc/Mes documents/plans/elisp")
(add-to-list 'load-path "/home/jmc/elisp")
(add-to-list 'load-path "/cygdrive/c/code/elisp")

(load "globales")
(load "rules")
(load "gating")
(load "known")
(load "unify")
(load "reset")
(load "agenda")
(load "rhs")
(load "encyclopedia")
(load "nclose-mode")

(load "set-instances")
(load "set-unification")

(load "ontology")
(load "owl-lite")
(load "owl-instances")

(load "nclos")

(load "nclose-advice")


;; Basic setup function for the chosen NClos object system, nil (default) installs a default implementation which misses most of the original NxpObj object system features -- it has no inheritance, no slot per se, no specific RHS operators at this stage -- but offers however some additional facilities in the closure of set expressions.
(defun nclose-use-nclos (nclos &optional ontology)
  "Installs an object system, usually nil, ':OWL-LITE or ':TOPIC-MAPS with given ontology."
  (setq nclose-global-nclos nclos)
  (setq nclose-global-ontology ontology)
)

;; This, being advised, is somewhat necessary as the first command
(nclose-init-advices)
(nclose-global-init)



;; TO DO LIST
;; - Enrich RHS action forms
;; - Augment working memory with simple class/object system.  Ad-hoc or Closette-inspired system?  AOP-based implementation with advising functions?
;; - Extend NClose Emacs major-mode with font-locking and keymaps
;; - Comprehensive TexInfo documentation (from Muse source)
;; - Exemple AOP extensions: (i) priorities; (ii) certainty factors with a choice of update functions such as Bayesian; (iii) tracing and explanations; (iv) machine learning; (v) RDF/OWL integration; (vi) TMS and dependency-driven backtracking; (vii) keep traces for compiling by traces a la Michael Franz

;; Sun Oct  5 14:49:23 2008
;; Complete the info file with nclos description
;; Syntax of LHS/RHS patterns
;; Build more tutorial examples
;; Work out the nclose mode




