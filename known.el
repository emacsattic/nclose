;;
;; The NClosEmacs Project
;;
;; known.el
;;
;; Ordering patterns for evaluation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REMARK
;; The "known-first" ordering is somewhat characteristic of pattern unification
;; in NClose.  Because ultimately the final source of information about a 
;; sign is the user, to whom the question is in last resort asked, the order in
;; which such questions are popped up is relevant.
;; Patterns are ordered by moving subpatterns governed by a logical operator
;; (and, or) first if all their variables/signs are already known.  This relies
;; on the LISP evaluation of these logical operators which will work out the 
;; minimal set of forms to evaluate.

(defun known-order (u v)
  "known-first order: a bound sign before an unbound one"
  (cond
   ((boundp u) t)
   (t (if (boundp v) nil t)))
)

(defun known-tree (l)
  "Recursively checks if all variables in patters are known and returns true if so, nil otherwise"
  (cond
   ((null l) t)
   ((and (atom (car l)) (or (numberp (car l)) 
			    (stringp (car l))
			    (memq (car l) nclose-global-lhsops)))
    (known-tree (cdr l)))
   ((and (atom (car l))
	 (memq (car l) nclose-global-lhs-setops))
    t)
   ((atom (car l))
    (and (boundp (car l)) (known-tree (cdr l))))
   ((listp (car l))
    (and (known-tree (car l)) (known-tree (cdr l))))
   )
)

(defun known-sort (l)
  "Recursively sort test subpatterns in patterns according to the known-first order"
  (cond
   ((null (car l)) nil)
   ;; An unknown symbol is pushed to the end of the list
   ((and (symbolp (car l)) (not (memq (car l) nclose-global-lhsops)))
    (if (boundp (car l)) (cons (car l) (known-sort (cdr l)))
      (append (known-sort (cdr l)) (list (car l)))))
   ;; A known symbol (number, string)  is left as is
   ((atom (car l))
    (cons (car l) (known-sort (cdr l))))
   ;; A list which starts with a test operator: sort its arguments
   ((listp (car l))
    (if (known-tree (car l))
	(cons (car l) (known-sort (cdr l)))
        (if (memq (caar l) '(and or))
	    (append (known-sort (cdr l))
		    (list (cons (caar l) (known-sort (cdar l)))))
	    (append (known-sort (cdr l)) (list (car l)))
	)
    )
   )
  )
)

(defun known-predicate (l1 l2)
  "Known predicate: any bound form comes first, preserving order"
  (if (known-tree l1) t nil))

(defun bound-sort (l)
  "Sorts subpatterns in LHS pattern according to bound symbols they contain"
  (cond
   ((null l) nil)
   ((memq (car l) '(and or not)) 
    (cons (car l) (sort (mapcar 'bound-sort (cdr l)) 'known-predicate)))
   (t l)
   )
)



