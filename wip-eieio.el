;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(require 'eieio-base)

(list (list ':tracking-symbol 'foo))
(defvar foo)
(defclass a (eieio-instance-tracker) ((tracking-symbol :initform foo)))


(insert (format "%s" (class-v b)))
[defclass b (eieio-instance-tracker) nil [0 0 0] nil nil nil [] nil nil nil nil nil (tracking-symbol) (The symbol used to maintain a list of our instances.
The instance list is treated as a variable, with new instances added to it.) [symbol] (nil) (nil) ((default)) (nil) [b--instances] [object b default-cache-object] (:custom-groups (default))]
[defclass b (eieio-instance-tracker) nil [0 0 0] nil nil nil [] nil nil nil nil nil (tracking-symbol) (The symbol used to maintain a list of our instances.
The instance list is treated as a variable, with new instances added to it.) [symbol] (nil) (nil) ((default)) (nil) [foo] [object b default-cache-object] (:custom-groups (default))]
[defclass a (eieio-instance-tracker) nil [0 0 0] nil nil nil [] nil nil nil nil nil (tracking-symbol) (The symbol used to maintain a list of our instances.
The instance list is treated as a variable, with new instances added to it.) [symbol] (nil) (nil) ((default)) (nil) [foo] [object a default-cache-object] (:custom-groups (default))]

(defun onto-s-cat (sym1 sym2) (intern (format "%s-%s" sym1 sym2)))

(defmacro nclose-defclass (name superclass fields &rest options-and-doc)
  `(eval-and-compile
     (defvar ,(onto-s-cat name "instances") nil)
     (eieio-defclass ',name 
		     ',(append superclass (list 'eieio-instance-tracker))
		     ',(append fields 
			       (list
				(list 'tracking-symbol 
				      ':initform (onto-s-cat name "instances"))))
		     ',options-and-doc)
       )
  )

(defun get-instances (class)
  (symbol-value (onto-s-cat class "instances")))



(get-instances b)

(defun get-all-instances (class)
  (cond
   ((null (class-children class)) (get-instances class))
   (t (append
	      (mapcar 'get-all-instances (class-children class))
	      (get-instances class)))
   )
  )


(defun list-children (class-list)
  (cond
   ((null class-list) nil)
   (t (append (list-subclasses (car class-list)) 
	      (list-children (cdr class-list))))
   )
  )

(defun list-subclasses (class)
  (append (list class) (list-children (class-children class)))
  )

(mapcar 'get-instances (list-subclasses b))

(delete-instance b2)

(get-all-instances b)

(macroexpand '(nclose-defclass b () ()))
(nclose-defclass b () ())

(macroexpand '(defvar foo))

(defvar c-instances nil)
(setq b1 (b "track1"))
(setq b2 (b "track2"))
(nclose-defclass c ( b) ())
b-instances

(setq c1 (c "track3"))
c-instances
(nclose-defclass a () ())
a-instances
(nclose-defclass d (c a) ())
(nclose-defclass e (b) ())
(nclose-defclass f (c) ())
(setq d1 (d "track5"))
(class-children b)