;;
;; The NClosEmacs Project
;;
;; nclose-mode.el
;;
;; A major mode for NClosEmacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fontlocking
;; Define different faces for hypotheses according to their status: under evaluation, true, false or unknown (which defaults to default face)

(defface nclose-font-bold-current-hypo-face
    '((((class grayscale) (background light))
       :foreground "yellow" :weight bold)
      (((class grayscale) (background dark))
       :foreground "yellow" :weight bold)
      (((class color) (background light))
       :foreground "yellow" :weight bold)
      (((class color) (background dark))
       :foreground "yellow" :weight bold)
      (t :foreground "yellow" :weight bold))
  "Face used to highlight hypotheses under evaluation."
  :group 'nclose-font-highlighting-faces)

(defface nclose-font-bold-true-hypo-face
    '((((class grayscale) (background light))
       :foreground "green" :weight bold)
      (((class grayscale) (background dark))
       :foreground "green" :weight bold)
      (((class color) (background light))
       :foreground "green" :weight bold)
      (((class color) (background dark))
       :foreground "green" :weight bold)
      (t :foreground "green" :weight bold))
  "Face used to highlight hypotheses known true."
  :group 'nclose-font-highlighting-faces)

(defface nclose-font-bold-false-hypo-face
    '((((class grayscale) (background light))
       :foreground "red" :weight bold)
      (((class grayscale) (background dark))
       :foreground "red" :weight bold)
      (((class color) (background light))
       :foreground "red" :weight bold)
      (((class color) (background dark))
       :foreground "red" :weight bold)
      (t :foreground "red" :weight bold))
  "Face used to highlight hypotheses known false."
  :group 'nclose-font-highlighting-faces)

(defvar nclose-font-bold-true-hypo-face 'nclose-font-bold-true-hypo-face)
(defvar nclose-font-bold-false-hypo-face 'nclose-font-bold-false-hypo-face)
(defvar nclose-font-bold-current-hypo-face 'nclose-font-bold-current-hypo-face)

;; Fontification of hypotheses
(defun hypo-facespec (hypo-name)
  "Select font face according to hypothesis status/value"
  (let ((hypo (find-hypo hypo-name)))
    (if hypo
	(cond
	 ((and (boundp hypo) (symbol-value hypo))
	  nclose-font-bold-true-hypo-face)
	 ((and (boundp hypo) (null (symbol-value hypo)))
	  nclose-font-bold-false-hypo-face)
	 ((and (not (boundp hypo)) (get hypo ':current))
	  nclose-font-bold-current-hypo-face)
	 (t 'default)
	 )
      'default
    ))
)

(defun local-fontify-LHS ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (forward-list)
    (backward-list)
    (down-list)
    (if (looking-at "add-to-kb")
	(let* ((ignore (forward-list))
	       (end (point))
	       (ignore (backward-list))
	       (start (point))
	       )
	  (y-or-n-p
	   (format "%s " (add-text-properties start end
			       '(face font-lock-keyword-face))))
	
	  )
      (up-list)
      )
    )
)

;; Major mode
;; (regexp-opt '("add-to-kb" "add-to-kb-sets"))
;; (regexp-opt '("owl-class!" "owl-individual!" "owl-property!" "owl-slot!"))
;; (insert (regexp-opt '(":id" ":individuals" ":classes")))
;; (insert (regexp-opt '("nclose-defclass" "nclose-defindividuals")))



(defvar nclose-font-lock-keywords
  '(("\\(add-to-kb\\(?:-sets\\)?\\)" . font-lock-keyword-face)
    ("(@hypo[ ]+\\([a-zA-Z0-9\-]+\\))" 0 (hypo-facespec (match-string 1)))
    ;; OWL-LITE
    ("\\(:OWL-LITE\\)" 1 font-lock-constant-face t)
    ("\\(onto![ ]+\\)\'\\([a-zA-Z0-9\-]+\\)" 
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)) 
    ("owl-\\(?:\\(?:class\\|individual\\|property\\|slot\\)!\\)"
     . font-lock-keyword-face) 
    ;; EIEIO
    ("\\(:EIEIO\\)" 1 font-lock-constant-face t)
    ("\\(:id\\)" 1 font-lock-constant-face t)
    ("\\(:individuals\\)" 1 font-lock-constant-face t)
    ("\\(:classes\\)" 1 font-lock-constant-face t)
    ("nclose-defclass" . font-lock-keyword-face)
    ("nclose-defindividuals" . font-lock-keyword-face)
    )
  
  "Default expressions to highlight in nclose knowledge bases."
)

(define-derived-mode nclose-mode lisp-mode "NClose" 
  "A major mode for NClosEmacs."
  (setq font-lock-defaults '(nclose-font-lock-keywords))
  ;; Update keymap
  (define-key nclose-mode-map "\C-cs" 'nclose-suggest)
  (define-key nclose-mode-map "\C-cv" 'nclose-volunteer)
  (define-key nclose-mode-map "\C-ck" 'nclose-knowcess)
  (define-key nclose-mode-map "\C-cr" 'nclose-reset-session)
 )



