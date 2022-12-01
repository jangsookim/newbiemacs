(defun nbm-key-tree-level (tree)
  (nth 0 tree))

(defun nbm-key-tree-key (tree)
  (nth 1 tree))

(defun nbm-key-tree-description (tree)
  (let (str)
    (setq str (nth 2 tree))
    (if (equal str "") (setq str (nbm-key-tree-function tree)))
    (if (equal str "") (setq str "prefix"))
    str))

(defun nbm-key-tree-function (tree)
  (nth 3 tree))

(defun nbm-key-tree-subtrees (tree)
  (nth 4 tree))

(defun nbm-parse-property (line property)
  "LINE is a string of the following form.

key: z, description: calculator, function: quick-calc

In the above example, if  PROPERTY is \"mode\",
then it returns \"global\".
If there is no property, it returns the empty string \"\"."
  (let (beg end)
    (if (string-match (format "\\(%s:[ ]*\\)" property) line)
        (progn
          (setq beg (match-end 1))
          (string-match "\\(,\\|$\\)" line beg)
          (setq end (match-beginning 1))
          (substring line beg end))
      "")))

(defun nbm-key-tree-nodes-from-org-file (org-file)
  "Return a list of key-tree from ORG-FILE, in which each line looks like this.
*** key: z, description: calculator, function: quick-calc
A key-tree structure is (level key description function)."
  (save-excursion
    (let (nodes line new-node level beg end)
      (find-file org-file)
      (setq nodes '())
      (beginning-of-buffer)
      (while (re-search-forward "\\(^[*]+\\)" nil t)
        (setq level (- (match-end 1) (match-beginning 1)))
        (beginning-of-line) (setq beg (point))
        (end-of-line) (setq end (point))
        (setq line (buffer-substring-no-properties beg end))
        (setq new-node (list level
                             (nbm-parse-property line "key")
                             (nbm-parse-property line "description")
                             (nbm-parse-property line "function")))
        (setq nodes (nbm-append new-node nodes)))
      (kill-buffer "nbm_key_tree.org")
      nodes)))

;; (setq *nbm-key-nodes* (nbm-key-tree-nodes-from-org-file))

;; *nbm-key-nodes* is a list of nodes.
;; Each node is of the following form.
;; (level key description function) 
;; LEVEL is a nonnegative integer.
;; If LEVEL=0 or 1 then, KEY and FUNCTION are both the empty string "".
;; If LEVEL=0, then DESCRIPTION is either "system" or "user".
;; If LEVEL=1, then DESCRIPTION is a mode name such as "global" or "latex-mode".

;; A key-seq is a list of the following form.
;; ()

;; (setq node (nth 80 *nbm-key-nodes*))
;; (nbm-get-ancestors node *nbm-key-nodes*)
;; (nbm-get-key-sequence node *nbm-key-nodes*)
;; (nbm-all-key-sequences *nbm-key-nodes*)

(defun nbm-get-ancestors (node tree)
  "Return the list of ancestors of NODE in TREE."
  (let (level index ancestors)
    (setq index (-elem-index node tree))
    (setq level (car node))
    (while (>= index 0)
      (setq temp (nth index tree))
      (if (< (car temp) level)
	  (setq level (- level 1)
		ancestors (cons temp ancestors)))
      (setq index (- index 1)))
    ancestors))

(defun nbm-get-key-seq (node tree)
  "Return the list of keys to reach NODE in TREE."
  (let (keys ancestor)
    (setq ancestors (nbm-get-ancestors node tree))
    (setq ancestors (reverse ancestors))
    (setq keys (list (nth 1 node)))
    (dolist (ancestor ancestors keys)
      (setq keys (cons (if (nth 1 ancestor)
			   (nth 2 ancestor)) ; if no key, get decription
		       keys)))))

(defun nbm-all-key-seqs (tree)
  "Return the list of all key sequences from TREE."
  (let (node k key-seqs)
    (dolist (node tree key-seqs)
      (setq k (nbm-get-key-sequence node tree))
      (setq key-seqs (nbm-append (cons )
				 key-seqs))
      )))




(defun nbm-key-tree-key-tree-from-nodes (nodes)
  "Create a key-tree from NODES."
  (save-excursion
    (let (tree level node first-node subnodes second-part)
      (when nodes
        (setq first-node (car nodes))
        (setq level (nbm-key-tree-level first-node))
        (setq nodes (cdr nodes)))
      (setq subnodes '())
      (while (and nodes (not second-part))
        (setq node (car nodes))
        (if (> (nbm-key-tree-level node) level)
            (setq subnodes (nbm-append node subnodes)
                  nodes (cdr nodes))
          (setq second-part t)))
      (if subnodes
          (setq first-node (nbm-append (nbm-key-tree-key-tree-from-nodes subnodes)
                                       first-node))
        (setq first-node (nbm-append nil first-node)))
      (if nodes
          (setq tree (cons first-node (nbm-key-tree-key-tree-from-nodes nodes)))
        (setq tree (list first-node)))
      tree)))

(defun nbm-key-tree-load ()
  (let (nodes tree)
    (setq nodes (nbm-key-tree-nodes-from-org-file
		 (nbm-root-f "nbm_key_tree.org")))
    (setq tree (nbm-key-tree-key-tree-from-nodes nodes))
    (setq *nbm-key-tree* tree)))

(defun nbm-append (last list)
  "Append LAST at the end of LIST."
  (reverse (cons last (reverse list))))

(defun nbm-key-tree-prompt (tree)
  "Run key-tree from TREE."
  (let (prompt subtrees T key func match)
    (setq func (nbm-key-tree-function tree))
    (if (not (string= func "")) (funcall (intern func)))
    (when (nbm-key-tree-subtrees tree)
      (setq subtrees (nbm-key-tree-subtrees tree))
      (setq key (char-to-string (read-char (nbm-key-tree-prompt-string subtrees))))
      (setq subtrees (nbm-key-tree-subtrees tree))
      (while subtrees
	(setq T (pop subtrees))
	(when (nbm-key-tree-compare-key key T)
	  (setq match t)
	  (nbm-key-tree-prompt T)))
      (unless match
	(message "You typed an undefined key sequence.")))))

(defun nbm-key-tree-compare-key (key T)
  "Return t if KEY is the same as the key in T."
  (cond ((string= key " ")
	       (if (string= (nbm-key-tree-key T) "SPC") t nil))
        ((string= key "\^M")
	       (if (string= (nbm-key-tree-key T) "RET") t nil))
        ((string= key "\t")
	       (if (string= (nbm-key-tree-key T) "TAB") t nil))
        (t (string= key (nbm-key-tree-key T)))))

(defun nbm-key-tree-global ()
  (interactive)
  "Run key-tree from the root key tree for global mode."
  (let (tree)
    (dolist (tree *nbm-key-tree*)
      (when (string= (nbm-key-tree-description tree) "global")
        (nbm-key-tree-prompt tree)))))

(defun nbm-key-tree-mode (&optional mode)
  (interactive)
  "Run key-tree from the root key tree for the current mode."
  (let (tree)
    (dolist (tree *nbm-key-tree*)
      (unless mode
	(setq mode (buffer-local-value 'major-mode (current-buffer))))
      (when (string= (nbm-key-tree-description tree) ;; description of level 1 tree has mode name
                     mode)
        (nbm-key-tree-prompt tree)))))

(defun nbm-key-tree-add-keybinding ()
  "Insert the string \"** key: KEY, description: DESCRIPTION, function: FUNCTION\" in the next line."
  (let (key description function line)
    (setq key (read-string "Enter key: "))
    (setq description (read-string "Enter description: "))
(setq function (read-command "Enter function: " 'nbm-nil-function))
    (end-of-line) (newline)
    (setq line (format "** key: %s" key))
    (unless (string= description "")
      (setq line (format "%s, description: %s" line description)))
    (unless (string= function "nbm-nil-function")
      (setq line (format "%s, function: %s" line function)))
    (insert line)))

(defun nbm-nil-function ()
  "This is a function doing nothing but to be passed to nbm-key-tree-add-keybinding."
  (interactive))

(defun nbm-key-tree-prompt-string (subtrees)
  "Return a string for the key-tree-prompt from SUBTREES."
  (setq col-width 30)
  (let (prompt temp T key func terminals internals col pos desc)
    (setq col (/ (frame-width) col-width)
	  pos 0
	  prompt ""
	  temp subtrees)
    (while temp
      (setq T (pop temp))
      (if (nbm-key-tree-subtrees T)
	  (setq internals (nbm-append T internals))
	(setq terminals (nbm-append T terminals))))
    (defun nbm-temp-prompt (TT color)
      (while TT
	(when (equal pos col)
	  (setq prompt (concat prompt "\n"))
	  (setq pos 0))
	(setq T (pop TT))
	(setq desc (nbm-key-tree-description T))
	(if (> (length desc) (- col-width 6))
	    (setq desc (concat (substring desc 0
					  (- col-width 9))
			       "...")))
	(if (equal color 1)
	    (setq desc (nbm-string-terminal-node desc))
	  (setq desc (nbm-string-internal-node desc))
	  )
	(setq key (nbm-string-key (nbm-key-tree-key T)))
	(setq prompt (format (concat "%s%3s: %-"
				     (number-to-string (- col-width 5))
				     "s")
			     prompt key desc))
	(setq pos (+ 1 pos))
	))
    (nbm-temp-prompt terminals 1)
    (nbm-temp-prompt internals 2)
    prompt))
