;; The following two are the important concepts in this file.
;;
;; A key-node is of the following form.
;; (depth key description function)
;; DEPTH is a nonnegative integer.
;; If DEPTH=0 or 1 then, KEY and FUNCTION are both the empty string "".
;; If DEPTH=0, then DESCRIPTION is either "system" or "user".
;; If DEPTH=1, then DESCRIPTION is a mode name such as "global" or "latex-mode".
;;
;; A key-seq is a list of the following form.
;; (keys description function)
;; KEYS is a list of the form e.g. ("global" "a" "e")

(defvar *nbm-key-seqs*)
(defvar *nbm-key-seqs-repeated*)

(defun nbm-parse-property (line property)
  "LINE is a string of the following form.

** key: z, description: calculator, function: quick-calc, level: 2

In the above example, if  PROPERTY is \"mode\",
then it returns \"global\".
If there is no property, it returns the empty string \"\"."
  (let (beg end)
    (if (string-match (format "\\(%s:[ ]*\\)" property) line)
        (progn
          (setq beg (match-end 1))
          (string-match "\\(,+\\|$\\)" line beg)
          (setq end (match-end 1))
	  (if (equal (substring line (- end 1) end) ",")
	      (setq end (- end 1)))
          (substring line beg end))
      "")))

(defun nbm-key-tree-nodes-from-org-file (org-file)
  "Return a list of key nodes from ORG-FILE, in which each line looks like this.
*** key: z, description: calculator, function: quick-calc, level 2
A key-tree structure is (depth key description function)."
  (save-excursion
    (let (nodes line new-node depth beg end level)
      (find-file org-file)
      (setq nodes '())
      (beginning-of-buffer)
      (while (re-search-forward "\\(^[*]+\\)" nil t)
	(setq depth (- (match-end 1) (match-beginning 1)))
	(beginning-of-line) (setq beg (point))
	(end-of-line) (setq end (point))
	(setq line (buffer-substring-no-properties beg end))
	(if (equal (nbm-parse-property line "level") "")
	    (setq level 1)
	  (setq level (string-to-number (nbm-parse-property line "level"))))
	(when (<= level (nbm-get-user-level))
	  (setq new-node (list depth
			       (nbm-parse-property line "key")
			       (nbm-parse-property line "description")
			       (nbm-parse-property line "function")))
	  (setq nodes (nbm-append new-node nodes))))
      (kill-buffer)
      nodes)))

(defun nbm-set-user-level ()
  "Set user level."
  (interactive)
  (let (level)
    (setq level (read-char (format "Set your level (current level is %s):
1) Beginner (Only basic commands are shown when hitting the space key.)
2) Intermidiate (More commands are shown.)
3) Advanced (All commands are shown.)" (nbm-get-user-level))))
    (when (member level '(?1 ?2 ?3))
      (nbm-set-user-variable "level" (char-to-string level)))))

(defun nbm-get-user-level ()
  "Return user level. By default it is 0."
  (unless (nbm-get-user-variable "level" nil)
    (nbm-set-user-variable "level" "1"))
  (string-to-number (nbm-get-user-variable "level" nil)))

;; The following should be loaded at the beginning of emacs session.
;; The loading process is as follows.
;; 1. Create key-nodes from user-key-tree.org and nbm-sys-key-tree.org.
;; 2. Convert the key-nodes to key-seqs.
;; 3. Sort key sequences

(defun nbm-key-tree-load ()
  "Initiate *nbm-key-seqs*."
  (let (nbm-nodes user-nodes all-nodes tree)
    (setq user-nodes (nbm-key-tree-nodes-from-org-file
		      (nbm-f "nbm-user-settings/user-key-tree.org")))
    (setq nbm-nodes (nbm-key-tree-nodes-from-org-file
		     (nbm-root-f "nbm-sys-key-tree.org")))
    (setq key-seqs (append (nbm-key-seqs-from-nodes user-nodes)
			   (nbm-key-seqs-from-nodes nbm-nodes)))
    (setq *nbm-key-seqs* (nbm-sort-and-remove-repeated-key-seqs key-seqs))))

(defun nbm-key-seq< (A B)
  "Return t if A occurs earlier than B.
This is case-insensitive."
  (let (a b keyA keyB done result (sort-fold-case t))
    (setq keyA (car A) keyB (car B))
    (while (and (not done) keyA keyB)
      (setq a (pop keyA) b (pop keyB))
      (cond ((> (length a) (length b)) (setq result t done t))
	    ((< (length a) (length b)) (setq result nil done t))
	    ((string< (upcase a) (upcase b)) (setq result t done t))
	    ((string> (upcase a) (upcase b)) (setq result nil done t))
	    ((string< a b) (setq result t done t))
	    ((string> a b) (setq result nil done t))))
    (unless done
      (if keyA (setq result nil))
      (if keyB (setq result t)))
    result))

(defun nbm-key-seqs-from-nodes (nodes)
  "Create key-seqs from NODES."
  (let (key key-seq key-seqs depth node)
    (dolist (node nodes key-seqs)
      (setq depth (car node))
      (when (equal depth 1)   	        ;; if depth=1 then set key-seq
	(setq key (list (nth 2 node)))) ;; to be description of node
      (when (> depth 1)
	(if (<= depth (length key))
	    (setq key (butlast key
			       (- (length key) (- (car node) 1)))))
	(setq key (nbm-append (nth 1 node) key)))
      (setq key-seq (cons key (nthcdr 2 node)))
      (setq key-seqs (nbm-append key-seq key-seqs)))))

(defun nbm-sort-and-remove-repeated-key-seqs (key-seqs)
  "Sort and remove repeated key sequences.
Repeated key-seqs are saved in *nbm-key-seqs-repeated*"
  (let (sorted no-repeat last-key-seq new-key-seq)
    (setq *nbm-key-seqs-repeated* nil)
    (setq sorted (sort key-seqs 'nbm-key-seq<))
    (setq no-repeat nil)
    (dolist (new-key-seq sorted)
      (setq last-key-seq (car (last no-repeat)))
      (if (and (> (length (car new-key-seq)) 1)
	       (equal (car new-key-seq) (car last-key-seq)))
	  (setq *nbm-key-seqs-repeated* (nbm-append (list last-key-seq new-key-seq)
					      *nbm-key-seqs-repeated*))
	(setq no-repeat (nbm-append new-key-seq no-repeat))))
    no-repeat))

(defun nbm-key-tree-show-repeated-keys ()
  "Message if there are repeated keys."
  (interactive)
  (let (repeated prompt)
    (if *nbm-key-seqs-repeated*
	(progn
	  (setq prompt "The following key sequences are repeated.")
	  (dolist (repeated *nbm-key-seqs-repeated*)
	    (setq prompt (format "%s\n%s and %s"
				 prompt (nth 0 repeated) (nth 1 repeated)))))
      (setq prompt "There are no repeated key sequences."))
    (message (format "%s" prompt))))

(defun nbm-key-tree-appear-in-which-key ()
  "Make the key-tree appear in which-key."
  (let (key-seq mode keys desc func buf key-str key)
    (find-file (nbm-f "nbm-user-settings/nbm-which-key.el"))
    (erase-buffer)
    (setq buf (current-buffer))
    (dolist (key-seq *nbm-key-seqs*)
      (when (> (length (car key-seq)) 1)
	(setq mode (car (car key-seq))
	      keys (cdr (car key-seq))
	      desc (nth 1 key-seq)
	      func (nth 2 key-seq))
	(if (equal desc "") (setq desc func))
	(setq key-str "")
	(dolist (key keys)
	  (if (equal key "\"") (setq key "\\\""))
	  (setq key-str (concat key-str key)))
	(insert (format "(evil-define-key '(normal visual motion insert emacs) %s (kbd \"%s%s\") '(\"%s\" . %s))\n"
			(if (equal mode "global") "'global" (concat mode "-map"))
			(if (equal mode "global") "<leader>" "<localleader>")
			key-str desc
			(if (equal func "") "(keymap)" func)))))
    (save-buffer) (kill-buffer buf)))

;; Functions for adding a keybiding to key tree

(defun nbm-key-tree-add-keybinding ()
  (interactive)
  "Insert the string \"** key: KEY, description: DESCRIPTION, function: FUNCTION, level: LEVEL\" in the next line."
  (let (key description function line level)
    (setq key (read-string "Enter key: "))
    (setq description (read-string "Enter description: "))
    (setq function (read-command "Enter function: " 'nbm-nil-function))
    (setq level (read-string "Enter level: " "1"))
    (end-of-line) (newline)
    (setq line (format "** key: %s" key))
    (unless (string= description "")
      (setq line (format "%s, description: %s" line description)))
    (unless (string= function "nbm-nil-function")
      (setq line (format "%s, function: %s" line function)))
    (setq line (format "%s, level: %s" line level))
    (insert line)))

(defun nbm-nil-function ()
  "This is a function doing nothing but to be passed to nbm-key-tree-add-keybinding."
  (interactive))
