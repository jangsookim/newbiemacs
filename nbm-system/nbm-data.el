;; nbm-VAR.txt must be a file whose content is as follows.
;; Each line is of the form "KEY=(one character), VALUE=(some string)"
;; For example, KEY=a, VALUE=test.pdf

(defun nbm-data-get-all (var)
  "Return the list of all items (key . value) in the file nbm-VAR.txt in the folder
newbiemacs/nbm-user-settings/nbm-variables."
  (let (str line item data)
    (setq str (nbm-get-user-variable var t))
    (when str
      (setq str (split-string str "\n"))
      (setq str (remove "" str))
      (while str
	(setq line (pop str))
	(setq item (cons (substring line 4 5) (substring line 13 nil)))
	(setq data (cons item data)))
      data)))

(defun nbm-data-get (var key)
  "Return the item (KEY . value) in the file nbm-VAR.txt in the folder
newbiemacs/nbm-user-settings/nbm-variables.
If there is no item with KEY, return nil."
  (let (str line item)
    (setq str (nbm-get-user-variable var t))
    (when str
      (setq str (split-string str "\n"))
      (setq str (remove "" str))
      (while (and str (not item))
	(setq line (pop str))
	(when (equal (substring line 4 5) key)
	  (setq item (cons key (substring line 13 nil)))))
      item)))

(defun nbm-data-add (var key value)
  "Add an item (KEY . VALUE) in the file nbm-VAR.txt in the folder
newbiemacs/nbm-user-settings/nbm-variables."
  (let (file exist)
    (setq exist (nbm-data-get var key))
    (when exist
      (when (equal ?y (read-char (format "The key already exists!
Do you want to delete this item? (type y for yes)\n%s" exist)))
	(nbm-data-delete var key)
	(setq exist nil)))
    (unless exist
      (setq file (concat *nbm-home* (format "nbm-user-settings/nbm-variables/nbm-%s.txt" var)))
      (unless (file-exists-p file) (nbm-set-user-variable var ""))
      (find-file file)
      (end-of-buffer)
      (insert (format "KEY=%s, VALUE=%s\n" key value))
      (save-buffer) (kill-buffer))))

(defun nbm-data-delete (var key)
  "Delete an item (KEY . value) in the file nbm-VAR.txt in the folder
newbiemacs/nbm-user-settings/nbm-variables."
  (find-file (concat *nbm-home* (format "nbm-user-settings/nbm-variables/nbm-%s.txt" var)))
  (beginning-of-buffer)
  (let ((case-fold-search nil))
    (when (re-search-forward (format "^KEY=%s" (regexp-quote key)) nil t)
      (beginning-of-line)
      (kill-line) (kill-line) (save-buffer)))
  (kill-buffer))

(defun nbm-visit ()
  "Run nbm-visit."
  (interactive)
  (let (choice prompt file-list data item key)
    (setq prompt "Select action:\n+) create new mark\n-) delete mark\n")
    (setq file-list "")
    (dolist (item (nbm-data-get-all "visit"))
      (setq file-list (format "%s\n%s) %s" file-list (car item) (file-name-nondirectory (cdr item)))))
    (setq prompt (concat prompt file-list))
    (setq choice (char-to-string (read-char prompt)))
    (cond ((equal choice "+")
	   (setq key (char-to-string (read-char "Enter a key to mark the current file. (+ and - must be avoided.): ")))
	   (nbm-data-add "visit" key (nbm-get-file-name)))
	  ((equal choice "-")
	   (setq key (char-to-string (read-char (format "Enter the key you want to delete:\n%s" file-list))))
	   (nbm-data-delete "visit" key))
	  (t
	   (setq item (nbm-data-get "visit" choice))
	   (if item
	       (find-file (cdr item))
	     (message "You typed a wrong key."))))))
