(defun nbm-goodnotes-data-init ()
  "Create a file if newbiemacs/nbm-user-settings/nbm-variables/nbm-goodnotes-data.el does not exist."
  (unless (file-exists-p (nbm-f "nbm-user-settings/nbm-variables/nbm-goodnotes-data.el"))
    (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-goodnotes-data.el"))
    (insert "(defvar *nbm-goodnotes-data* nil)
(setq *nbm-goodnotes-data*
      '(
	(\"+ (add a note)\" . nil)
	(\"- (delete a note)\" . nil)
	))")
    (save-buffer) (eval-buffer) (kill-buffer))
  (load-file (nbm-f "nbm-user-settings/nbm-variables/nbm-goodnotes-data.el")))

(defun nbm-goodnotes-data-add (note-name url)
  "Add a cons cell (note-name . url) to *nbm-goodnotes-data*.
This variable is then saved in newbiemacs/nbm-user-settings/nbm-variables/nbm-goodnotes-data.el."
  (let (prompt)
    (when (nbm-goodnotes-data-get note-name)
      (setq prompt (format "(%s . %s) is already in *nbm-goodnotes-data*!\nProceed anyway? (Type y for yes.): "
			   note-name (nbm-goodnotes-data-get note-name)))
      (when (equal ?y (read-char prompt))
	(nbm-goodnotes-data-delete note-name)
	(nbm-goodnotes-data-add note-name url)))
    (unless (nbm-goodnotes-data-get note-name)
      (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-goodnotes-data.el"))
      (beginning-of-buffer)
      (search-forward "'(")
      (setq url (car (split-string url "#page")))
      (setq url (string-replace "share.goodnotes.com" "web.goodnotes.com" url))
      (insert (format "\n\t(\"%s\" . \"%s\")" note-name url))
      (save-buffer) (eval-buffer) (kill-buffer))))

(defun nbm-goodnotes-data-get (note-name)
  "Get the url such that (NOTE-NAME . URL) is in *nbm-goodnotes-data*."
  (unless (boundp '*nbm-goodnotes-data*)
    (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-goodnotes-data.el"))
    (save-buffer) (eval-buffer) (kill-buffer))
  (cdr (assoc note-name *nbm-goodnotes-data*)))

(defun nbm-goodnotes-data-delete (note-name)
  "Delete the cons cell with note-name NOTE-NAME in *nbm-goodnotes-data*.
This variable is then saved in newbiemacs/nbm-user-settings/nbm-variables/nbm-goodnotes-data.el."
  (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-goodnotes-data.el"))
  (beginning-of-buffer)
  (re-search-forward (format "^\t(\"%s\" . " note-name))
  (beginning-of-line) (kill-line) (kill-line)
  (save-buffer) (eval-buffer) (kill-buffer))

(defun nbm-goodnotes-goto-note ()
  "Go to a note."
  (interactive)
  (nbm-goodnotes-data-init)
  (let (note-name page choice url)
    (setq note-name (completing-read "Choose a note (+ for adding a note, - for deleting a note): " *nbm-goodnotes-data*))
    (cond 
     ((equal note-name "+ (add a note)")
      (setq note-name (read-string "Enter the name of the note: "))
      (setq url (read-string "Enter the url of the note: "))
      (nbm-goodnotes-data-add note-name url))
     ((equal note-name "- (delete a note)")
      (setq note-name (completing-read "Choose the note to delete: " *nbm-goodnotes-data*))
      (unless (or (equal note-name "+ (add a note)")
		  (equal note-name "- (delete a note)"))
	(nbm-goodnotes-data-delete note-name)))
     (t
      (setq page (read-number "Enter the page you want to visit: " 0))
      (nbm-goodnotes-goto-page note-name page)))))

(defun nbm-goodnotes-goto-page (note-name &optional page)
  "Go to a goodnote url."
  (let (url)
    (if (> page 0)
	(setq page (format "#page-%s" page))
      (setq page ""))
    (setq url (format "%s%s" (nbm-goodnotes-data-get note-name) page))
    (browse-url url)))
