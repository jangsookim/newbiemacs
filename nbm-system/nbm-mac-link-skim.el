(defun nbm-skim-get-filename ()
  "Get the filename of the pdf currently opened in Skim.
Return nil if no file is opened in Skim."
  (let (str)
    (unless (string-match "Skim got an error" (org-mac-link-skim-get-page))
      (setq str (substring (org-mac-link-skim-get-page) 9 -2))
      (setq str (string-replace "\\" "" str))
      (setq str (car (split-string str "::"))))))

(defun nbm-skim-data-init ()
  "Create a file if newbiemacs/nbm-user-settings/nbm-variables/nbm-skim-data.el does not exist."
  (unless (file-exists-p (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
    (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
    (insert "(defvar *nbm-skim-data* nil)
(setq *nbm-skim-data*
      '(
	))")))

(defun nbm-skim-data-add (key value)
  "Add a cons cell (KEY . VALUE) to *nbm-skim-data*.
This variable is then saved in newbiemacs/nbm-user-settings/nbm-variables/nbm-skim-data.el."
  (nbm-skim-data-init)
  (let (prompt)
    (when (nbm-skim-data-get key)
      (setq prompt (format "(%s . %s) is already in *nbm-skim-data*!\nProceed anyway? (Type y for yes.): "
			   key (nbm-skim-data-get key)))
      (when (equal ?y (read-char prompt))
	(nbm-skim-data-delete key)
	(nbm-skim-data-add key value)))
    (unless (nbm-skim-data-get key)
      (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
      (beginning-of-buffer)
      (search-forward "'(")
      (insert (format "\n\t(\"%s\" . \"%s\")" key value))
      (save-buffer) (eval-buffer) (kill-buffer))))

(defun nbm-skim-data-get (key)
  "Get the value such that (KEY . VALUE) is in *nbm-skim-data*."
  (unless (boundp '*nbm-skim-data*)
    (nbm-skim-data-init)
    (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
    (save-buffer) (eval-buffer) (kill-buffer))
  (cdr (assoc key *nbm-skim-data*)))

(defun nbm-skim-data-delete (key)
  "Delete the cons cell with key KEY in *nbm-skim-data*.
This variable is then saved in newbiemacs/nbm-user-settings/nbm-variables/nbm-skim-data.el."
  (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
  (beginning-of-buffer)
  (re-search-forward (format "^\t(\"%s\" . " key))
  (beginning-of-line) (kill-line) (kill-line)
  (save-buffer) (eval-buffer) (kill-buffer))

(defun nbm-skim-set-page-offset (offset)
  "Set a page offset for the current pdf file."
  (nbm-skim-data-add (file-name-nondirectory (nbm-skim-get-filename)) offset))

(defun nbm-skim-goto-page ()
  "Go to a specific page of the pdf current opened in skim."
  (interactive)
  (let (offset page str)
    (setq str (nbm-skim-get-filename))
    (unless str
      (message "You must open a pdf in Skim."))
    (when str
      (setq page (read-string "Page (write + or - for offset): "))
      (if (member (substring page 0 1) '("+" "-"))
	  (nbm-skim-set-page-offset (string-to-number page))
	(progn
	  (setq offset (nbm-skim-data-get (file-name-nondirectory str)))
	  (unless offset (setq offset "0"))
	  (setq str (format "%s::%s" str (+ (string-to-number page) (string-to-number offset))))
	  (org-mac-link-skim-open str nil))))))
