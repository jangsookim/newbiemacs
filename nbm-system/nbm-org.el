(defun nbm-org-toggle-strike-through ()
  "Toggle strike though the current item."
  (interactive)
  (let (end)
    (end-of-line) (setq end (point))
    (beginning-of-line) (re-search-forward "[*]+ \\|[-+] \\|[0-9]+[.)] " end t)
    (when (string= (char-to-string (char-after)) "[")
	(forward-sexp) (forward-char))
    (if (equal (char-after) ?+)
	(progn
	  (delete-char 1) (end-of-line) (delete-char -1))
      (progn
	(insert "+") (end-of-line) (insert "+")))))

(defun nbm-org-toggle-checkbox ()
  "Toggle checkbox in the current item.
If there is no checkbox, create one."
  (interactive)
  (save-excursion
    (let (end)
      (end-of-line) (setq end (point))
      (beginning-of-line) (re-search-forward "\\(^[*]+ \\|^ *[-+] \\|^ *[0-9]+[.)] \\)" end t)
      (when (match-string 1)
	(if (string= (char-to-string (char-after)) "[")
	    (org-toggle-checkbox)
	  (if (equal (substring (match-string 1) 0 1) "*")
	      (insert "[/] ")
	    (insert "[ ] ")))))))

(defun nbm-make-permanant-note ()
  "Delete the 15 digits in the current temporary org-roam file to make it a public note.
For example, 20221109090747-test.org will be changed to test.org."
  (interactive)
  (let (old new choice)
    (setq old (file-name-nondirectory (buffer-file-name)))
    (if (string-match "[0-9]\\{14\\}-" "20221109090747-test_2.org")
        (progn
          (setq new (substring old 15))
          (setq choice (read-char (format "Rename the file?: (Type y for yes.)\nOld name: %s\nNew name: %s" old new)))
          (when (equal choice ?y)
            (rename-file old new) (find-file new) (kill-buffer old)
            (message "File name changed.")))
      (message "Invalid filename. Check if the current file is a temporary org-roam, e.g. 20221109090747-test.org."))))

(defun nbm-org-consecutive-dates ()
  "Insert consecutive dates."
  (interactive)
  (let (type)
    (setq type (read-char "Which type of consecutive dates?\ns) schedule\na) active time stamp\ni) inactive time stamp"))
    (cond ((equal type ?s)
	   (call-interactively 'org-schedule)
	   (save-excursion
	     (org-backward-heading-same-level 0)
	     (search-forward "SCHEDULED:")
	     (end-of-line)
	     (insert "--")
	     (call-interactively 'org-time-stamp)))
	  ((equal type ?a)
	   (call-interactively 'org-time-stamp)
	   (insert "--")
	   (call-interactively 'org-time-stamp))
	  ((equal type ?i)
	   (call-interactively 'org-time-stamp-inactive)
	   (insert "--")
	   (call-interactively 'org-time-stamp-inactive)))))

(defun nbm-org-jump-to-heading ()
  "Jump to a heading in the current org file."
  (interactive)
  (let (org-refile-history org-refile-targets level)
    (setq level (read-char "Enter the max level of headings to search (default 1):"))
    (if (member level '(?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
	(setq org-refile-targets (cons `(,(buffer-file-name) . (:maxlevel . ,level)) org-refile-targets))
      (setq org-refile-targets '((nil . (:level . 1)))))
    (org-refile (universal-argument))))

(defun nbm-org-jump-to-archive ()
  "Jump to the archive file of the current org file."
  (interactive)
  (find-file (concat (buffer-file-name) "_archive")))

(defun nbm-org-jump-to-file ()
  "Jump to a file in the current org file."
  (interactive)
  (let (file-list file)
    (setq file-list (org-element-map (org-element-parse-buffer) 'link
		      (lambda (link)
			(when (string= (org-element-property :type link) "file")
			  (org-element-property :path link)))))
    (setq file (completing-read "Choose a file to open: " file-list nil t ""))
    (find-file file)))
