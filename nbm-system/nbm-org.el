(defun nbm-org-roam-search ()
  "Grep my org roam folder."
  (interactive)
  (setq keyword (read-string "Enter the search keyword (regexp) for org-roam: " nil nil nil nil))
  (rgrep keyword "*.org" (nbm-f "org/") nil))

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
      (beginning-of-line) (re-search-forward "\\(^[*]+ \\|^[-+] \\|^[0-9]+[.)] \\)" end t)
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