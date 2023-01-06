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
  (let (old new choice pos)
    (setq old (file-name-nondirectory (buffer-file-name)))
    (if (string-match "[0-9]\\{14\\}-" "20221109090747-test_2.org")
	(progn
	  (setq new (substring old 15))
	  (setq choice (read-char (format "Rename the file?: (Type y for yes.)\nOld name: %s\nNew name: %s" old new)))
	  (when (equal choice ?y)
	    (setq pos (point))
	    (rename-file old new) (find-file new) (kill-buffer old)
	    (goto-char pos)
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

(defun nbm-org-jump-to-url ()
  "Jump to a url in the current org file."
  (interactive)
  (let (url url-list)
    (setq url-list (org-element-map (org-element-parse-buffer) 'link
		      (lambda (link)
			(when (string-match-p "http" (org-element-property :type link))
			  (concat (org-element-property :type link) ":"
				  (org-element-property :path link))))))
    (setq url (completing-read "Choose a url to open: " url-list nil t ""))
    (org-link-open-from-string url)))

(defun nbm-org-html-export ()
  "Export to html and open it."
  (interactive)
  (org-html-export-to-html)
  (shell-command (format "open %s.html"
		 (file-name-sans-extension (buffer-file-name)))))

(defun nbm-org-html-theme ()
  "Insert org-html-theme in the header."
  (interactive)
  (let (themes choice str)
    (unless (file-exists-p (nbm-f "nbm-user-settings/org-themes"))
      (find-file (nbm-f "nbm-user-settings"))
      (shell-command "git clone https://gitlab.com/OlMon/org-themes.git")
      (kill-buffer))
    (setq themes '(
		   "bigblow_inline"
		   "comfy_inline"
		   ;; "darksun"
		   "gray"
		   "imagine_light"
		   "latexcss"
		   "plain"
		   "readtheorg_inline"
		   "rethink_inline"
		   "retro_dark"
		   "simple_gray"
		   "simple_inline"
		   "simple_white"
		   "simple_whiteblue"
		   "solarized_dark"
		   "solarized_light"
		   "stylish_white"
		   "white_clean"
		   ))
    (setq choice (completing-read "Select the theme: " themes nil nil nil nil "readtheorg_inline"))
    (setq str (format "#+SETUPFILE: %snbm-user-settings/org-themes/src/%s/%s.theme"
		      *nbm-home* choice choice))
    (save-excursion
      (beginning-of-buffer)
      (when (search-forward "#+SETUPFILE:" nil t)
	  (beginning-of-line) (kill-line) (kill-line))
      (beginning-of-buffer)
      (search-forward "#+title:") (next-line) (beginning-of-line)
      (insert (format "%s\n" str)))
    (message (format "Inserted %s" str))))

(defun nbm-org-sage-tangle ()
  "Tangle the python blocks to a sage file.
If there is a sage shell, then load the sage file in the sage shell.
Otherwise, copy a string in the clipboard to load it."
  (interactive)
  (let (python sage str buf)
    (setq python (concat (file-name-sans-extension (buffer-file-name)) ".python"))
    (setq sage (concat (file-name-sans-extension (buffer-file-name)) ".sage"))
    (org-babel-tangle)
    (rename-file python sage t)
    (setq str (format "load(\"%s\")" sage))
    (if (gnus-buffer-exists-p "*Sage*")
	(save-excursion
	  (setq buf (current-buffer))
	  (switch-to-buffer "*Sage*")
	  (insert str) (sage-shell:send-input)
	  (switch-to-buffer buf)
	  (other-window 1)
	  (end-of-buffer))
      (progn
	(kill-new str)
	(message (format "Copied to clipboard: %s" str))))))

(defun nbm-org-latex-in-line-math ()
  "Insert \\( \\) and open an org edit special buffer."
  (interactive)
  (insert "\\(  \\)")
  (org-edit-special) (backward-char 3) (evil-append 0))

(defun nbm-org-latex-display-math ()
  "Insert \\[ \\] and open an org edit special buffer."
  (interactive)
  (insert "\\[  \\]")
  (org-edit-special) (backward-char 3) (evil-append 0))

(defun nbm-org-move-to-archived ()
  "Move the current file to the archived directory: newbiemacs/archived/org/
and store the org link."
  (interactive)
  (let (archive html new-file)
    (unless (file-exists-p (nbm-f "archived"))
      (make-directory (nbm-f "archived"))
      (make-directory (nbm-f "archived/org")))
    (setq new-file (nbm-f (concat "archived/org/"
			   (file-name-nondirectory (buffer-file-name)))))
    (setq html (concat (file-name-sans-extension (buffer-file-name)) ".html"))
    (setq archive (concat (buffer-file-name) "_archive"))
    (if (file-exists-p html)
	(rename-file html (concat (file-name-sans-extension new-file) ".html")))
    (if (file-exists-p archive)
	(rename-file archive (concat new-file "_archive")))
    (rename-file (buffer-file-name) new-file)
    (kill-buffer) (find-file new-file)
    (push (list (concat "file:" new-file)
		(file-name-nondirectory (file-name-sans-extension new-file)))
	  org-stored-links)
    (message (concat "Moved and org link stored: " new-file))))

(defun nbm-org-yank-code ()
  "Yank the current code line."
  (interactive)
  (save-excursion
    (let (end)
      (end-of-line) (setq end (point))
      (beginning-of-line)
      (when (search-forward ": " end t)
	(kill-new (buffer-substring (point) end))
	(message (concat "Copied to clipboard: " (current-kill 0)))))))
