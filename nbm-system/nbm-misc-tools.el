(defun nbm-yank-favorite-string ()
  "Copy a frequently used string to the kill-ring."
  (interactive)
  (let (choice string-list prompt key str beg end temp buf)
    (save-excursion
      (find-file (nbm-f "nbm-user-settings/references/favorites.txt"))
      (setq buf (current-buffer))
      (beginning-of-buffer)
      (setq prompt "Choose the string to yank: (Only the first line of each string is shown.)\n")
      (while (re-search-forward "^\\(.\\)) " nil t)
	(setq key (string-to-char (match-string 0)))
	(setq beg (point))
	(if (re-search-forward "^.) " nil t)
	    (setq end (- (point) 4))
	  (setq end (point-max)))
	(setq str (buffer-substring beg end))
	(setq string-list (cons (list key str) string-list))
	(setq prompt (format "%s\n%c) %s" prompt key
			     (car (split-string str "\n"))))
	(goto-char end))
      (kill-buffer buf)
      (setq choice (read-char prompt))
      (dolist (temp string-list)
	(when (equal choice (nth 0 temp))
	  (kill-new (nth 1 temp))
	  (message (format "Copied in clipboard: %s" (nth 1 temp))))))))

(defun nbm-load-sage ()
  "Copy a command to load the current file in sage notebook."
  (interactive)
  (kill-new (format "load(\"%s\")" (buffer-file-name)))
  (message (format "Copied to clipboard: %s" (current-kill 0))))

(defun nbm-get-user-variable (var all)
  "Return the content of the file nbm-VAR.txt in the folder
newbiemacs/nbm-user-settings/nbm-variables.
If ALL is t, then return the full content.
Otherwise, return the first line."
  (interactive)
  (let (file)
    (setq file (concat *nbm-home* (format "nbm-user-settings/nbm-variables/nbm-%s.txt" var)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(if all
	    (buffer-string)
	  (car (split-string (buffer-string) "\n")))))))
    
(defun nbm-set-user-variable (var content)
  "Set CONTENT to be the content of the file nbm-VAR.txt in the folder
newbiemacs/nbm-user-settings/nbm-variables."
  (let (file)
    (setq file (concat *nbm-home* (format "nbm-user-settings/nbm-variables/nbm-%s.txt" var)))
    (find-file file) (erase-buffer)
    (insert content) (save-buffer) (kill-buffer)))



