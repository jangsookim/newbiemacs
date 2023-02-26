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

(defun nbm-get-user-variable (var &optional all)
  "Return the content of the file nbm-VAR.txt in the folder
newbiemacs/nbm-user-settings/nbm-variables.
If ALL is t, then return the full content.
Otherwise, return the first line."
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

(defun nbm-tab-line-jump ()
  "Jump to a tab in the tab line list."
  (interactive)
  (let (key pos)
    (setq key ?h)
    (while (member key '(?h ?l ?k))
      (setq key (read-char "h) go to the previous buffer
l) go to the next buffer
k) kill this buffer
other key) stop"))
      (if (equal key ?k) (kill-buffer (current-buffer)))
      (if (and (equal key ?l)
	       (not (equal (current-buffer) (car (last (tab-line-tabs-window-buffers))))))
	  (tab-line-switch-to-next-tab))
      (if (and (equal key ?h)
	       (not (equal (current-buffer) (car (tab-line-tabs-window-buffers)))))
	  (tab-line-switch-to-prev-tab)))))

(defun nbm-tab-line-previous-buffer ()
  "Jump to the previous tab in the tab line list."
  (interactive)
  (unless (equal (current-buffer)
		 (car (tab-line-tabs-window-buffers)))
    (tab-line-switch-to-prev-tab)))

(defun nbm-tab-line-next-buffer ()
  "Jump to the next tab in the tab line list."
  (interactive)
  (unless (equal (current-buffer)
		 (car (last (tab-line-tabs-window-buffers))))
    (tab-line-switch-to-next-tab)))

(defun nbm-yasnippet-quick-new ()
  "Create a new snippet quickly."
  (interactive)
  (let (name key content)
    (setq name (read-string "Enter a name of a new snippet: "))
    (setq key (read-string "Enter a key of a new snippet: "))
    (setq content (read-string "Enter a content of a new snippet: " name))
    (find-file (nbm-f (format "nbm-user-settings/snippets/%s/%s"
			      major-mode name)))
    (insert (format "# -*- mode: snippet -*-
# name: %s
# key: %s
# --
%s" name key content))
    (save-buffer) (kill-buffer)
    (message (concat "Created a snippet file:"
		     (nbm-f (format "nbm-user-settings/snippets/%s/%s"
				    major-mode name))))))

(defun nbm-yasnippet-delete ()
  "Delete a snippet."
  (interactive)
  (let (file-name)
    (setq file-name (read-file-name
		"Choose the snippet to delete: "
		(nbm-f (format "nbm-user-settings/snippets/%s/" major-mode))
		nil t nil 'nbm-file-name-non-dot-p))
    (find-file file-name)
    (when (equal ?y (read-char "Do you want to delete this snippet? (type y for yes)"))
      (delete-file file-name)
      (message (format "File deleted: %s" file-name)))
    (kill-buffer)))

(defun nbm-git-merge ()
  "Run a simple git merge tool in the current file."
  (interactive)
  (save-excursion
    (beginning-of-buffer) (smerge-next)
    (let (choice)
      (while (< (point) (point-max))
	(setq choice (read-char "a) keep all\nu) keep upper\nl) keep lower"))
	(cond ((equal choice ?a) (smerge-keep-all))
	      ((equal choice ?u) (smerge-keep-upper))
	      ((equal choice ?l) (smerge-keep-lower)))
	(setq choice (read-char "n) go to the next conflict\np) go to the previous conflict"))
	(cond ((equal choice ?n) (smerge-next))
	      ((equal choice ?p) (smerge-prev))))))
  (message "The merge tool has scanned the whole file."))
