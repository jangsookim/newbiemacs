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
  (let (file buf)
    (setq file (concat *nbm-home* (format "nbm-user-settings/nbm-variables/nbm-%s.txt" var)))
    (find-file file) (setq buf (current-buffer)) (erase-buffer)
    (insert content) (save-buffer) (kill-buffer buf)))

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

(defun nbm-look-up-dictionary ()
  "Search the word at point."
  (interactive)
  (browse-url (format "https://www.thefreedictionary.com/%s"
		      (read-string "Enter a word to look up: " (word-at-point)))))

(defun nbm-google-search ()
  "Google search."
  (interactive)
  (browse-url (format "https://www.google.com/search?q=%s"
		      (read-string "Enter a word to google: "))))

(defun nbm-paper-search (service)
  "Search a paper."
  (interactive)
  (let (author authors title search)
    (setq authors (read-string "Enter names of authors.\nNames must be separated by semicolons \";\".\nIt is recommended to write each name as \"Last name, First name\".\nYou can simply press Enter if you don't want to include this in your search.\n: "))
    (setq title (read-string "Enter a search term for title.\nYou can simply press Enter if you don't want to include this in your search.\n: "))
    (cond ((equal service "arxiv")
	   (setq search (format "https://arxiv.org/search/advanced?advanced=&terms-0-operator=AND&terms-0-term=%s&terms-0-field=author&terms-1-operator=AND&terms-1-term=%s&terms-1-field=title&classification-physics_archives=all&classification-include_cross_list=include&date-filter_by=all_dates&date-year=&date-from_date=&date-to_date=&date-date_type=submitted_date&abstracts=show&size=50&order=-announced_date_first" authors title)))
	  ((equal service "mathscinet")
	   (setq search "")
	   (if (equal authors "")
	       (setq authors nil)
	     (setq authors (split-string authors ";")))
	   (while authors
	     (setq author (pop authors))
	     (unless (equal search "") (setq search (concat search " AND ")))
	     (setq search (format "%sau:(%s)" search author)))
	   (unless (equal title "")
	     (unless (equal search "") (setq search (concat search " AND ")))
	     (setq search (format "%sti:(%s)" search title)))
	   (setq search (concat "https://mathscinet.ams.org/mathscinet/publications-search?query="
			search)))
	  ((equal service "zbmath")
	   (setq search "")
	   (if (equal authors "")
	       (setq authors nil)
	     (setq authors (split-string authors ";")))
	   (while authors
	     (setq author (pop authors))
	     (unless (equal search "") (setq search (concat search "+%26")))
	     (setq search (format "%sau:\"%s\"%%2C" search author)))
	   (unless (equal title "")
	     (unless (equal search "") (setq search (concat search "+%26")))
	     (setq search (format "%sti:%s" search (string-replace " " "+" title))))
	   (setq search (concat "https://zbmath.org/?q=" search))))
    (browse-url search)))

(defun nbm-paper-search-arxiv ()
  "Search a paper in arxiv."
  (interactive)
  (nbm-paper-search "arxiv"))

(defun nbm-paper-search-mathscinet ()
  "Search a paper in MathSciNet."
  (interactive)
  (nbm-paper-search "mathscinet"))

(defun nbm-paper-search-zbmath ()
  "Search a paper in zbmath."
  (interactive)
  (nbm-paper-search "zbmath"))

(defun nbm-paste-vertically (after)
  "Insert the current kill vertically."
  (let (str)
    (save-excursion
      (when after (forward-char))
      (setq str (split-string (current-kill 0) "\n"))
      (while str
	(save-excursion (insert (pop str)))
	(next-line)))))

(defun nbm-paste-vertically-after ()
  "Insert the current kill vertically after cursor."
  (interactive)
  (nbm-paste-vertically t))

(defun nbm-paste-vertically-before ()
  "Insert the current kill vertically before cursor."
  (interactive)
  (nbm-paste-vertically nil))

(defun nbm-list-of-buffers-with-extension (ext)
  "Return the list of buffers with file extension EXT."
  (let (bufs)
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
		 (equal (file-name-extension (buffer-file-name buf)) ext))
	(setq bufs (cons buf bufs))))
    bufs))

(defun nbm-switch-to-buffer-with-extension (ext)
  "Switch to a buffer with file extension EXT."
  (let (bufs)
    (setq bufs (mapcar #'buffer-name (nbm-list-of-buffers-with-extension ext)))
    (switch-to-buffer (completing-read "Choose a buffer:" bufs))))

(defun nbm-switch-to-buffer-with-same-extension ()
  "Switch to a buffer with the same file extension as the current buffer."
  (interactive)
  (nbm-switch-to-buffer-with-extension (file-name-extension (buffer-file-name))))

(defun nbm-find-duplicated-items (list)
  "Return the list of all duplicated items in LIST."
  (delete-dups (seq-filter
                (lambda (el) (member el (cdr (member el list))))
                list)))
