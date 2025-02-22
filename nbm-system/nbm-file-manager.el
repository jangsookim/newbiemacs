(defun nbm-f (filename)
  "Return FILENAME with prefix *nbm-home*."
  (concat *nbm-home* filename))

(defun nbm-root-f (filename)
  "Return FILENAME with prefix ~/nbm-root/"
  (concat (getenv "HOME") "/nbm-root/" filename))

(defun nbm-append (last list)
  "Append LAST at the end of LIST."
  (reverse (cons last (reverse list))))

(defun nbm-find-file-with-extension (ext)
  "Find a file with extension EXT in the EXT folder.
EXT should be pdf, tex, el, or sage."
  (let (file dir file-list)
    (setq dir (nbm-f (concat ext "/")))
    (if (equal ext "pdf")
	(setq file-list (directory-files-recursively dir "[.]pdf$\\|[.]djvu$"))
      (setq file-list (directory-files-recursively dir (format "^[^.].*[.]%s$" ext))))
    (unless (equal ext "pdf")
      (setq file-list (nbm-sort-files-by-modified-time file-list)))
    (setq file-list (mapcar (lambda (arg) (cons (substring arg (length dir) nil) arg)) file-list))
    (when (member ext '("tex" "pdf"))
      (setq file-list (cons (nbm-f ext) file-list)))
    (helm :sources (helm-build-sync-source "find-file"
		     :candidates file-list
		     :action 'helm-type-file-actions)
	  :prompt "Find files: "
	  :buffer "*helm find files*")))

(defun nbm-find-pdf ()
  "Find a file in the pdf folder."
  (interactive)
  (nbm-find-file-with-extension "pdf"))

(defun nbm-find-tex ()
  "Find a tex file in the tex folder."
  (interactive)
  (nbm-find-file-with-extension "tex"))

(defun nbm-find-el ()
  "Find a el file in the el folder."
  (interactive)
  (nbm-find-file-with-extension "el"))

(defun nbm-find-sage ()
  "Find a sage file in the sage folder."
  (interactive)
  (nbm-find-file-with-extension "sage"))

(defun nbm-find-misc ()
  "Find a file in the misc folder."
  (interactive)
  (let (file dir file-list)
    (setq dir (nbm-f "misc/"))
    (setq file-list (directory-files-recursively dir "^[^.].*[^~]$" nil
						 (lambda (arg) (not (string-search "/." arg)))))
    (setq file-list (mapcar (lambda (arg) (cons (substring arg (length dir) nil) arg)) file-list))
    (helm :sources (helm-build-sync-source "find-file"
		     :candidates file-list
		     :action 'helm-type-file-actions)
	  :prompt "Find files: "
	  :buffer "*helm find files*")))

(defun nbm-recent-file-with-extension (ext)
  "Find a recent file with extension EXT.
EXT should be tex, pdf, el, or sage."
  (defun nbm-temp-insert ()
    (insert (concat ext "$ ")))
  (minibuffer-with-setup-hook 'nbm-temp-insert (call-interactively 'helm-recentf)))

(defun nbm-recent-pdf ()
  "Find a recent pdf file."
  (interactive)
  (nbm-recent-file-with-extension "pdf"))

(defun nbm-recent-tex ()
  "Find a recent tex file."
  (interactive)
  (nbm-recent-file-with-extension "tex"))

(defun nbm-recent-org ()
  "Find a recent org file."
  (interactive)
  (nbm-recent-file-with-extension "org"))

(defun nbm-recent-el ()
  "Find a recent el file."
  (interactive)
  (nbm-recent-file-with-extension "el"))

(defun nbm-recent-sage ()
  "Find a recent sage file."
  (interactive)
  (nbm-recent-file-with-extension "sage"))

(defun nbm-add-to-misc-symlinks ()
  "Create a symbolic link of the current file in the misc-symlinks folder."
  (interactive)
  (if (equal system-type 'windows-nt)
      (progn
	(kill-new (format "mklink \"%smisc/symlinks/%s\" \"%s\""
			  *nbm-home* (read-string "Enter the symlink file name: "
						  (file-name-nondirectory (nbm-get-file-name)))
			  (nbm-get-file-name)))
	(message (format "Command copied in the clipboard. Past it in the command prompt run as administrator.")))
    (progn
      (shell-command (format "ln -s \"%s\" \"%smisc/symlinks/%s\""
			     (nbm-get-file-name) *nbm-home*
			     (read-string "Enter the symlink file name: "
					  (file-name-nondirectory (nbm-get-file-name)))))
      (message (format "A symbolic link created in the following directory.\n%s" (nbm-f "misc/symlinks/"))))))

(defun nbm-add-to-symlinks ()
  "Create a symbolic link of the current file in the tex or misc-symlinks folder."
  (interactive)
  (if (equal (file-name-extension (nbm-get-file-name)) "tex")
      (nbm-latex-add-to-symlinks)
    (nbm-add-to-misc-symlinks)))

(defun nbm-new-file ()
  "Create a new file with a chosen EXTENSION in the folder newbiemacs/EXTENSION."
  (interactive)
  (let (extension)
    (setq extension (read-char "Choose the extension of the file you are looking for:
t) tex (default)
s) sage
e) el"))
    (defun nbm-temp-insert ()
      (insert extension "$ ") (kill-buffer extension))
    (if (memq extension (list ?t ?s ?e ?\^M))
        (cond ((memq extension '(?t ?\^M)) (nbm-latex-new-file))
              ((equal extension ?s) (message "sage under construction"))
              ((equal extension ?e) (message "el under construction")))
      (message "Wrong choice of extension!"))))

(defun nbm-find-extensions-with-same-file-name ()
  "Find a list of extensions with the same file name as the current file in the current folder."
  (let (ext-list)
    (dolist (file (nbm-find-files-with-same-name))
      (setq ext-list (cons (file-name-extension file) ext-list)))
    (setq ext-list (reverse ext-list))))

(defun nbm-copy-to-desktop ()
  "Copy the current file with a choice of extension to Desktop."
  (interactive)
  (let (choice-list choice-letter-list filename choice extension newfile prompt)
    (if (equal major-mode 'dired-mode)
        (progn
          (setq filename (file-name-sans-extension (dired-get-filename)))
          (setq extension (file-name-extension (dired-get-filename)))
          (setq newfile (format "%s/%s.%s" *nbm-desktop* (file-name-nondirectory filename) extension)))
      (progn
        (setq filename (file-name-sans-extension (buffer-file-name)))
        (setq choice-list (nbm-find-extensions-with-same-file-name))
        (setq choice-letter-list (nbm-choice-letter-list choice-list))
        (setq prompt (format "Choose a file extension of %s:\n" filename))
        (dotimes (n (length choice-list))
          (setq prompt (format "%s %s) %s" prompt
                               (nth n choice-letter-list)
                               (nth n choice-list))))
        (setq choice (string (read-char prompt)))
        (dotimes (n (length choice-list))
          (if (equal choice (nth n choice-letter-list))
              (setq extension (nth n choice-list))))
        (setq newfile (format "%s/%s.%s" *nbm-desktop* (file-name-nondirectory filename) extension))))
    (copy-file (format "%s.%s" filename extension) newfile 1)
    (message (format "Copied: %s" newfile))))

(defun nbm-choice-letter-list (choice-list)
  "Return a list like (\"p\" \"e\" \"h\" \"1\") if CHOICE-LIST is (\"pdf\" \"el\" \"html\" \"ppt\")."
  (let (choice letter letter-list num-list)
    (setq num-list (number-sequence 1 20))
    (dolist (choice choice-list)
      (setq letter (substring choice 0 1))
      (when (member letter letter-list)
        (setq letter (number-to-string (car num-list)))
        (setq num-list (cdr num-list)))
      (setq letter-list (cons letter letter-list)))
    (setq letter-list (reverse letter-list))))

(defun nbm-find-files-with-same-name ()
  "Find a list of files with the same name as the current file in the current folder."
  (directory-files "." nil (format "\\`%s[.].*[^~]\\'"
                                   (file-name-sans-extension
                                    (file-name-nondirectory (buffer-file-name))))))

(defun nbm-show-in-finder (&optional file-name)
  "Open Finder on the current folder."
  (interactive)
  (unless file-name
    (setq file-name (if (equal system-type 'windows-nt)
			(nbm-get-dir-name)
		      (nbm-get-file-name))))
  (cond ((equal system-type 'windows-nt)
	 (shell-command (format "start %s" file-name)))
	((equal system-type 'darwin)
	 (shell-command (format "open -R \"%s\"" file-name)))
	((equal system-type 'gnu/linux)
	 (let (process-connection-type)
	   (start-process "" nil "nautilus" "--browser" file-name)))))

(defun nbm-move-to-folder (file)
  "Move FILE to one of the following folders.
current folder, inbox, desktop, pdf, trash-bin"
  (let (choice)
    (setq choice (read-char (format "Move %s to:
c) current folder: %s
i) inbox
d) desktop
p) pdf
x) trash-bin
q) quit" file (nbm-get-dir-name))))
    (if (equal choice ?x)
	(if (file-directory-p file)
	    (delete-directory file t t)
	  (delete-file file t)))
    (when (member choice '(?c ?i ?d ?p))
      (setq new-file (read-string "Enter the new file name : "
				  (file-name-nondirectory file))))
    (when (equal choice ?i)
      (unless (file-exists-p (nbm-f "inbox/"))
	(make-directory (nbm-f "inbox/")))
      (rename-file file (concat (nbm-f "inbox/") new-file) 1))
    (if (equal choice ?p)
	(rename-file file (concat (nbm-f "pdf/") new-file) 1))
    (if (equal choice ?c)
	(rename-file file (concat (nbm-get-dir-name) new-file) 1))
    (if (equal choice ?d)
	(rename-file file (format "%s/%s" *nbm-desktop* new-file) 1))
    (if (equal choice ?q) (message "Aborted."))))

(defun nbm-files-from-screenshot (&optional include-dir)
  "Return the list of files in the folders in *nbm-screenshots* sorted by modified time.
Directories and files starting with ., $, or # will be ignored.
If INCLUDE-DIR is non-nil, consider directories as well."
  (let (dir file-list)
    (dolist (dir *nbm-screenshots*)
      (setq file-list (append file-list (directory-files dir t "\\`[^.$#]"))))
    (unless include-dir
      (setq file-list (-remove 'file-directory-p file-list)))
    (nbm-sort-files-by-modified-time file-list)))

(defun nbm-move-newest-file ()
  "Move the newest file in the folders in *nbm-screenshots*."
  (interactive)
  (nbm-move-to-folder (car (nbm-files-from-screenshot nil))))

(defun nbm-get-lowest-dir-name ()
  "Return the lowest directory name of the current file."
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory (buffer-file-name)))))

(defun nbm-timestamp ()
  (interactive)
  (insert (format-time-string " %Y-%m-%d")))

(defun nbm-query-replace-in-dir (dir from-string to-string)
  "Replace FROM-STRING by TO-STRING in all files in DIR."
  (let (file-list file)
    (setq file-list (directory-files dir t "\\`[^.].*[^~]\\'"))
    (while file-list
      (setq file (car file-list))
      (setq file-list (cdr file-list))
      (if (file-directory-p file)
          (progn
            (nbm-query-replace-in-dir file from-string to-string))
        (when (or (string= (file-name-extension file) "org")
                  (string= (file-name-extension file) "tex")
                  (string= (file-name-extension file) "sh")
                  (string= (file-name-extension file) "el")
                  (string= (file-name-extension file) "html"))
          (find-file file)
          (text-mode)
          (goto-char (point-min))
          (query-replace from-string to-string)
          (save-buffer) (kill-buffer))))))

(defun nbm-update-string-in-newbiemacs ()
  "Replace FROM-STRING by TO-STRING in all files in newbiemacs."
  (interactive)
  (let (from-string to-string)
    (setq from-string (read-string "File name to change from: "))
    (setq to-string (read-string "File name to change to: "))
    (nbm-query-replace-in-dir (nbm-f "") from-string to-string)
    (message (format "%s has been replaced by %s" from-string to-string))))

(defun nbm-update-string-in-current-dir ()
  "Replace FROM-STRING by TO-STRING in all files in the current directory."
  (interactive)
  (let (from-string to-string)
    (setq from-string (read-string "String to change from: "))
    (setq to-string (read-string "String to change to: "))
    (nbm-query-replace-in-dir (nbm-get-dir-name)
			      from-string to-string)
    (message (format "%s has been replaced by %s" from-string to-string))))

(defun nbm-update-string-in-el ()
  "Replace FROM-STRING by TO-STRING in all files in newbiemacs/el."
  (interactive)
  (let (from-string to-string)
    (setq from-string (read-string "String to change from: "))
    (setq to-string (read-string "String to change to: "))
    (nbm-query-replace-in-dir (nbm-f "el") from-string to-string)
    (message (format "%s has been replaced by %s" from-string to-string))))

(defun nbm-delete-double-empty-lines ()
  "Delete all double empty lines in the current buffer."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward "\n\n\n" nil t) (backward-char 3)
      (kill-line))))

(defun nbm-file-last-modified-time (file)
  "Return the last modified time of FILE."
  (nth 5 (file-attributes file)))

(defun nbm-time< (file-A file-B)
  "Return t if file-A is earlier than file-B and nil otherwise."
  (let (a b A B done answer)
    (setq A (nbm-file-last-modified-time file-A))
    (setq B (nbm-file-last-modified-time file-B))
    (while (and A (not done))
      (setq a (pop A) b (pop B))
      (if (< a b) (setq answer t done t))
      (if (> a b) (setq done t)))
    answer))

(defun nbm-time> (file-A file-B)
  "Return t if file-A is newer than file-B and nil otherwise."
  (nbm-time< file-B file-A))

(defun nbm-newest-file (files)
  "Return the newest file in the list FILES of filenames except directories."
  (let (newest file)
    (while (and (not newest) files)
      (setq file (pop files))
      (unless (file-directory-p file)
	(setq newest file)))
    (while files
      (setq file (pop files))
      (if (and (nbm-time< newest file)
	       (not (file-directory-p file)))
	  (setq newest file)))
    newest))

(defun nbm-newest-downloaded-file (ext-list)
  "Return the newest downloaded file with extension in EXT-LIST."
  (let (reg-str ext)
    (setq reg-str "")
    (dolist (ext ext-list)
      (if (equal reg-str "")
	  (setq reg-str (format "[.]%s$" ext))
	(setq reg-str (format "%s\\|[.]%s$" reg-str ext))))
    (nbm-newest-file (directory-files *nbm-downloads* t reg-str))))

(defun nbm-downloaded-files ()
  "Return the list of downloaded files sorted by modified time."
  (let (file-list)
    (setq file-list (nbm-sort-files-by-modified-time (directory-files *nbm-downloads* t)))
    (setq file-list (-remove (lambda (file)
			       (equal "." (substring (file-name-nondirectory file) 0 1)))
			     file-list))
    file-list))

(defun nbm-exclude-file-extensions (file-list ext-list)
  "Remove the directories or files in FILE-LIST with extension in EXT-LIST."
  (seq-remove (lambda (file) (or (member (file-name-extension file) ext-list)
				 (file-directory-p file)))
	      file-list))

(defun nbm-open-downloaded-file ()
  "Open a downloaded file."
  (interactive)
  (let (file)
    (setq file (completing-read "Choose a file to open: "
				  (nbm-exclude-file-extensions
				   (nbm-downloaded-files) '("ini" "BIN"))))
    (find-file file)))

(defun nbm-sort-files-by-modified-time (files)
  "Return the sorted list of files by modified time."
  (interactive)
  (sort files 'nbm-time>))

(defun nbm-rename-current-file ()
  "Rename the current file."
  (interactive)
  (let (old new choice pos)
    (setq old (file-name-nondirectory (buffer-file-name)))
    (setq new (read-string "Enter a new file name of the current file: " old))
    (setq choice (read-char (format "Rename the file?: (Type y for yes.)\nOld name: %s\nNew name: %s" old new)))
    (when (equal choice ?y)
      (setq pos (point))
      (rename-file old new) (find-file new) (kill-buffer old)
      (goto-char pos)
      (message "File name changed."))))

(defun nbm-save-as ()
  "Save-as the current file (or the content of the buffer
if the buffer is not associated with a file.)."
  (interactive)
  (let (old new choice pos content)
    (if buffer-file-name
	(progn
	  (setq old (file-name-nondirectory (buffer-file-name)))
	  (setq new (read-string "Enter a file name to save-as the current file: " old))
	  (copy-file old new))
      (progn
	(setq new (read-string "Enter a file name to save-as the buffer content: "))
	(setq content (buffer-string))
	(find-file new) (insert content) (save-buffer) (kill-buffer)))
    (message (concat "Saved as: " new))))

(defun nbm-path-string (path)
  "Make PATH a valid string. For example,
\"~/this is/an example.txt\" will be changed to
\"~/this\\ is/an\\ example.txt\" on MacOS
and to \"\"~/this is/an example.txt\"\" on Windows."
  (if (equal system-type 'windows-nt)
      (format "\"%s\"" path)
    (replace-regexp-in-string " " "\\\\ " path)))

(defun nbm-file-name-non-dot-p (file)
  "Return t if FILE does not start with the dot . symbol."
  (not (equal (substring (file-name-nondirectory file) 0 1) ".")))


(defun nbm-make-unique-filename (dir file ext)
  "Return a unique filename of the form FILE1.EXT or FILE2.EXT, etc., in DIR.
DIR may or may not end with \"/\"."
  (let ((num 1))
    (when (equal (substring dir -1 nil) "/")
      (setq dir (substring dir 0 -1)))
    (while (file-exists-p (format "%s/%s%s.%s" dir file num ext))
      (setq num (1+ num)))
    (format "%s/%s%s.%s" dir file num ext)))

(defun nbm-kill-all-buffers-with-extension ()
  "Kill all unmodified buffers with a selected extension."
  (interactive)
  (let (ext file)
    (when (buffer-file-name)
      (setq ext (file-name-extension (buffer-file-name))))
    (setq ext (read-string "Choose an extension to kill buffers: " ext))
    (dolist (buf (buffer-list))
      (setq file (buffer-file-name buf))
      (when (and file
		 (equal (file-name-extension file) ext)
		 (not (buffer-modified-p buf)))
	(kill-buffer buf)))))
