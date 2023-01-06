(defun nbm-f (filename)
  "Return FILENAME with prefix *nbm-home*."
  (concat *nbm-home* filename))

(defun nbm-root-f (filename)
  "Return FILENAME with prefix ~/nbm-root/"
  (concat (getenv "HOME") "/nbm-root/" filename))

(defun nbm-append (last list)
  "Append LAST at the end of LIST."
  (reverse (cons last (reverse list))))

(defun nbm-find-file-with-extension (ext search-flag)
  "Find a file with extension EXT in the EXT folder.
EXT should be tex, pdf, el, or sage.
If search-flag is non-nil, it will list files ending with EXT."
  (let (buf)
    (find-file (nbm-f (concat ext "/")))
    (setq buf (current-buffer))
    (defun nbm-temp-insert ()
      (if search-flag (insert (concat ext "$ ")))
      (kill-buffer buf))
    (minibuffer-with-setup-hook 'nbm-temp-insert (call-interactively 'helm-projectile))))

(defun nbm-find-pdf ()
  "Find a pdf file in the pdf folder."
  (interactive)
  (nbm-find-file-with-extension "pdf" nil))

(defun nbm-find-tex ()
  "Find a tex file in the tex folder."
  (interactive)
  (nbm-find-file-with-extension "tex" t))

(defun nbm-find-el ()
  "Find a el file in the el folder."
  (interactive)
  (nbm-find-file-with-extension "el" t))

(defun nbm-find-sage ()
  "Find a sage file in the sage folder."
  (interactive)
  (nbm-find-file-with-extension "sage" t))

(defun nbm-find-misc ()
  "Find a sage file in the sage folder."
  (interactive)
  (let (buf)
    (find-file (nbm-f "misc/"))
    (setq buf (current-buffer))
    (helm-projectile)))

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
              ((equal extension ?e) (message "el under construction"))
              )
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
            (setq extension (concat "." (file-name-extension (dired-get-filename))))
            (setq newfile (concat *nbm-desktop* (file-name-nondirectory filename) extension)))
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
                (setq extension (concat "." (nth n choice-list)))))
          (setq newfile (concat *nbm-desktop* (file-name-nondirectory filename) extension))))
      (copy-file (concat filename extension) newfile 1)
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

(defun nbm-show-in-finder ()
  "Open Finder on the current folder."
  (interactive)
  (if (equal system-type 'windows-nt)
      (shell-command (format "start %s" (file-name-directory (nbm-get-file-name))))
    (shell-command (format "open -R \"%s\"" (nbm-get-file-name)))))

(defun nbm-show-trash-bin ()
  "Open Finder on the trash bin."
  (interactive)
  (unless (file-exists-p trash-directory)
    (make-directory trash-directory))
  (if (equal system-type 'windows-nt)
      (shell-command (format "start %s" trash-directory))
    (shell-command (format "open -R \"%s\"" trash-directory))))

(defun nbm-move-files-from-downloads ()
  "Move the files in Downloads to various folders."
  (interactive)
  (let (file file-list choice)
    (setq file-list (sort (directory-files *nbm-downloads* t "\\`[^.$#]")
			  'nbm-time>))
    (while (and file-list (not (equal choice ?q)))
      (setq file (car file-list))
      (setq file-list (cdr file-list))
      (setq choice (read-char (format "Move %s to:
c) current folder: %s
i) inbox
d) desktop
p) pdf
x) trash-bin
q) quit" file (file-name-directory (nbm-get-file-name)))))
      (if (equal choice ?x)
	  (if (file-directory-p file)
	      (delete-directory file t t)
	    (delete-file file t)))
      (when (member choice '(?c ?i ?d ?p))
	(setq new-file (read-string "Enter the new filename: "
				    (file-name-nondirectory file))))
      (when (equal choice ?i)
	(unless (file-exists-p (nbm-f "inbox/"))
	  (make-directory (nbm-f "inbox/")))
	(rename-file file (concat (nbm-f "inbox/") new-file) 1))
      (if (equal choice ?p)
	  (rename-file file (concat (nbm-f "pdf/")
				    new-file) 1))
      (if (equal choice ?c)
	  (rename-file file (concat (file-name-directory (nbm-get-file-name))
				    new-file) 1))
      (if (equal choice ?d)
	  (rename-file file (concat *nbm-desktop* new-file) 1)))
    (if (equal choice ?q)
	(message "Aborted.")
      (message "All files in Downloads folder have been checked."))))

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
    (nbm-query-replace-in-dir (file-name-directory (nbm-get-file-name))
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
  "Return the newest file in the list FILES of filenames."
  (let (newest file)
    (setq newest (pop files))
    (while files
      (setq file (pop files))
      (if (nbm-time< newest file)
	  (setq newest file)))
    newest))

(defun nbm-sort-files-by-modified-time (files)
  "Return the sorted list of files by modified time."
  (interactive)
  (sort files 'nbm-time<))

(defun nbm-rename-current-file ()
  "Rename the current file."
  (interactive)
  (let (old new choice pos)
    (setq old (file-name-nondirectory (buffer-file-name)))
    (setq new (read-string "Enter the new name of the current file: " old))
    (setq choice (read-char (format "Rename the file?: (Type y for yes.)\nOld name: %s\nNew name: %s" old new)))
    (when (equal choice ?y)
      (setq pos (point))
      (rename-file old new) (find-file new) (kill-buffer old)
      (goto-char pos)
      (message "File name changed."))))
