
(defun nbm-find-pdf ()
  "Find a pdf file in the pdf folder."
  (interactive)
  (defun nbm-temp-insert () (insert "pdf$ ") (kill-buffer "pdf"))
  (find-file (nbm-f "pdf/"))
  (minibuffer-with-setup-hook 'nbm-temp-insert (call-interactively 'helm-projectile)))

(defun nbm-find-tex ()
  "Find a tex file in the tex folder."
  (interactive)
  (defun nbm-temp-insert () (insert "tex$ ") (kill-buffer "tex"))
  (find-file (nbm-f "tex/"))
  (minibuffer-with-setup-hook 'nbm-temp-insert (call-interactively 'helm-projectile)))

(defun nbm-find-el ()
  "Find a el file in the el folder."
  (interactive)
  (defun nbm-temp-insert () (insert "el$ ") (kill-buffer "el"))
  (find-file (nbm-f "el/"))
  (minibuffer-with-setup-hook 'nbm-temp-insert (call-interactively 'helm-projectile)))

(defun nbm-find-sage ()
  "Find a sage file in the sage folder."
  (interactive)
  (defun nbm-temp-insert () (insert "sage$ ") (kill-buffer "sage"))
  (find-file (nbm-f "sage/"))
  (minibuffer-with-setup-hook 'nbm-temp-insert (call-interactively 'helm-projectile)))

(defun nbm-find-file-with-extension ()
  "Find a file with a chosen EXTENSION in the folder newbiemacs/EXTENSION."
  (interactive)
  (let (extension)
    (setq extension (read-char "Choose the extension of the file you are looking for:
p) pdf (default)
t) tex
o) org
s) sage
e) el"))
    (if (memq extension (list ?t ?p ?s ?e ?\^M ?o))
          (progn
            (setq extension (cond ((equal extension ?t) "tex")
                                  ((memq extension '(?p ?\^M)) "pdf")
                                  ((equal extension ?o) "org")
                                  ((equal extension ?s) "sage")
                                  ((equal extension ?e) "el")))
	    (defun nbm-temp-insert ()
	      (insert extension "$ ") (kill-buffer extension))
            (find-file (nbm-f (concat extension "/")))
            (minibuffer-with-setup-hook 'nbm-temp-insert (call-interactively 'helm-projectile))
            )
      (message "Wrong choice of extension!")
          )))

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

(defun nbm-f (filename)
  "Return FILENAME with prefix *nbm-home*."
  (concat *nbm-home* filename))

(defun nbm-rgrep ()
  "Do rgrep on the current folder on the files *.el *.tex *.org."
  (interactive)
  (let (search-key)
    (setq search-key (read-string "Enter search key words: "))
    (rgrep search-key "*.el *.tex *.org " ".")))

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
  (shell-command (concat "open -R \"" (nbm-get-file-name) "\"")))

(defun nbm-show-trash-bin ()
  "Open Finder on the trash bin."
  (interactive)
  (unless (file-exists-p trash-directory)
    (make-directory trash-directory))
  (shell-command (format "open -R \"%s\"" trash-directory)))

(defun nbm-move-files-from-downloads ()
  "Move the files in Downloads to various folders."
  (interactive)
  (let (file file-list choice)
    (setq file-list (directory-files *nbm-downloads* t "\\`[^.$#]"))
    (while (and file-list (not (equal choice ?q)))
      (setq file (car file-list))
      (setq file-list (cdr file-list))
      (setq choice (read-char (concat "Move " file " to:\ni) inbox d) desktop x) trash-bin q) quit")))
      (if (equal choice ?x)
	  (if (file-directory-p file)
	      (delete-directory file t t)
	    (delete-file file t))
	  )
      (when (equal choice ?i)
	(unless (file-exists-p (nbm-f "inbox/"))
	  (make-directory (nbm-f "inbox/")))
        (rename-file file (concat (nbm-f "inbox/") (file-name-nondirectory file)) 1))
      (if (equal choice ?d)
          (rename-file file (concat *nbm-desktop* (file-name-nondirectory file)) 1)))
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

(defun nbm-newest-file (files)
  "Return the newest file in the list FILES of filenames."
  (let (newest file)
    (setq newest (pop files))
    (while files
      (setq file (pop files))
      (if (nbm-time< newest file)
	  (setq newest file)))
    newest))
