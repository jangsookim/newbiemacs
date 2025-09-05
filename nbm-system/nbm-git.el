(defun nbm-git-init ()
  "Initiate a git repository. Ask if publish it to GitHub."
  (interactive)
  (let (warning)
    (if (file-exists-p "./.git")
	(message "The current directory is already a git repository.")
      (when (equal ?y (read-char (format "Create a new git repository from the current directory? (type y or n)
Current directory: %s" (nbm-get-dir-name))))
	(setq warning (shell-command-to-string "git init --quiet"))
	(if (not (equal warning ""))
	    (message warning)
	  (when (equal ?y (read-char "Do you want to publish the current repository to GitHub? (type y for yes): "))
	    (nbm-git-publish-to-github)))))))

(defun nbm-git-publish-to-github ()
  "Publish the current git repository to github."
  (interactive)
  (let (remote repo-name access username gh)
    (setq remote (shell-command-to-string "git remote -v"))
    (if (not (equal remote ""))
	(message (format "There is an existing remote repository.\n%s" remote))
      (progn
	(if (file-exists-p "/opt/homebrew/bin/gh")
	    (setq gh "/opt/homebrew/bin/gh") (setq gh "gh"))
	(setq username (substring (shell-command-to-string (format "%s api user --jq .login" gh)) 0 -1))
	(unless username 
	  (message "Failed to connect to GitHub.
Make sure that you have installed GitHub CLI and run the following command in a terminal.
gh auth login"))
	(when username
	  (setq repo-name (read-string (concat "Enter a name for the new GitHub repository. (You must only use alpabets, numbers, and dashes.)\n: ")
				       (file-name-nondirectory (directory-file-name (nbm-get-dir-name)))))
	  (setq repo-name (string-replace " " "-" repo-name))
	  (setq access (completing-read "Choose the accessibility: " '("private" "public")))
	  (shell-command (format "%s repo create %s --%s" gh repo-name access))
	  (shell-command (format "git remote add origin https://github.com/%s/%s.git" username repo-name))
	  (shell-command "git branch -M main")
	  (unless (string-match "^No commits yet$" (shell-command-to-string "git status"))
	    (shell-command "git push -u origin main"))
	  (if (string-search (format "From https://github.com/%s/%s.git" username repo-name)
			     (shell-command-to-string "git ls-remote --exit-code"))
	      (message (format "The following GitHub repository has been created.
https://github.com/%s/%s.git" username repo-name))
	    (message (format "Failed to create the following GitHub repositiory.
https://github.com/%s/%s.git" username repo-name))))))))

(defun nbm-git-merge ()
  "Run a simple git merge tool in the current file."
  (interactive)
  (beginning-of-buffer) (re-search-forward "^<<<<<<< HEAD$") (beginning-of-line)
  (let (choice quit)
    (while (< (point) (point-max))
      (setq choice (read-char "a) keep all\nu) keep upper\nl) keep lower\ns) skip\nother key) quit"))
      (cond ((equal choice ?a) (smerge-keep-all))
	    ((equal choice ?u) (smerge-keep-upper))
	    ((equal choice ?l) (smerge-keep-lower))
	    ((equal choice ?s))
	    (t (setq quit t)))
      (setq choice (read-char "n) go to the next conflict\np) go to the previous conflict\nother key) quit"))
      (cond ((equal choice ?n) (smerge-next))
	    ((equal choice ?p) (smerge-prev))
	    (t (setq quit t)))))
  (message "The merge tool has scanned the whole file."))
