(defun nbm-git-init ()
  "Initiate git repository. Ask if clone it to github."
  (interactive)
  (when (equal ?y (read-char "Create a new git repository from the current directory? (y or n)"))
    (shell-command "git init")
    (when (equal ?y (read-char "Do you want to publish the current repository to github? (Type y for yes): "))
      (nbm-git-publish-to-github))))

(defun nbm-git-publish-to-github ()
  "Publish the current git repository to github."
  (interactive)
  (let (confirm)
    (let (repo-name choice access username gh)
      (setq username (substring (shell-command-to-string "git config user.name") 0 -1))
      (setq repo-name (read-string (concat "Enter a name for the new repository (no space!): ")))
      (setq access (completing-read "Choose the accessibility: " '("private" "public")))
      (if (file-exists-p "/opt/homebrew/bin/gh")
	 (setq gh "/opt/homebrew/bin/gh") (setq gh "gh"))
      (shell-command (format "%s repo create %s --%s" gh repo-name access))
      (shell-command (format "git remote add origin https://github.com/%s/%s.git" username repo-name))
      (shell-command "git branch -M main")
      (shell-command "git push -u origin main")
      (if (string-search (format "From https://github.com/%s/%s.git" username repo-name)
			 (shell-command-to-string "git ls-remote --exit-code"))
	  (message (format "The following repository has been created.
https://github.com/%s/%s.git" username repo-name))
	(message (format "The repository was not created.
Make sure that you have installed GitHub CLI and run the following command in a terminal.
gh auth login"))))))

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
