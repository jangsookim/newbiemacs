(defun nbm-toggle-nbm-home ()
  "Delete nbm-home.txt if it exists. If not, create one with Dropbox/newbiemacs in it."
  (interactive)
  (save-excursion
    (if (file-exists-p (nbm-root-f "nbm-home.txt.bak"))
	(progn
	  (delete-file (nbm-root-f "nbm-home.txt"))
	  (rename-file (nbm-root-f "nbm-home.txt.bak") (nbm-root-f "nbm-home.txt"))
	  (message "The original nbm-home.txt has been recovered."))
      (progn
	(rename-file (nbm-root-f "nbm-home.txt") (nbm-root-f "nbm-home.txt.bak"))
	(message "nbm-home.txt has been renamed to nbm-home.txt.bak")))))

(defun nbm-update-version ()
  "Update Newbiemacs version."
  (interactive)
  (find-file (nbm-root-f "nbm-system/nbm-newbie-mode.el"))
  (beginning-of-buffer)
  (re-search-forward "\\(\"Newbiemacs \\)\\([0-9]+[.][0-9]+\\)")
  (replace-match (read-string "Enter new version: ")
		 nil nil nil 2)
  (if (equal ?y (read-char "Is this okay? (y or n): "))
      (save-buffer) (revert-buffer nil t))
  (kill-buffer))

(defun nbm-devel-config ()
  "Open one of the nbm configuration files."
  (interactive)
  (let (choice)
    (setq choice (read-char "Which file do you want to open?
e) .emacs
i) nbm-init.el
c) nbm-config.org
k) nbm-sys-key-tree.org"))
    (if (equal choice ?e)
	(find-file (concat (getenv "HOME") "/.emacs")))
    (if (equal choice ?i)
	(find-file (nbm-root-f "nbm-init.el")))
    (if (equal choice ?k)
	(find-file (nbm-root-f "nbm-sys-key-tree.org")))
    (if (equal choice ?c)
	(find-file (nbm-root-f "nbm-config.org")))))
