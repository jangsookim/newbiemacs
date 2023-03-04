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

