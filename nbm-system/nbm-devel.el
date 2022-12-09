(defun nbm-toggle-nbm-home ()
  "Delete nbm-home.txt if it exists. If not, create one with Dropbox/newbiemacs in it."
  (interactive)
  (save-excursion
    (if (file-exists-p (nbm-root-f "nbm-home.txt"))
	(progn
	  (delete-file (nbm-root-f "nbm-home.txt"))
	  (message "nbm-home.txt deleted.")
	  )
      (progn
	(find-file (nbm-root-f "nbm-home.txt"))
	(insert (concat (getenv "HOME") "/Dropbox/newbiemacs/"))
	(save-buffer) (kill-buffer)
	(message "nbm-home.txt created. (contents:~/Dropbox/newbiemacs)")
	))))

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

(defun nbm-toggle-pdf-viewer ()
  (interactive)
  (if (equal (nth 1 (car TeX-view-program-selection)) "PDF Tools")
      (progn
	(setq TeX-view-program-list
	      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o"))
	      TeX-view-program-selection '((output-pdf "Skim")))
	(setq openwith-associations '(("\\.pdf\\'" "open" (file))
				      ("\\.hwp\\'" "open" (file))
				      ("\\.xlsx\\'" "open" (file))
				      ("\\.djvu\\'" "open" (file))))
	(message "PDF viewer is now Skim.")
	)
    (progn
      (pdf-tools-install)
      (setq auto-revert-interval 0.1)
      (global-auto-revert-mode)
      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	    TeX-source-correlate-start-server t)
      (setq openwith-associations '(
				    ("\\.hwp\\'" "open" (file))
				    ("\\.xlsx\\'" "open" (file))
				    ("\\.djvu\\'" "open" (file))
				    ))
      (message "PDF viewer is now pdf-tools.")
      )
    )
  )

