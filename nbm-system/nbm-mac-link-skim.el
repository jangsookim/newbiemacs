(defun nbm-skim-get-filename ()
  "Get the filename of the pdf currently opened in Skim.
Return nil if no file is opened in Skim."
  (let (str)
    (unless (string-match "Skim got an error" (org-mac-link-skim-get-page))
      (setq str (substring (org-mac-link-skim-get-page) 9 -2))
      (setq str (string-replace "\\" "" str))
      (setq str (car (split-string str "::"))))))

(defun nbm-skim-data-init ()
  "Create a file if newbiemacs/nbm-user-settings/nbm-variables/nbm-skim-data.el does not exist."
  (unless (file-exists-p (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
    (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
    (insert "(defvar *nbm-skim-data* nil)
(setq *nbm-skim-data*
      '(
	))")))

(defun nbm-skim-data-add (key value)
  "Add a cons cell (KEY . VALUE) to *nbm-skim-data*.
This variable is then saved in newbiemacs/nbm-user-settings/nbm-variables/nbm-skim-data.el."
  (let (prompt)
    (when (nbm-skim-data-get key)
      (setq prompt (format "(%s . %s) is already in *nbm-skim-data*!\nProceed anyway? (Type y for yes.): "
			   key (nbm-skim-data-get key)))
      (when (equal ?y (read-char prompt))
	(nbm-skim-data-delete key)
	(nbm-skim-data-add key value)))
    (unless (nbm-skim-data-get key)
      (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
      (beginning-of-buffer)
      (search-forward "'(")
      (insert (format "\n\t(\"%s\" . \"%s\")" key value))
      (save-buffer) (eval-buffer) (kill-buffer))))

(defun nbm-skim-data-get (key)
  "Get the value such that (KEY . VALUE) is in *nbm-skim-data*."
  (unless (boundp '*nbm-skim-data*)
    (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
    (save-buffer) (eval-buffer) (kill-buffer))
  (cdr (assoc key *nbm-skim-data*)))

(defun nbm-skim-data-delete (key)
  "Delete the cons cell with key KEY in *nbm-skim-data*.
This variable is then saved in newbiemacs/nbm-user-settings/nbm-variables/nbm-skim-data.el."
  (find-file (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-data.el"))
  (beginning-of-buffer)
  (re-search-forward (format "^\t(\"%s\" . " key))
  (beginning-of-line) (kill-line) (kill-line)
  (save-buffer) (eval-buffer) (kill-buffer))

(defun nbm-skim-set-page-offset (offset)
  "Set a page offset for the current pdf file."
  (nbm-skim-data-add (file-name-nondirectory (nbm-skim-get-filename)) offset))

(defun nbm-skim-goto-page-run ()
  "Go to a specific page of the pdf current opened in skim."
  (interactive)
  (nbm-skim-goto-page (read-string "Page (write + or - for offset): ")))

(defun nbm-skim-goto-page (page &optional offset)
  "Go to a specific page of the pdf current opened in skim.
PAGE must be a string of the form +num, -num, or num. For example, +12, -4, or 100."
  (nbm-skim-data-init)
  (let (str)
    (setq str (nbm-skim-get-filename))
    (unless str
      (message "You must open a pdf in Skim."))
    (when str
      (if (member (substring page 0 1) '("+" "-"))
	  (nbm-skim-set-page-offset (string-to-number page))
	(progn
	  (unless offset
	    (setq offset (nbm-skim-data-get (file-name-nondirectory str)))
	    (unless offset (setq offset "0")))
	  (setq str (format "%s::%s" str (+ (string-to-number page) (string-to-number offset))))
	  (org-mac-link-skim-open str nil))))))

(defun nbm-skim-bookmark-jump ()
  "Jumpt to a bookmark of the pdf current opened in skim."
  (interactive)
  (let (page str bookmark offset txt)
    (unless (file-exists-p (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-bookmarks"))
      (make-directory (nbm-f "nbm-user-settings/nbm-variables/nbm-skim-bookmarks")))
    (setq str (nbm-skim-get-filename))
    (unless str
      (message "You must open a pdf in Skim."))
    (when str
      (setq str (file-name-sans-extension (file-name-nondirectory str)))
      (setq txt (nbm-f (format "nbm-user-settings/nbm-variables/nbm-skim-bookmarks/%s.txt" str)))
      (unless (file-exists-p txt)
	(find-file txt)
	(insert "; Any line starting with a semi-colon will be ignored.
; Each line must be something like description page number as follows.
; 5.5 Exponential structures 45
; Selecting the above line will make Skim jump to page 45 with proper offset of the current pdf.
; If you don't want the offset, write the page number starting with x as follows.
; Table of Contents x10
; 
; The following is a possible example.
;
; toc x3
; 
; Chapter 1 What is Enumerative Combinatorics? 9
; 1.1 How to count 9
; 1.2 Sets and multisets 23
; 1.3 Cycles and inversions 29
; 1.4 Descents 38\n\n")
	(save-buffer) (kill-buffer))
      (setq bookmark (split-string (f-read-text txt) "\n"))
      (setq bookmark (cl-remove-if (lambda (x) (or (length= x 0) (equal (substring x 0 1) ";"))) bookmark))
      (setq bookmark (cons "Select this if you want to edit the bookmark file." bookmark))
      (setq page (completing-read "Where to? " bookmark))
      (if (equal page (car bookmark))
	  (find-file txt)
	(progn
	  (while (equal (substring page -1 nil) " ")
	    (setq page (substring page 0 -1)))
	  (setq page (car (last (split-string page " "))))
	  (when (equal (substring page 0 1) "x")
	    (setq page (substring page 1 nil))
	    (setq offset "0"))
	  (nbm-skim-goto-page page offset))))))
