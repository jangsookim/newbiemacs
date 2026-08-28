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
  (beginning-of-buffer) (re-search-forward "^<<<<<<< HEAD") (beginning-of-line)
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

(defun nbm-git--newest-commit-in-log ()
  "Return the newest commit displayed in the current Magit log."
  (save-excursion
    (let ((reverse (member "--reverse" magit-buffer-log-args))
	  commit finished)
      (goto-char (if reverse (point-max) (point-min)))
      (beginning-of-line)
      (while (and (not commit) (not finished))
	(setq commit (magit-commit-at-point))
	(unless commit
	  (if (if reverse (bobp) (eobp))
	      (setq finished t)
	    (forward-line (if reverse -1 1)))))
      commit)))

(defun nbm-git--write-revision-file (revision file destination)
  "Write FILE at Git REVISION to DESTINATION."
  (with-temp-buffer
    (unless (zerop (magit-git-insert
		    "show" (concat revision ":" file)))
      (error "Could not read %s at revision %s" file revision))
    (write-region (point-min) (point-max) destination nil 'silent)))

(defun nbm-git--filter-string (program arguments input)
  "Run PROGRAM with ARGUMENTS, passing INPUT on standard input.
Return standard output, or nil if the process fails."
  (let ((error-file (make-temp-file "nbm-git-filter-error-")))
    (unwind-protect
	(with-temp-buffer
	  (insert input)
	  (let ((status
		 (apply #'call-process-region
			(point-min) (point-max) program t
			(list t error-file) nil arguments)))
	    (and (integerp status)
		 (zerop status)
		 (buffer-string))))
      (delete-file error-file))))

(defun nbm-git--latex-diff-revise-fragment
    (fragment mode latexrevise)
  "Return FRAGMENT with the changes selected by MODE.
LATEXREVISE is the path to the latexrevise executable."
  ;; DIFAUXCMD makes the rest of its line a comment.  It is only needed in
  ;; the complete diff file and prevents latexrevise from recovering a small
  ;; isolated fragment, so remove just that comment marker first.
  (setq fragment
	(replace-regexp-in-string "%DIFAUXCMD[^\n]*" "" fragment))
  ;; Restore deleted commands before handing an isolated hunk to latexrevise.
  (setq fragment
	(replace-regexp-in-string
	 "%DIFDELCMD < \\(\\(?:.\\|\n\\)*?\\)%%%" "\\1" fragment))
  (let* ((sentinel "NBMLATEXDIFFFRAGMENTEND")
	 (begin "\\begin{document}\n")
	 (end "\n\\end{document}\n")
	 (result
	  (nbm-git--filter-string
	   latexrevise (list mode "--no-warnings")
	   ;; latexrevise expects a space after each DIF...end token.  The
	   ;; sentinel supplies it without changing the returned fragment.
	   (concat "\\documentclass{article}\n" begin fragment
		   " " sentinel end))))
    (when (and result (string-match
		       (concat (regexp-quote begin)
			       "\\(\\(?:.\\|\n\\)*\\)"
			       (regexp-quote end))
		       result))
      (replace-regexp-in-string
	(regexp-quote sentinel) ""
	(string-remove-suffix
	 (concat " " sentinel) (match-string 1 result))))))

(defun nbm-git--latex-diff-same-visible-text-p
    (old-fragment new-fragment preamble pandoc)
  "Return non-nil if OLD-FRAGMENT and NEW-FRAGMENT render as the same text.
PREAMBLE supplies document command definitions.  PANDOC is the path to
the pandoc executable."
  (let* ((separator "NBMLATEXDIFFSEMANTICSEPARATOR")
	 (document
	  (concat preamble
		  "\n\\begin{document}\n"
		  old-fragment
		  "\n\\par\n" separator "\n\\par\n"
		  new-fragment
		  "\n\\end{document}\n"))
	 (plain
	  (nbm-git--filter-string
	   pandoc '("--from=latex" "--to=plain" "--wrap=none" "--citeproc")
	   document)))
    (when (and plain (string-match (regexp-quote separator) plain))
      (let ((old-text
	     (string-trim
	      (replace-regexp-in-string
	       "[[:space:] ]+" " " (substring plain 0 (match-beginning 0)))))
	    (new-text
	     (string-trim
	      (replace-regexp-in-string
	       "[[:space:] ]+" " " (substring plain (match-end 0))))))
	(and (not (string-empty-p old-text))
	     (equal old-text new-text))))))

(defun nbm-git--latex-diff-remove-formatting-only-changes
    (pandoc latexrevise)
  "Remove formatting-only replacement hunks from the current diff buffer.
Keep the new version of each removed hunk.  Return the number removed."
  (let ((preamble
	 (save-excursion
	   (goto-char (point-min))
	   (if (search-forward "\\begin{document}" nil t)
	       (buffer-substring-no-properties (point-min) (match-beginning 0))
	     "\\documentclass{article}\n")))
	(count 0))
    (goto-char (point-min))
    (while (re-search-forward "\\\\DIFdelbegin\\(FL\\)?[ \t]*" nil t)
      (let* ((start (match-beginning 0))
	     (fl (or (match-string-no-properties 1) ""))
	     (delete-end (concat "\\DIFdelend" fl))
	     (add-begin (concat "\\DIFaddbegin" fl))
	     (add-end (concat "\\DIFaddend" fl))
	     pair-end)
	(if (and (search-forward delete-end nil t)
		 (progn
		   (skip-chars-forward " \t\r\n")
		   (looking-at (regexp-quote add-begin)))
		 (progn
		   (goto-char (match-end 0))
		   (search-forward add-end nil t))
		 (progn
		   ;; The latexdiff begin/end tokens include a trailing space.
		   (skip-chars-forward " \t")
		   (setq pair-end (point))))
	    (let* ((fragment
		    (buffer-substring-no-properties start pair-end))
		   (old-fragment
		    (nbm-git--latex-diff-revise-fragment
		     fragment "--decline" latexrevise))
		   (new-fragment
		    (nbm-git--latex-diff-revise-fragment
		     fragment "--accept" latexrevise)))
	      (if (and old-fragment new-fragment
		       (nbm-git--latex-diff-same-visible-text-p
			old-fragment new-fragment preamble pandoc))
		  (progn
		    (delete-region start pair-end)
		    (goto-char start)
		    (insert new-fragment)
		    (setq count (1+ count)))
		(goto-char pair-end)))
	  (goto-char (min (point-max) (1+ start))))))
    count))

(defun nbm-git--latex-diff-compile (tex-file)
  "Compile TEX-FILE with latexmk and return the resulting PDF file."
  (let ((latexmk (executable-find "latexmk"))
	(default-directory (file-name-directory tex-file))
	(error-file (make-temp-file "nbm-latexmk-error-"))
	(pdf-file (concat (file-name-sans-extension tex-file) ".pdf"))
	(synctex-file (concat (file-name-sans-extension tex-file) ".synctex.gz")))
    (unless latexmk
      (user-error "Cannot find the latexmk executable"))
    (unwind-protect
	(with-temp-buffer
	  (let ((status
		 (call-process latexmk nil (list (current-buffer) error-file) nil
			       "-pdf" "-interaction=nonstopmode"
			       (file-name-nondirectory tex-file))))
	    (unless (and (integerp status) (zerop status))
	      (let ((details
		     (string-trim
		      (concat
		       (buffer-string)
		       "\n"
		       (with-temp-buffer
			 (insert-file-contents error-file)
			 (buffer-string))))))
		(error "latexmk failed%s"
		       (if (string-empty-p details)
			   ""
			 (concat ": " details))))))
	  (unless (file-exists-p pdf-file)
	    (error "latexmk did not create %s" pdf-file))
	  (unless (file-exists-p synctex-file)
	    (error "latexmk did not create SyncTeX output for %s" tex-file))
	  pdf-file)
      (delete-file error-file))))

(defun nbm-git--latex-diff-create (root default-file old-revision new-revision)
  "Create a LaTeX diff between two revisions in repository ROOT.
DEFAULT-FILE is used when it is a TeX file present in both revisions."
  (let ((default-directory root)
	(program (executable-find "latexdiff"))
	(latexrevise (executable-find "latexrevise"))
	(pandoc (executable-find "pandoc")))
    (unless program
      (user-error "Cannot find the latexdiff executable"))
    (unless latexrevise
      (user-error "Cannot find the latexrevise executable"))
    (unless pandoc
      (user-error "Cannot find pandoc, which is needed to ignore LaTeX-only changes"))
    (let* ((old-files (magit-revision-files old-revision))
	   (new-files (magit-revision-files new-revision))
	   (changed-files (magit-changed-files old-revision new-revision))
	   (tex-files
	    (sort
	     (delq nil
		   (mapcar (lambda (file)
			     (and (string-match-p "\\.tex\\'" file)
				  (member file changed-files)
				  (member file new-files)
				  file))
			   old-files))
	     #'string<))
	   (default-file
	    (and default-file
		 (if (file-name-absolute-p default-file)
		     (file-relative-name default-file root)
		   default-file)))
	   (file
	    (cond
	     ((member default-file tex-files)
	      default-file)
	     ((null tex-files)
	      (user-error
	       "The selected revisions have no changed TeX file in common"))
	     ((null (cdr tex-files))
	      (car tex-files))
	     (t
	      (completing-read "LaTeX file to compare: " tex-files
			       nil t nil nil (car tex-files)))))
	   (source-directory (expand-file-name
			      (or (file-name-directory file) "") root))
	   (output-file (expand-file-name
			 "diff.tex"
			 (if (file-directory-p source-directory)
			     source-directory
			   root)))
	   (output-buffer (get-file-buffer output-file))
	   (old-file (make-temp-file "nbm-latexdiff-old-" nil ".tex"))
	   (new-file (make-temp-file "nbm-latexdiff-new-" nil ".tex"))
	   (error-file (make-temp-file "nbm-latexdiff-error-")))
      (unwind-protect
	  (progn
	    (when (and output-buffer (buffer-modified-p output-buffer))
	      (user-error "Save or discard the modified %s buffer first"
			  (buffer-name output-buffer)))
	    (nbm-git--write-revision-file old-revision file old-file)
	    (nbm-git--write-revision-file new-revision file new-file)
	    (when (and (file-exists-p output-file)
		       (not (yes-or-no-p
			     (format "Overwrite existing file %s? " output-file))))
	      (user-error "Canceled"))
	    (with-temp-buffer
	      (let ((status (call-process program nil
				  (list (current-buffer) error-file)
				  nil old-file new-file)))
		(unless (and (integerp status) (zerop status))
		  (let ((details
			 (with-temp-buffer
			   (insert-file-contents error-file)
			   (string-trim (buffer-string)))))
		    (error "latexdiff failed%s"
			   (if (string-empty-p details)
			       ""
			     (concat ": " details)))))
		(let ((ignored
		       (nbm-git--latex-diff-remove-formatting-only-changes
			pandoc latexrevise)))
		  (when (> ignored 0)
		    (message "Ignored %d LaTeX-only change%s"
			     ignored (if (= ignored 1) "" "s"))))
		(write-region (point-min) (point-max) output-file nil 'silent)))
	    (let* ((pdf-file (nbm-git--latex-diff-compile output-file))
		   (pdf-buffer (get-file-buffer pdf-file)))
	      (when (buffer-live-p output-buffer)
		(with-current-buffer output-buffer
		  (revert-buffer t t)))
	      (when (buffer-live-p pdf-buffer)
		(with-current-buffer pdf-buffer
		  (revert-buffer t t)))
	      (find-file pdf-file)
	      (message "Created and opened LaTeX diff: %s" pdf-file)))
	(delete-file old-file)
	(delete-file new-file)
	(delete-file error-file)))))

(defun nbm-git--latex-diff-select-new
    (root default-file revisions log-args newest old-revision)
  "Select the new revision after OLD-REVISION has been selected."
  (unless old-revision
    (user-error "No old revision selected"))
  (let ((default-directory root))
    (magit-log-select
      (apply-partially #'nbm-git--latex-diff-create
		       root default-file old-revision)
      "Type %p on the new revision,"
      nil revisions log-args newest)))

(defun nbm-git-latex-diff ()
  "Create a LaTeX diff by selecting two revisions from a Magit log.

The old revision initially defaults to the commit at point.  The new
revision initially defaults to the newest commit in the current log.
In a file-specific log, use that file when it is a TeX file present at
both revisions; otherwise prompt for a TeX file.  Write the result to
diff.tex beside the selected source file, compile it with latexmk, and
open diff.pdf.  Formatting-only replacements whose rendered text is
unchanged are omitted."
  (interactive)
  (require 'magit-log)
  (require 'magit-files)
  (unless (derived-mode-p 'magit-log-mode)
    (user-error "This command must be run from a Magit log"))
  (let* ((old-default (magit-commit-at-point))
	 (newest-default (nbm-git--newest-commit-in-log))
	 (root (magit-toplevel))
	 (default-file (magit-current-file))
	 (revisions (or magit-buffer-revisions
			(or (magit-get-current-branch) "HEAD")))
	 ;; A selection log does not carry the original file arguments, so
	 ;; --follow would be invalid there.
	 (log-args (remove "--follow" magit-buffer-log-args)))
    (unless old-default
      (user-error "Place point on a commit to choose the default old revision"))
    (unless newest-default
      (user-error "The current Magit log contains no commits"))
    (magit-log-select
      (apply-partially #'nbm-git--latex-diff-select-new
		       root default-file revisions log-args newest-default)
      "Type %p on the old revision,"
      nil revisions log-args old-default)))
