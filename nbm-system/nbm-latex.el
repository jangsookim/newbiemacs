;; Making a file name from data in tex file
(defun nbm-latex-get-title ()
  "Return the title of the current tex file."
  (save-excursion
    (let (START END)
      (goto-char (point-min))
      (when (search-forward "\\title" nil t nil)
            (if (string= (buffer-substring (point) (1+ (point))) "[")
                (forward-sexp))
            (search-forward "{")
            (setq START (point))
            (backward-char) (forward-sexp)
            (setq END (1- (point)))
            (format "%s" (buffer-substring START END))))))

(defun nbm-latex-get-authors ()
  "Return a reversely ordered list of the authors of the current tex file."
  (save-excursion
    (let (START END authors)
      (setq authors '())
      (goto-char (point-min))
      (while (search-forward "\\author{" nil t nil)
        (setq START (point))
        (backward-char) (forward-sexp)
        (setq END (1- (point)))
        (setq authors (cons (format "%s" (buffer-substring START END)) authors)))
      authors)))

(defun nbm-latex-make-filename ()
  "Return a string for a filename using title and authors.
For example, Kim, Stanton. q-integrals over order polytopes.
If there is no title, return the filename."
  (let (file-name temp title authors)
    (setq title (nbm-latex-get-title))
    (setq authors (nbm-latex-get-authors))
    (if title
        (setq file-name (concat ". " title))
      (setq file-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (while authors
      (setq temp (car (last (split-string (pop authors) " "))))
      (setq file-name (format "%s%s" temp file-name))
      (if authors (setq file-name (concat ", " file-name))))
    file-name))

(defun nbm-latex-custom-filename ()
  "Make a custom filename using nbm-latex-make-filename."
  (interactive)
  (read-string (concat "Enter the file name: ") (nbm-latex-make-filename)))

(defun nbm-latex-make-and-copy-filename ()
  "Copy the a string for a filename using title and authors."
  (interactive)
  (let (file-name)
    (setq file-name (nbm-latex-custom-filename))
    (kill-new file-name)
    (message "\"%s\" has been copied." file-name)))

(defun nbm-latex-add-to-symlinks ()
  "Create a symbolic link of the current tex file in the symlinks folder"
  (interactive)
  (let (choice)
    (setq choice (read-char "Choose the file-name scheme (default d):  t) title of paper  d) directory name"))
  (if (equal choice ?t)
      (nbm-latex-add-to-symlinks-using-title)
    (nbm-latex-add-to-symlinks-using-dir-name))))

(defun nbm-latex-add-to-symlinks-using-title ()
  "Create a symbolic link of the current tex file in the symlinks folder"
  (let (file-name)
    (setq file-name (read-string "Enter the symlink file name: " (nbm-latex-make-filename)))
    (shell-command (format "ln -s \"%s\" \"%stex/symlinks/%s.tex\""
                           (buffer-file-name) *nbm-home* file-name))
    (message (format "A symbolic link created: %s.tex" file-name))))

(defun nbm-latex-add-to-symlinks-using-dir-name ()
  "Create a symbolic link of the current tex file in the symlinks folder using the lowest directory name."
  (shell-command (format "ln -s \"%s\" \"%stex/symlinks/%s.tex\""
                         (buffer-file-name) *nbm-home*
                           (read-string "Enter the symlink file name: " (nbm-get-lowest-dir-name))))
  (message (format "A symbolic link created: %s.tex" (nbm-get-lowest-dir-name))))

(defun nbm-latex-new-file ()
  "Create a new latex file from a template."
  (interactive)
  (let (choice title filename dirname temp)
    (setq title (read-string (concat "Enter a new latex filename (default: note): ")
                             nil nil "note" nil))
    (setq dirname (concat (nbm-f "tex/") (format-time-string "%Y-%m-%d-") title))
    (setq temp (read-file-name "Choose the template file: (default is template.tex) "
			       (nbm-f "nbm-user-settings/templates/")
			       "template.tex"))
    (make-directory dirname)
    (setq filename (concat dirname "/" title ".tex"))
    (copy-file temp filename)
    (find-file filename) (goto-char (point-min))
    (when (search-forward "\\title{" nil t nil)
     (insert title))
    (search-forward "begin{document}" nil t nil)
    (next-line) (recenter-top-bottom) (save-buffer)
    (message "Created a new file.")))

(defun nbm-latex-new-macro ()
  "Add a new macro after the following flag line.

% DO NOT DELETE THIS COMMENT!!! MACROS BELOW:

If there is no such flag, such a line will be added to the previous line
of the first occurence of \"\\necommand\" or \"\\begin{document}\"."
  (interactive)
  (let (macro-name macro-body flag no-flag)
    (setq flag "% DO NOT DELETE THIS COMMENT!!! MACROS BELOW:")
    (save-excursion
      (goto-char (point-min))
      (unless (search-forward flag nil t)
	(setq no-flag t)
	(goto-char (point-min))
	(re-search-forward "\\newcommand\\|begin{document}")
	(beginning-of-line)
	(insert (format "\n%s\n\n\n" flag))
	(previous-line 2))
      (setq macro-name (read-string "Enter the macro name: "))
      (setq macro-body (read-string "Enter the macro body: "
				    (format "\\operatorname{%s}" macro-name)))
      (insert (concat "\\newcommand\\" macro-name "{" macro-body "}"))
      (if no-flag
	  (message "The following line has been added in the tex file.
\n%s\n\nDo NOT delete or modify this line." flag))
      )))

(defvar *nbm-latex-change-variable-confirm* t)

(defun nbm-latex-change-variable ()
  "Replace a variable in a math mode in current buffer."
  (interactive)
  (let (x y TYPE START END temp choice case r-start r-end)
    (setq case case-fold-search)
    (setq case-fold-search nil)
    (save-excursion
      (setq x (read-string "Variable to change from: " nil nil nil))
      (setq y (read-string "Variable to change to: " nil nil nil))
      (setq TYPE t)
      (if (use-region-p)
          (setq r-start (region-beginning) r-end (region-end))
        (setq r-start (point-min) r-end (point-max)))
      (goto-char r-start)
      (while TYPE
        (setq temp (nbm-latex-find-next-math-mode))
        (setq TYPE (nth 0 temp) START (nth 1 temp) END (nth 2 temp))
        (if TYPE
            (progn
              (nbm-latex-replace-x-y-region x y START END)
              (goto-char END)
              (if (> END r-end)
                  (setq TYPE nil))))
        ))
    (setq *nbm-latex-change-variable-confirm* t)
    (setq case-fold-search case)))

(defun nbm-latex-replace-x-y-region (x y START END)
  "Replace x by y from START to END in the current buffer if x is not macro."
  (save-excursion
    (goto-char START)
    (while (search-forward x END t nil)
      (unless (nbm-latex-is-macro 1)
        (when *nbm-latex-change-variable-confirm*
	  (if (commandp 'beacon-blink)	; Execute beacon if it's installed.
	      (beacon-blink))
          (setq choice (read-char (format "Change %s to %s?: (Type y for yes. Type ! to change everything.)" x y))))
        (if (eq choice ?!)
            (setq *nbm-latex-change-variable-confirm* nil))
        (if (or (not *nbm-latex-change-variable-confirm*) (eq choice ?y))
            (progn
              (delete-region (- (point) (string-width x)) (point))
              (insert y)))))))

(defun nbm-latex-is-macro (k)
  "Determine whether k characters from the current point represents a macro."
  (save-excursion
    (goto-char (- (point) k))
    (while (looking-at "[a-z-A-Z]") (backward-char))
    (if (equal (buffer-substring (point) (+ (point) 1)) "\\")
        t nil)))

(defun nbm-latex-find-next-math-mode()
  "Return (TYPE START END), where TYPE is [, (, $, $$, n, or nil and,
START and END are the starting and ending points of the environment
except the environment macro."
  (save-excursion
    (let (TYPE START END)
      (setq TYPE nil START nil END nil)
      (when (re-search-forward "\\\\\\[\\|\\\\(\\|\\$\\|\\\\begin{equation\\|\\\\begin{align\\|\\\\begin{multlin" nil t nil)
        (setq TYPE (buffer-substring (1- (point)) (point)))
        (when (equal (buffer-substring (1- (point)) (1+ (point))) "$$")
          (setq TYPE "$$") (forward-char))
        (when (equal (buffer-substring (1- (point)) (point)) "n")
          (search-forward "}"))
        (setq START (point)))
      (if (equal TYPE "n")
          (progn
            (LaTeX-find-matching-end)
            (search-backward "\\")))
      (if (equal TYPE "[")
          (search-forward "\\]" nil t nil))
      (if (equal TYPE "(")
          (search-forward "\\)" nil t nil))
      (if (equal TYPE "$$")
          (search-forward "$$" nil t nil))
      (if (equal TYPE "$")
          (search-forward "$" nil t nil))
      (setq END (point))
      (list TYPE START END))))

(defun nbm-latex-find-math-mode ()
  "Return (TYPE START END), where TYPE is [], (), or nil and, START
and END are the starting and ending points of the environment."
  (save-excursion
    (let (pt dm im TYPE START END)
      (setq pt (point) dm (point-min) im (point-min))
      (if (search-backward "\\[" nil t nil)
          (setq dm (point)))
      (goto-char pt)
      (if (search-backward "\\(" nil t nil)
          (setq im (point)))
      (goto-char pt)
      (if (> dm im)                     ; if you are in display math mode
          (progn (goto-char dm)
                 (search-forward "\\]" nil t nil)
                 (if (< pt (point))     ; if you are really in display math mode
                     (setq TYPE "[]" START dm END (point))
                   (setq TYPE nil START nil END nil))))
      (if (< dm im)                     ; if you are in in-line math mode
          (progn (goto-char im)
                 (search-forward "\\)" nil t nil)
                 (if (< pt (point))     ; if you are really in in-line math mode
                       (setq TYPE "()" START im END (point))
                   (setq TYPE nil START nil END nil))))
      (message TYPE)
      (list TYPE START END))))

(defun nbm-latex-copy-math-with-paren()
  "Copy the content in \\( \\) or \\[ \\] including the parentheses."
  (interactive)
  (save-excursion
    (let (TYPE START END temp)
      (setq temp (nbm-latex-find-math-mode))
      (setq TYPE (nth 0 temp) START (nth 1 temp) END (nth 2 temp))
      (if (equal TYPE "[]")             ; if you are in display math mode
          (progn
            (copy-region-as-kill START END)
            (message "Copied the content in \\[ \\] with parentheses.")))
      (if (equal TYPE "()")             ; if you are in in-line math mode
          (progn
            (copy-region-as-kill START END)
            (message "Copied the content in \\( \\) with parentheses.")))
      (unless TYPE (message "You are not in math mode!")))))

(defun nbm-latex-delete-math-with-paren()
  "Delete the content in \\( \\) or \\[ \\] including the parentheses."
  (interactive)
  (save-excursion
    (let (TYPE START END temp)
      (setq temp (nbm-latex-find-math-mode))
      (setq TYPE (nth 0 temp) START (nth 1 temp) END (nth 2 temp))
      (if (equal TYPE "[]")             ; if you are in display math mode
          (progn
            (kill-region START END)
            (message "Deleted the content in \\[ \\].")))
      (if (equal TYPE "()")             ; if you are in in-line math mode
          (progn
            (kill-region START END)
            (message "Deleted the content in \\( \\).")))
      (unless TYPE (message "You are not in math mode!")))))

(defun nbm-latex-copy-math()
  "Copy the content in \\( \\) or \\[ \\]."
  (interactive)
  (save-excursion
    (let (TYPE START END temp)
      (setq temp (nbm-latex-find-math-mode))
      (setq TYPE (nth 0 temp) START (nth 1 temp) END (nth 2 temp))
      (if (equal TYPE "[]")             ; if you are in display math mode
          (progn
            (copy-region-as-kill (+ START 3) (- END 3))
            (message "Copied the content in \\[ \\] without parentheses.")))
      (if (equal TYPE "()")             ; if you are in in-line math mode
          (progn
            (copy-region-as-kill
             (if (equal (buffer-substring START (+ START 3)) "\\( ") ; if it's like \( a+b\)
                 (+ START 3) (+ START 2))
             (if (equal (buffer-substring (- END 3) END) " \\)") ; if it's like \(a+b \)
                 (- END 3) (- END 2)))
            (message "Copied the content in \\( \\) without parentheses.")))
      (unless TYPE (message "You are not in math mode!")))))

(defun nbm-latex-delete-math()
  "Copy the content in \\( \\) or \\[ \\]."
  (interactive)
  (save-excursion
    (let (TYPE START END temp)
      (setq temp (nbm-latex-find-math-mode))
      (setq TYPE (nth 0 temp) START (nth 1 temp) END (nth 2 temp))
      (if (equal TYPE "[]")             ; if you are in display math mode
          (progn
            (kill-region (+ START 3) (- END 3))
            (message "Deleted the content in \\[ \\].")))
      (if (equal TYPE "()")             ; if you are in in-line math mode
          (progn
            (kill-region
             (if (equal (buffer-substring START (+ START 3)) "\\( ") ; if it's like \( a+b\)
                 (+ START 3) (+ START 2))
             (if (equal (buffer-substring (- END 3) END) " \\)") ; if it's like \(a+b \)
                 (- END 3) (- END 2)))
            (message "Deleted the content in \\( \\).")))
      (unless TYPE (message "You are not in math mode!")))))

(defun nbm-latex-toggle-equation ()
  "Change \\ [ \\] to \\begin{equation}...\\end{equation} or vice versa."
  (interactive)
  (save-excursion
    (let (p d e label)
      (setq p (point))
      (setq d (point-min))
      (setq e (point-min))
      (if (search-backward "\\[" nil t nil)
          (setq d (point)))
      (goto-char p)
      (if (search-backward "\\begin{equation}" nil t nil)
          (setq e (point)))
      (if (> e d)
          (progn (goto-char e)
                 (search-forward "\\end{equation}" nil t nil)
                 (if (< p (point))
                     (progn
                       (delete-region (- (point) 14) (point))
                       (insert "\\]")
                       (if (search-backward "\\label" nil t nil)
                           (setq label (point))
                         (setq label 0))
                       (if (< e label)
                           (progn (search-forward "}")
                                  (delete-region label (point))))
                       (goto-char e)
                       (delete-region e (+ e 16))
                       (insert "\\[")
                       (message "Equation has been changed to \\[ \\]."))
                   (message "You are not inside an equation!"))))
      (if (< e d)
          (progn (goto-char d)
                 (delete-region d (+ d 2))
                 (insert "\\begin{equation}")
                 (setq label (read-string "Enter a label: " "" nil nil nil))
                 (if (not (equal label ""))
                     (insert (concat "\\label{eq:" label "}")))
                 (search-forward "\\]" nil t nil)
                 (delete-region (- (point) 2) (point))
                 (insert "\\end{equation}")
                 (message "\\[ \\] has been changed to Equation."))))))

(defun nbm-latex-insert-label ()
  "Insert the label in the current environment."
  (interactive)
  (save-excursion
    (let (env label)
      (setq env (LaTeX-current-environment))
      (goto-char (car (LaTeX-env-beginning-pos-col)))
      (if (> (length env) 3)
	  (setq env (substring env 0 3)))
      (search-forward "\\begin" nil t) (forward-sexp)
      (if (member env '("equ" "ali" "mul"))
	  (setq env "eq"))
      (setq label (read-string "Enter a label: "))
      (insert (format "\\label{%s:%s}" env label)))))

(defun nbm-latex-delete-label ()
  "Delete the labels in the current environment."
  (interactive)
  (save-excursion
    (let (beg end)
      (setq beg (car (LaTeX-env-beginning-pos-col)))
      (LaTeX-find-matching-end)
      (while (search-backward "\\label" beg t)
	(when (eq ?y (read-char "Are you sure to delete this label? (Type y for yes): "))
	  (setq beg (point)) (forward-char 6) (forward-sexp)
	  (delete-region beg (point)) (delete-blank-lines))))))

(defun nbm-latex-toggle-bbl-file ()
  "Insert the bib file or remove it."
  (interactive)
  (save-excursion
    (let (b f bib-exist)
      (goto-char (point-max))
      (if (search-backward "\\bibliography{" nil t nil)
	  (progn (previous-line)
		 (kill-line) (kill-line) (kill-line) (kill-line)
		 (setq f (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
		 (insert-file (concat f ".bbl"))
		 (message "Bibtex toggled: bibtex OFF"))
	(progn (when (search-backward "\\begin{thebibliography}" nil t nil)
		 (setq bib-exist t)
		 (setq b (point))
		 (search-forward "\\end{thebibliography}" nil t nil)
		 (delete-region b (point)))
	       (unless bib-exist
		 (search-backward "\\end{document}" nil t nil)
		 (insert "\n") (previous-line))
	       (insert (format "\\bibliographystyle{abbrv}\n\\bibliography{%s}"
			       (nbm-f "nbm-user-settings/references/ref.bib")))
	       (message "Bibtex toggled: bibtex ON"))))))

(defun nbm-bib-item-create-key (bib-str choice)
  "Create a key for the bib item given by BIB-STR.
CHOICE should be a char ?1, ?2, or ?3.
CHOICE 1: Cho2022 (default)
CHOICE 2: CKL2022
CHOICE 3: ChoKimLee2022"
  (interactive)
  (let (authors year key keys a)
    (setq year (nbm-get-bibtex-entry "year" bib-str))
    (setq authors (nbm-get-bibtex-entry "author" bib-str))
    (setq authors (split-string authors " and "))
    (setq key "")
    (while authors
      (setq name (pop authors))
      ;; get the last name depending on whether name is written Jack Sparrow or Sparrow, Jack.
      (if (string-match "," name)
	  (setq name (car (split-string name ",")))
	(setq name (car (last (split-string name " ")))))
      (cond ((equal choice ?2)
	     (setq key (concat key (substring name 0 1))))
	    ((equal choice ?3)
	     (setq key (concat key name)))
	    (t (if (equal (length key) 0) (setq key (concat key name)))) ; choice 1 is default
	    )
      )
    (if year (setq key (concat key year)))
    (setq keys (nbm-latex-get-bib-key-list))
    ;; Check if the key is already used.
    (when (member key keys)
      (setq a ?a)
      (while (member (format "%s%c" key a) keys) ; Attach a or b ... if the key is already used.
	(setq a (+ a 1)))
      (setq key (format "%s%c" key a)))
    key))

(defun nbm-latex-new-bib-item ()
  "Create a bib item in the main bib file using citation data from arxiv or MathSciNet.
https://beta.mathscinet.ams.org/mathscinet/beta"
  (interactive)
  (save-excursion
    (let (str choice beg end)
      (with-output-to-temp-buffer "bib-item-temp-buffer"
	(setq str (current-kill 0))
	(switch-to-buffer "bib-item-temp-buffer")
	(insert str) (beginning-of-buffer)
	;; If the bib item is @Online, change it to @misc.
	(when (search-forward "@Online" nil t)
	  (replace-match "@misc")
	  (search-forward "eprint" nil t) (search-forward "{" nil t)
	  (setq arxiv (buffer-substring (point) (- (search-forward "}") 1)))
	  (setq url (concat "https://arxiv.org/abs/" arxiv))
	  (end-of-line)
	  (insert (concat "\n  howpublished = {{\\it Preprint}, \\href{" url "}{arXiv:"
			  arxiv "}},")))
	(beginning-of-buffer)
	;; If title has {...} make it {{...}}.
	(search-forward "title") (search-forward "{")
	(unless (equal (buffer-substring (- (point) 1) (+ (point) 1)) "{{")
	  (insert "{") (search-forward "}") (insert "}"))
	;; If the bibitem contains month, remove it.
	(beginning-of-buffer)
	(when (search-forward "month" nil t)
	  (beginning-of-line) (kill-line 2))

	(setq choice (read-char "Choose the key scheme. (Suppose the authors are Cho, Kim, and Lee.)\n1: Cho2022 (default)\n2: CKL2022\n3: ChoKimLee2022"))
	(setq key (nbm-bib-item-create-key str choice))
	;; Replace the original bib key with the new key.
	(beginning-of-buffer)
	(search-forward "{") (setq beg (point))
	(search-forward ",") (setq end (- (point) 1))
	(delete-region beg end) (backward-char) (insert key)

	(when (equal (read-char "Do you want to save this bib item? (Type y or n)") ?y)
	  (setq str (buffer-string))
	  (find-file (nbm-f "nbm-user-settings/references/ref.bib"))
	  (end-of-buffer) (insert str) (save-buffer) (kill-buffer))
	)
      (kill-buffer "bib-item-temp-buffer"))))

(defun nbm-latex-get-bib-key-list ()
  "Return the list of all bib item keys in the main bib file."
  (let (keys beg end)
    (find-file (nbm-f "nbm-user-settings/references/ref.bib"))
    (beginning-of-buffer)
    (while (re-search-forward "^[ ]*@" nil t)
      (search-forward "{") (setq beg (point))
      (search-forward ",") (setq end (- (point) 1))
      (setq keys (cons (buffer-substring beg end) keys)))
    (kill-buffer)
    keys))

(defun nbm-latex-insert-figure ()
  "Insert the most recent file from *nbm-screenshots* to ./figures."
  (let (fig files ext file choice dir)
    (setq files '())
    (dolist (dir *nbm-screenshots*)
      (if (file-exists-p dir)
	(setq files (append files (directory-files dir t "[.]jpeg\\|[.]png\\|[.]jpg")))))
    (setq newest (nbm-newest-file files))
    (setq ext (concat "." (file-name-extension newest)))
    (setq choice (read-char (concat "Move this file?: (Type y for yes.)\n" newest)))
    (when (equal choice ?y)
      (unless (file-directory-p "./figures/") (make-directory "./figures/"))
      (setq fig (read-string "Enter the figure name: "))
      (if (file-exists-p (concat "./figures/" fig ext))
	  (message (concat "./figures/" fig ext " already exists!"))
	(copy-file newest (concat "./figures/" fig ext)))
      (setq choice (read-char (concat "Delete this file?: (Type y for yes.)\n" newest)))
      (when (eq choice ?y) (delete-file newest))
      (end-of-line)
      (insert (concat " See Figure~\\ref{fig:" fig "}.\n"
		      "\n\\begin{figure}\n"
		      "  \\centering\n"
		      "  \\includegraphics[scale=.2]{./figures/" fig ext "}\n"
		      "  \\caption{}\n"
		      "  \\label{fig:" fig "}\n"
		      "\\end{figure}\n\n"))
      (goto-char (- (point) (+ 31 (length fig)))))))

;; converting code

(defun nbm-latex-toggle-pgml ()
  "Toggle latex code to pgml in the selected region."
  (interactive)
  (let (r-start r-end)
    (setq r-start (region-beginning) r-end (region-end))
    (save-excursion
      (goto-char r-start)
      (if (search-forward "[`" r-end t)
          (nbm-replace-strings '("[`" "\\(" "`]" "\\)") r-start r-end)
        (nbm-replace-strings '("\\[" "[`" "\\]" "`]"
                               "\\(" "[`" "\\)" "`]"
                               "\\{" "\\lbrace " "\\}" "\\rbrace ")
                             r-start r-end)))))

(defun nbm-replace-strings (change-list &optional START END)
  "Replace a to b and c to d etc if CHANGE-LIST = '(a b c d ...)."
  (unless START (setq START (point-min)))
  (unless END (setq END (point-min)))
  (narrow-to-region START END)
  (let (from-string to-string)
    (while change-list
      (goto-char (point-min))
      (setq from-string (car change-list))
      (setq change-list (cdr change-list))
      (setq to-string (car change-list))
      (setq change-list (cdr change-list))
      (while (search-forward from-string nil t)
        (replace-match to-string nil t))))
  (widen))

(defun nbm-latex-convert-to-hwp ()
  "Toggle latex code to hwp in the selected region. (But not the other way around.)"
  (interactive)
  (save-excursion
    (save-buffer)
    (narrow-to-region (region-beginning) (region-end))
    (beginning-of-buffer)
    (replace-regexp-in-region "\\(_.\\)\\|\\(\\^.\\)" "\\1\\2 ")
    (while (search-forward "\\{" nil t) (replace-match " lbrace "))
    (beginning-of-buffer)
    (while (search-forward "\\}" nil t) (replace-match " rbrace "))
    (beginning-of-buffer)
    (while (search-forward "\\" nil t) (replace-match " "))
    (beginning-of-buffer)
    (while (search-forward " dots" nil t) (replace-match " cdots"))
    (beginning-of-buffer)
    (while (search-forward " frac" nil t)
      (replace-match " ") (forward-sexp) (insert "over"))
    (kill-new (buffer-substring (point-min) (point-max)))
    (revert-buffer t t)
    (message "Hwp math code is copied to the clipboard.")))

;; bibtex

(defun nbm-get-bibtex-entry (property str)
  "Return the entry for PROPERTY in a bibtex item STR.
For example, if PROPERTY is \"author\", then the string of the authors
will be returned."
  (let (beg end item)
    (with-temp-buffer
      (insert str)
      (beginning-of-buffer)
      (search-forward property)
      (search-forward "{") (setq beg (point)) (backward-char)
      (forward-sexp) (backward-char) (setq end (point))
      (buffer-substring beg end))))

(defun nbm-mathscinet-make-filename ()
  "The bibtex file must be copied from mathscinet.
Return the string \"Author1, Author2. Year. Title.pdf\"."
  (let (title authors year temp filename str)
    (setq str (current-kill 0))
    (setq title (nbm-get-bibtex-entry "title" str))
    (setq year (nbm-get-bibtex-entry "year" str))
    (setq authors (split-string (nbm-get-bibtex-entry "author" str) " and "))
    (setq filename (format ". %s. %s.pdf" year title))
    (while authors
      (setq filename (concat (car (split-string (car authors) ",")) filename))
      (setq authors (cdr authors))
      (if (> (length authors) 0) (setq filename (concat ", " filename))))
    filename))


(defun nbm-arxiv-make-filename ()
  "The two lines with title and authors from arxiv homepage must be copied. Return the string \"Author1, Author2. Title.pdf\"."
  (let (title authors temp filename line arxiv)
    (setq temp (split-string (current-kill 0) "\n"))
    (setq arxiv '())
    (dolist (line temp)
      (unless (equal line "")     ; Sometimes the newline \n is copied in between.
	(setq arxiv (nbm-append line arxiv))))
    (setq title (car arxiv))
    (setq authors (split-string (nth 1 arxiv) ","))
    (setq filename "")
    (while authors
      (if (> (length filename) 0)
          (setq filename (concat filename ", ")))
      (setq filename (concat filename
                             (car (last (split-string (car authors) " ")))))
      (setq authors (cdr authors)))
    (setq filename (concat filename ". " title ".pdf"))))

(defun nbm-move-pdf-from-downloads ()
  "Move the most recent PDF from the downloads folder to the pdf folder.
Two lines from arxiv or a bibtex code from mathscinet must be copied first."
  (interactive)
  (let (file choice temp file-name mathscinet)
    (setq pdf (nbm-newest-file (directory-files *nbm-downloads* t
						"\\`[^.$#].*\\([.]pdf\\|[.]djvu\\)$")))
    (setq choice (read-char (format "Move %s into the following folder?\n%s\n\ny: yes\nq: quit

(Note: Two lines from arxiv or three lines from mathscinet must be copied first.)" pdf *nbm-pdf*)))
    (when (equal choice ?y)
      (setq temp (current-kill 0))
      (if (equal (substring temp 0 1) "@") (setq mathscinet t))
      (setq temp (split-string temp "\n"))
      (setq file-name (read-string "Enter a suitable file name: "
				   (if mathscinet (nbm-mathscinet-make-filename)
				     (nbm-arxiv-make-filename))))
      (setq choice (read-char (format "Move \"%s\"\ninto \"%s\"\nunder the following name?\n%s\n\n(Type y for yes)."
				      pdf *nbm-pdf* file-name)))
      (when (equal choice ?y)
	(rename-file pdf (concat *nbm-pdf* file-name) 1)
	(message "File moved!"))
      (if (equal choice ?q) (message "Aborted.")))))

(defun nbm-latex-Cref ()
  "Reftex with Cref."
  (interactive)
  (let ((reftex-refstyle "\\Cref"))
       (reftex-reference " ")))

(defun nbm-latex-eqref ()
  "Reftex with eqref."
  (interactive)
  (let ((reftex-refstyle "\\eqref"))
       (reftex-reference "e")))

(defun nbm-latex-fig-ref ()
  "Reftex with figure."
  (interactive)
  (let ((reftex-refstyle "\\Cref"))
       (reftex-reference "f")))

(defun nbm-latex-sec-ref ()
  "Reftex with section."
  (interactive)
  (let ((reftex-refstyle "\\Cref"))
       (reftex-reference "s")))

(defun nbm-latex-section ()
  "Reftex with section."
  (interactive)
  (LaTeX-section 2))

(defun nbm-latex-toggle-star ()
  "Toggle the current environment between ENV with ENV*.
Delete or insert a label accordingly."
  (interactive)
  (let (env)
    (setq env (LaTeX-current-environment))
    (if (s-suffix? "*" env)
	(progn
	  (setq env (substring env 0 -1))
	  (nbm-latex-insert-label))
      (progn
	(setq env (concat env "*"))
	(nbm-latex-delete-label)))
    (LaTeX-modify-environment env)))


;; latex fonts

(defun nbm-latex-font-emph ()
  (interactive)
  (TeX-font nil 5))

(defun nbm-latex-font-bold ()
  (interactive)
  (TeX-font nil 2))

(defun nbm-latex-font-roman ()
  (interactive)
  (TeX-font nil 18))

(defun nbm-latex-font-mathbb ()
  (interactive)
  (TeX-font nil 19))

(defun nbm-latex-font-truetype ()
  (interactive)
  (TeX-font nil 20))

(defun nbm-latex-font-sf ()
  (interactive)
  (TeX-font nil 6))

(defun nbm-latex-font-sc ()
  (interactive)
  (TeX-font nil 3))

(defun nbm-latex-font-mathcal ()
  (interactive)
  (TeX-font nil 1))

(defun nbm-latex-font-mathfrak ()
  (interactive)
  (TeX-font nil 11))

(defun nbm-latex-font-delete ()
  (interactive)
  (TeX-font nil 4))
