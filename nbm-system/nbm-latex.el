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
    (nbm-modify-paper-filename file-name)))

(defun nbm-latex-custom-filename ()
  "Make a custom filename using nbm-latex-make-filename."
  (interactive)
  (read-string (concat "Enter the file name: ") (nbm-latex-make-filename)))

(defun nbm-latex-make-and-yank-filename ()
  "Copy the a string for a filename using title and authors."
  (interactive)
  (let (file-name)
    (setq file-name (nbm-latex-custom-filename))
    (kill-new file-name)
    (message "\"%s\" has been copied." file-name)))

(defun nbm-latex-add-to-symlinks ()
  "Create a symbolic link of the current tex file in the symlinks folder"
  (interactive)
  (let (choice file-name)
    (if (equal major-mode 'dired-mode)
	(setq choice ?f)
      (setq choice
	    (read-char "Choose the symlink file name (default f):\nf) current file name\nt) title of paper\nd) directory name")))
    (cond ((equal choice ?t)
	   (setq file-name (read-string "Enter the symlink file name: " (nbm-latex-make-filename))))
	  ((equal choice ?d)
	   (setq file-name (read-string "Enter the symlink file name: " (nbm-get-lowest-dir-name))))
	  (t
	   (setq file-name (read-string "Enter the symlink file name: "
					(file-name-sans-extension (file-name-nondirectory (nbm-get-file-name)))))))
    (if (equal system-type 'windows-nt)
	(progn
	  (kill-new (format "mklink \"%stex/symlinks/%s.tex\" \"%s\""
			    *nbm-home* file-name (nbm-get-file-name)))
	  (message (format "Command copied in the clipboard. Past it in the command prompt run as administrator.")))
      (progn
	(shell-command (format "ln -s \"%s\" \"%stex/symlinks/%s.tex\""
			       (nbm-get-file-name) *nbm-home* file-name))
	(message (format "A symbolic link created: %s.tex" file-name))))))

(defun nbm-latex-new-file-from-template (dir filename title)
  "Create a new file FILE from a template file under directory DIR.
Write TITLE for the title in the tex file.
DIR must end with /.
FILENAME must end with .tex."
  (let (temp)
    (setq temp (read-file-name "Choose the template file: (default is template.tex) "
			       (nbm-f "nbm-user-settings/templates/")
			       "template.tex"))
    (copy-file temp (concat dir filename))
    (find-file (concat dir filename)) (goto-char (point-min))
    (when (search-forward "\\title{" nil t nil)
      (insert title))
    (save-buffer)))

(defun nbm-latex-new-file ()
  "Create a new latex file from a template."
  (interactive)
  (let (dirname choice title filename temp)
    (when (equal ?n (read-char (format "Create a tex file in the Newbiemacs tex directory?
y: yes (default)
n: no (create a tex file in the current directory)

Current dir: %s" (nbm-get-dir-name))))
      (setq dirname "./"))
    (setq title (read-string (concat "Enter a new latex filename (default: note): ")
			     nil nil "note" nil))
    (unless dirname
      (setq dirname (concat (nbm-f "tex/") (format-time-string "[%Y_%m_%d]_")
			    (string-replace " " "_" title) "/"))
      (make-directory dirname))
    (setq filename (concat (string-replace " " "_" title) ".tex"))
    (nbm-latex-new-file-from-template dirname filename title)
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
      (insert (concat "\n\\newcommand\\" macro-name "{" macro-body "}"))
      (if no-flag
	  (message "The following line has been added in the tex file.
\n%s\n\nDo NOT delete or modify this line." flag)))))

(defun nbm-latex-change-variables (&optional auto)
  "Replace variable x_1,x_2,...,x_k to y_1,y_2,...,y_k in a math mode in current buffer.
If AUTO is non-nil, replace without user confirmation."
  (interactive)
  (let (x y prompt)
    (setq x (read-string "Write the variables to change from. If there are more than one variable write them separated by commas. For example, x,y,z
Variables to change from: " nil nil nil))
    (unless (equal x "")
      (setq y (read-string "Write the variables to change to. If there are more than one variable write them separated by commas. For example, x,y,z
Variables to change to: " nil nil nil))
      (setq x (split-string (string-replace " " "" x) ",")
	    y (split-string (string-replace " " "" y) ","))
      (setq prompt "Do you want to change variables as follows? (type y for yes)\n")
      (dotimes (i (length x))
	(setq prompt (concat prompt (format "%s -> %s\n" (nth i x) (nth i y)))))
      (when (or auto (equal ?y (read-char (substring prompt 0 -1))))
	(nbm-latex-replace-x-y x y auto)))))

(defun nbm-latex-replace-x-y (x y &optional auto)
  "Replace X by Y in the current buffer or the selected region.
X and Y are lists of variables. Each X_i will be replace by Y_i.
If AUTO is non-nil, replace without user confirmation."
  (save-excursion
    (let ((case-fold-search nil) reg-exp i temp beg end done choice replace-all custom quit)
      (when auto (setq replace-all t))
      (if (use-region-p)
	  (setq beg (region-beginning) end (region-end))
	(setq beg (point-min) end (point-max)))
      (goto-char beg)
      (setq reg-exp "")
      (dotimes (i (length x))
	(setq reg-exp (concat reg-exp
			      (format "\\|%s"
				      (string-replace "\\" "\\\\" (nth i x))))))
      (setq reg-exp (substring reg-exp 2 nil))
      (while (and (re-search-forward reg-exp nil t) (< (point) end) (not quit))
	(setq temp (match-string 0))
	(when (nbm-latex-is-variable temp)
	  (setq i (-elem-index temp x))
	  (unless replace-all
	    (setq choice (read-char (format "Do you want to replace this %s by %s?
(Type y for yes,
 type c for a customized change,
 type ! to replace all for the rest, and
 type q to quit.)" (nth i x) (nth i y))))
	    (if (equal choice ?!) (setq replace-all t)))
	  (cond ((or replace-all (equal ?y choice))
		 (delete-region (- (point) (length temp)) (point))
		 (insert (nth i y))
		 (setq end (+ end (length (nth i y)) (- (length (nth i x)))))
		 (when (and (looking-at "[a-zA-Z]")
			    (equal (substring (nth i y) 0 1) "\\"))
		   (insert " ")
		   (setq end (1+ end))))
		((equal ?c choice)
		 (setq custom (read-string "Enter a new variable to be inserted: "))
		 (delete-region (- (point) (length temp)) (point))
		 (insert custom))
		((equal ?q choice)
		 (setq quit t))))))))

(defun nbm-latex-is-variable (var)
  "Return t if VAR is in math mode and not part of a macro or comment."
  (save-excursion
    (let ((is-var t))
      (when (or (not (texmathp)) (TeX-in-commented-line))
	(setq is-var nil))
      (unless (equal (substring var 0 1) "\\")
	(goto-char (- (point) (length var)))
	(re-search-backward "[^a-zA-Z]")
	(when (or (equal (buffer-substring (point) (1+ (point))) "\\")
		  (equal (buffer-substring (- (point) 6) (1+ (point))) "\\begin{")
		  (equal (buffer-substring (- (point) 4) (1+ (point))) "\\end{"))
	  (setq is-var nil)))
      is-var)))

(defun nbm-latex-find-math-mode (include-env)
  "Return (type beg end).
type is \"\\[\", \"\\(\",equation, etc, or nil.
beg and end are the starting and ending points of the environment.
If INCLUDE-ENV is non-nil, then the region from beg and end
includes the environment macro."
  (save-excursion
    (cond ((or (equal (buffer-substring (point) (+ (point) 2)) "\\(")
	       (equal (buffer-substring (1- (point)) (1+ (point))) "\\(")
	       (equal (buffer-substring (point) (+ (point) 2)) "\\[")
	       (equal (buffer-substring (1- (point)) (1+ (point))) "\\["))
	   (forward-char 2))
	  ((equal (TeX-current-macro) "begin")
	   (search-forward "}")))
    (let (type end end)
      (when (texmathp)
	(setq type (car texmathp-why)
	      beg (cdr texmathp-why))
	(goto-char (1+ beg))
	(cond ((equal type "\\[")
	       (search-forward "\\]"))
	      ((equal type "\\(")
	       (search-forward "\\)"))
	      (t (LaTeX-find-matching-end)))
	(setq end (point))
	(unless include-env
	  (if (member type '("\\(" "\\["))
	      (setq beg (+ beg 2) end (- end 2))
	    (progn
	      (goto-char beg) (search-forward "}") (setq beg (point))
	      (goto-char end) (search-backward "\\") (setq end (point))))
	  (if (member (buffer-substring beg (1+ beg)) '("\n" " "))
	      (setq beg (1+ beg)))
	  (if (member (buffer-substring (1- end) end) '("\n" " "))
	      (setq end (1- end))))
	(list type beg end)))))

(defun nbm-latex-exit-math-mode (&optional front)
  "Go to the end of the current math mode.
If FRONT is non-nil, exit to the front of the math mode."
  (interactive)
  (let ((math (nbm-latex-find-math-mode t)))
    (if (car math)
	(if front
	    (goto-char (nth 1 math))
	  (goto-char (nth 2 math)))
      (message "You are not in math mode!"))))

(defun nbm-latex-exit-math-mode-front ()
  (interactive)
  (nbm-latex-exit-math-mode t))

(defun nbm-latex-copy-math-with-env ()
  "Copy the content in the current math mode including the environment macro."
  (interactive)
  (let ((math (nbm-latex-find-math-mode t)))
    (if (car math)
	(progn
	  (copy-region-as-kill (nth 1 math) (nth 2 math))
	  (message "Copied the math content with the environment macro."))
      (message "You are not in math mode!"))))

(defun nbm-latex-delete-math-with-env ()
  "Delete the content in the current math mode including the environment macro."
  (interactive)
  (let ((math (nbm-latex-find-math-mode t)))
    (if (car math)
	(progn
	  (kill-region (nth 1 math) (nth 2 math))
	  (message "Deleted the math content with the environment macro."))
      (message "You are not in math mode!"))))

(defun nbm-latex-delete-math ()
  "Delete the content in the current math mode."
  (interactive)
  (let ((math (nbm-latex-find-math-mode nil)))
    (if (car math)
	(progn
	  (kill-region (nth 1 math) (nth 2 math))
	  (message "Deleted the math content."))
      (message "You are not in math mode!"))))

(defun nbm-latex-copy-math ()
  "Copy the content in the current math mode."
  (interactive)
  (let ((math (nbm-latex-find-math-mode nil)) str)
    (if (car math)
	(progn
	  (setq str (buffer-substring (nth 1 math) (nth 2 math)))
	  (setq str (replace-regexp-in-string "\\\\label{[^}]*}" "" str))
	  (setq str (replace-regexp-in-string "\n *\n" "\n" str))
	  (setq str (replace-regexp-in-string "^\n" "" str))
	  (kill-new str)
	  (message "Copied the math content."))
      (message "You are not in math mode!"))))

(defun nbm-latex-modify-math ()
  "Modify the content in the current math mode."
  (interactive)
  (let ((math (nbm-latex-find-math-mode t)) old new)
    (save-excursion
      (unless (car math)
	(backward-char)
	(setq math (nbm-latex-find-math-mode t)))
      (if (car math)
	  (progn
	    (set-mark (nth 1 math))
	    (goto-char (nth 2 math))
	    (nbm-latex-change-variables t)
	    (deactivate-mark))
	(message "You are not in math mode!")))))

(defun nbm-latex-paste-previous-math ()
  "Paste the content of the previous math mode.
If the cursor is not in math mode, include the math environment."
  (interactive)
  (let (found currently-math)
    (save-excursion
      (when (texmathp) (setq currently-math t))
      (while (and (not found) (re-search-backward "\\\\\\|\\$" nil t))
	(when (texmathp)
	  (if currently-math
	      (nbm-latex-copy-math)
	    (nbm-latex-copy-math-with-env))
	  (setq found t))))
    (if found
	(progn
	  (insert (current-kill 0)) (backward-char 2)
	  (nbm-latex-modify-math) (forward-char 2))
      (message "No math mode before the cursor."))))

(defun nbm-latex-paste-avy-math ()
  "Paste the content of the math mode chosen by avy jump.
The candidates must have length at least 5.
If the cursor is not in math mode, include the math environment."
  (interactive)
  (let (found currently-math math math-list (beg (window-start)) (end (window-end)))
    (save-excursion
      (when (texmathp) (setq currently-math t))
      (goto-char beg)
      (while (re-search-forward (concat (regexp-quote "\\[") "\\|"
					(regexp-quote "\\(") "\\|"
					(regexp-quote "\\end"))
				end t)
	(when (and (texmathp) (not (TeX-in-comment)))
	  (setq math (nbm-latex-find-math-mode nil))
	  (when (> (nth 2 math) (+ 5 (nth 1 math)))
	    (setq math (nbm-latex-find-math-mode t))
	    (setq math (buffer-substring (nth 1 math) (nth 2 math)))
	    (when math-list (setq math-list (concat math-list "\\|")))
	    (setq math-list (concat math-list (regexp-quote math)))))))
    (save-excursion
      (when (avy-jump math-list)
	(setq found t)
	(if currently-math
	    (nbm-latex-copy-math)
	  (nbm-latex-copy-math-with-env))))
    (if found
	(insert (current-kill 0))
      (message "Wrong a math mode."))))

(defun nbm-latex-paste-avy-environment ()
  "Paste the content of the environment chosen by avy jump."
  (interactive)
  (let (found env env-beg env-end (beg (window-start)) (end (window-end)))
    (save-excursion
      (when (avy-jump "\\\\begin")
	(setq found t)
	(setq env-beg (point)) (forward-char)
	(LaTeX-find-matching-end)
	(setq env-end (point))
	(setq env (buffer-substring env-beg env-end ))))
    (insert env)
    (backward-char 2) (nbm-latex-uniquify-labels) (forward-char 2)))

(defun nbm-latex-toggle-inline-math ()
  "Change inline math \"(..)\" to display math \"[..]\" or vice versa."
  (interactive)
  (save-excursion
    (let ((math (nbm-latex-find-math-mode t)) punctuation)
      (cond ((not (car math))
	     (message "You are not inside a math mode!"))
	    ((equal (car math) "\\(")
	     (goto-char (nth 2 math))
	     (re-search-backward "[^ ] *\\\\)")
	     (forward-char)
	     (delete-region (point) (nth 2 math))
	     (when (looking-at "[,.;:!?]") (forward-char))
	     (when (looking-at " *\n") (kill-line))
	     (if (looking-back "^ *")
		 (insert "\\]\n")
	       (insert "\n\\]\n"))
	     (goto-char (nth 1 math))
	     (if (looking-at "\\\\( *\n")
		 (kill-line 1)
	       (delete-region (point) (+ (point) 2)))
	     (if (looking-back "^ *")
		 (insert "\\[\n")
	       (insert "\n\\[\n"))
	     (setq math (nbm-latex-find-math-mode t)))
	    ((equal (car math) "\\[")
	     (goto-char (nth 2 math))
	     (if (looking-back "^ *\\\\]")
		 (progn
		   (previous-line) (end-of-line)
		   (unless (looking-back " ") (insert " "))
		   (kill-line) (zap-to-char 1 ?\]))
	       (delete-region (- (point) 2) (point)))
	     (insert "\\)")
	     (when (looking-back "[,.;:!?] *\\\\)")
	       (re-search-backward "[,.;:!?]")
	       (setq punctuation (buffer-substring (point) (1+ (point))))
	       (delete-region (point) (1+ (point)))
	       (search-forward "\\)")
	       (insert punctuation))
	     (goto-char (nth 1 math))
	     (if (looking-at "\\\\\\[ *\n")
		 (kill-line 1)
	       (delete-region (point) (+ (point) 2)))
	     (insert "\\(")
	     (setq math (nbm-latex-find-math-mode t))))
      (if math
	  (if (equal (car math) "\\(")
	      (progn
		(set-mark (nth 1 math))
		(goto-char (nth 2 math))
		(fill-paragraph t t)
		(deactivate-mark))
	    (indent-region (nth 1 math) (nth 2 math)))
	(message "You are not inside a proper math mode for toggling!")))))

(defun nbm-latex-toggle-display-math ()
  "Change display math \"[..]\" to \\begin{equation}...\\end{equation} or
any math environment to display math."
  (save-excursion
    (let ((math (nbm-latex-find-math-mode t)))
      (cond ((not (car math))
	     (message "You are not inside a math mode!"))
	    ((equal (car math) "\\(")
	     (message "You are not inside a display math mode!"))
	    ((equal (car math) "\\[")
	     (goto-char (nth 2 math))
	     (delete-region (- (point) 2) (point))
	     (insert "\\end{equation}")
	     (goto-char (nth 1 math))
	     (delete-region (point) (+ (point) 2))
	     (insert "\\begin{equation}"))
	    (t
	     (goto-char (nth 2 math))
	     (search-backward "\\")
	     (delete-region (point) (nth 2 math))
	     (insert "\\]")
	     (goto-char (nth 1 math))
	     (search-forward "}")
	     (delete-region (nth 1 math) (point))
	     (insert "\\["))))))

(defun nbm-latex-toggle-equation ()
  "Change \\ [ \\] to \\begin{equation}...\\end{equation} or vice versa.
Delete or add a label accordingly."
  (interactive)
  (save-excursion
    (let ((math (nbm-latex-find-math-mode t)))
      (cond ((not (car math))
	     (message "You are not inside a math mode!"))
	    ((equal (car math) "\\(")
	     (message "You are not inside a display math mode!"))
	    ((equal (car math) "\\[")
	     (nbm-latex-toggle-display-math)
	     (nbm-latex-new-label))
	    (t
	     (nbm-latex-delete-label)
	     (nbm-latex-toggle-display-math))))))

(defun nbm-latex-change-env-name (new-env)
  "Change the environment with NEW-ENV."
  (save-excursion
    (let ((old-env (LaTeX-current-environment))
	  (beg (car (LaTeX-env-beginning-pos-col)))
	  (end (LaTeX-find-matching-end)))
      (goto-char end)
      (search-backward old-env)
      (replace-match new-env)
      (goto-char beg)
      (search-forward old-env)
      (replace-match new-env))))

(defun nbm-latex-toggle-align ()
  "Change \\ [ \\] or \\begin{equation}...\\end{equation}
to \\begin{align}...\\end{align} or vice versa."
  (interactive)
  (let ((math (nbm-latex-find-math-mode t)))
    (when (equal (car math) "\\[")
      (nbm-latex-toggle-display-math)
      (nbm-latex-change-env-name "equation*")
      (setq math (nbm-latex-find-math-mode t)))
    (cond ((not (car math))
	   (message "You are not inside a math mode!"))
	  ((equal (car math) "\\(")
	   (message "You are not inside a display math mode!"))
	  ((member (car math) '("align*" "align"))
	   (save-excursion
	     (goto-char (nth 1 math))
	     (while (re-search-forward "&\\|\\\\\\\\" (nth 2 math) t)
	       (replace-match "")))
	   (if (equal (car math) "align")
	       (nbm-latex-change-env-name "equation")
	     (nbm-latex-toggle-display-math)))
	  (t
	   (if (member (car math) '("equation" "multline"))
	       (nbm-latex-change-env-name "align")
	     (nbm-latex-change-env-name "align*"))))))

(defun nbm-latex-toggle-multline ()
  "Change \\ [ \\] or \\begin{equation}...\\end{equation}
to \\begin{multline}...\\end{multline} or vice versa."
  (interactive)
  (let ((math (nbm-latex-find-math-mode t)))
    (when (equal (car math) "\\[")
      (nbm-latex-toggle-display-math)
      (nbm-latex-change-env-name "equation*")
      (setq math (nbm-latex-find-math-mode t)))
    (cond ((not (car math))
	   (message "You are not inside a math mode!"))
	  ((equal (car math) "\\(")
	   (message "You are not inside a display math mode!"))
	  ((member (car math) '("multline*" "multline"))
	   (save-excursion
	     (goto-char (nth 1 math))
	     (while (re-search-forward "\\\\\\\\" (nth 2 math) t)
	       (replace-match "")))
	   (if (equal (car math) "multline")
	       (nbm-latex-change-env-name "equation")
	     (nbm-latex-toggle-display-math)))
	  ((member (car math) '("align*" "align"))
	   (save-excursion
	     (goto-char (nth 1 math))
	     (while (re-search-forward "&\\|\\\\\\\\" (nth 2 math) t)
	       (replace-match "")))
	   (if (equal (car math) "align")
	       (nbm-latex-change-env-name "multline")
	     (nbm-latex-change-env-name "multline*")))
	  (t
	   (if (member (car math) '("equation"))
	       (nbm-latex-change-env-name "multline")
	     (nbm-latex-change-env-name "multline*"))))))

(defun nbm-latex-toggle-frac ()
  "Toggle between (a)/(b) and \\frac{a}{b} in the selected region or in the current line."
  (interactive)
  (save-excursion
    (let (pos r-beg r-end beg end beg1 end1 beg2 end2 num den frac slash found)
      (if (use-region-p)
	  (setq r-beg (region-beginning) r-end (region-end))
	(save-excursion
	  (beginning-of-line) (setq r-beg (point))
	  (end-of-line) (setq r-end (point))))
      (setq pos (point))
      (save-excursion
	(when (search-forward "/" r-end t)
	  (setq slash (point)) (search-backward ")") (setq end1 (point))
	  (forward-char) (backward-sexp)
	  (setq beg (point)) (setq beg1 (1+ (point)))
	  (when (and (<= beg pos) (<= pos slash))
	    (setq found t) (goto-char slash)
	    (search-forward "(") (setq beg2 (point))
	    (backward-char) (forward-sexp) (setq end2 (1- (point)))
	    (setq end (point)))))
      (unless found
	(save-excursion
	  (when (search-backward "/" r-beg t)
	    (setq slash (point)) (search-forward "(") (setq beg2 (point))
	    (backward-char) (forward-sexp) (setq end2 (1- (point)))
	    (setq end (point))
	    (when (and (<= slash pos) (<= pos end))
	      (setq found t) (goto-char slash)
	      (search-backward ")") (setq end1 (point))
	      (forward-char) (backward-sexp) (setq beg (point))
	      (setq beg1 (1+ (point)))))))
      (unless found
	(if (equal (TeX-current-macro) "frac")
	    (progn
	      (goto-char (TeX-find-macro-start))
	      (setq beg (point)) (setq frac t))
	  (when (search-backward "\\frac" r-beg t)
	    (setq beg (point)) (setq frac t))))
      (when frac
	(search-forward "{") (setq beg1 (point))
	(backward-char) (forward-sexp) (setq end1 (1- (point)))
	(search-forward "{") (setq beg2 (point))
	(backward-char) (forward-sexp) (setq end2 (1- (point)))
	(setq end (point)))
      (if (or found frac)
	  (progn
	    (setq num (buffer-substring beg1 end1)
		  den (buffer-substring beg2 end2))
	    (delete-region beg end)
	    (if found
		(insert (format "\\frac{%s}{%s}" num den))
	      (insert (format "(%s)/(%s)" num den))))
	(message "Not in a fraction environment!")))))

(defun nbm-latex-toggle-parenthesis ()
  "Toggle between (..) and \\left(..\\right)."
  (interactive)
  (save-excursion
    (let (beg end choice brace)
      (save-excursion
	(when (looking-at "[(]\\|[[]\\|[{]")
	  (when (looking-at "{") (setq brace t) (backward-char))
	  (setq beg (point)))
	(when (looking-at ")\\|]\\|\\}")
	  (when (looking-at "}") (setq brace t) (backward-char))
	  (forward-char)
	  (if brace (nbm-latex-backward-sexp) (backward-sexp))
	  (when brace (backward-char))
	  (setq beg (point))))
      (setq choice (read-char "Choose the macro for the parentheses:
0) left-right (default)
1) big
2) Big
3) bigg
4) Bigg
n) none
d) delete the parentheses"))
      (when beg
	(goto-char beg)
	(when (looking-back "\\\\\\(big\\|Big\\|bigg\\|Bigg\\|left\\) *" (line-beginning-position))
	  (zap-to-char -1 ?\\) (setq beg (point)))
	(if brace (nbm-latex-forward-sexp) (forward-sexp))
	(backward-char)
	(when (looking-back "\\\\\\(big\\|Big\\|bigg\\|Bigg\\|right\\) *" (line-beginning-position))
	  (zap-to-char -1 ?\\))
	(forward-char) (setq end (point))
	(cond ((equal choice ?1)
	       (goto-char (1- end)) (insert "\\big")
	       (goto-char beg) (insert "\\big"))
	      ((equal choice ?2)
	       (goto-char (1- end)) (insert "\\Big")
	       (goto-char beg) (insert "\\Big"))
	      ((equal choice ?3)
	       (goto-char (1- end)) (insert "\\bigg")
	       (goto-char beg) (insert "\\bigg"))
	      ((equal choice ?4)
	       (goto-char (1- end)) (insert "\\Bigg")
	       (goto-char beg) (insert "\\Bigg"))
	      ((equal choice ?n))
	      ((equal choice ?d)
	       (goto-char end) (delete-char -1)
	       (goto-char beg) (delete-char 1))
	      (t
	       (goto-char (1- end)) (insert "\\right")
	       (goto-char beg) (insert "\\left")))))))

(defun nbm-latex-forward-sexp ()
  "Go to the closing parenthesis if the cursor is at an opening parenthesis."
  (interactive)
  (let (level opening closing candidates found)
    (setq candidates (list (buffer-substring (- (point) 1) (+ (point) 1))
			   (buffer-substring (point) (+ (point) 2))))
    (cond ((member "\\(" candidates)
	   (setq opening "\\(" closing "\\)"))
	  ((member "\\[" candidates)
	   (setq opening "\\[" closing "\\]"))
	  ((member "\\{" candidates)
	   (setq opening "\\{" closing "\\}")))
    (when opening
      (setq level 0) (forward-char)
      (while (not found)
	(re-search-forward (format "\\(%s\\|%s\\)" (regexp-quote opening) (regexp-quote closing)))
	(if (equal (match-string 1) opening)
	    (setq level (1+ level))
	  (setq level (1- level)))
	(when (equal level -1)
	  (setq found t) (backward-char))))))

(defun nbm-latex-backward-sexp ()
  "Go to the opening parenthesis if the cursor is at a closing parenthesis."
  (interactive)
  (let (level opening closing candidates found)
    (setq candidates (list (buffer-substring (- (point) 1) (+ (point) 1))
			   (buffer-substring (point) (+ (point) 2))))
    (cond ((member "\\)" candidates)
	   (setq opening "\\(" closing "\\)"))
	  ((member "\\]" candidates)
	   (setq opening "\\[" closing "\\]"))
	  ((member "\\}" candidates)
	   (setq opening "\\{" closing "\\}")))
    (when opening
      (setq level 0)
      (while (not found)
	(re-search-backward (format "\\(%s\\|%s\\)" (regexp-quote opening) (regexp-quote closing)))
	(if (equal (match-string 1) closing)
	    (setq level (1+ level))
	  (setq level (1- level)))
	(when (equal level -1)
	  (setq found t) (forward-char))))))

(defun nbm-latex-evil-jump-item ()
  "This function behaves like evil-jump-item also working with \\(, \\{, \\[.
In normal mode, the cursor can be placed on either \\ or (.
In visual mode, the cursor must be placed on \\."
  (interactive)
  (let (pos)
    (setq pos (point))
    (nbm-latex-forward-sexp)
    (when (equal pos (point))
      (nbm-latex-backward-sexp))
    (when (and (use-region-p) (not (equal pos (point))))
      (forward-char))
    (when (equal pos (point))
      (evil-jump-item))))

(defun nbm-latex-new-label (&optional auto)
  "Add a new label in the current environment.
If AUTO is non-nil, create an automatic label."
  (interactive)
  (save-excursion
    (reftex-reset-mode)
    (reftex-access-scan-info)
    (let ((env (LaTeX-current-environment)) num)
      (cond ((equal env "document")
	     (message "You are not in a proper environment!"))
	    ((member env '("align" "equation" "multline"))
	     (if auto
		 (setq label "")
	       (setq label (read-string "Enter a label below. (If you type XXX, then eq:XXX will be inserted. If you type nothing, a unique numeric label will be inserted.)\n")))
	     (if (equal label "") (setq label (reftex-label nil t))
	       (setq label (concat "eq:" label))))
	    (t
	     (setq env (concat (substring env 0 3) ":"))
	     (if auto
		 (setq label "")
	       (setq label (read-string (format "Enter a label below. (If you type XXX, then %sXXX will be inserted. If you type nothing, a unique numeric label will be inserted.)\n" env))))
	     (if (equal label "")
		 (setq label (reftex-uniquify-label env t))
	       (setq label (concat env label)))))
      (cond ((equal env "align")
	     (insert (format "\\label{%s}" label)))
	    ((equal env "document"))
	    ((equal env "fig:")
	     (LaTeX-find-matching-end)
	     (backward-char 12)
	     (insert (format "\\label{%s}\n" label)))
	    (t
	     (LaTeX-find-matching-begin)
	     (search-forward "\\begin" nil t) (forward-sexp)
	     (when (looking-at "[ ]*\\[") (forward-sexp))
	     (insert (format "\\label{%s}" label)))))))

(defun nbm-latex-delete-label ()
  "Delete the labels in the current environment."
  (save-excursion
    (let (bound beg end (count 0))
      (setq bound (car (LaTeX-env-beginning-pos-col)))
      (LaTeX-find-matching-end)
      (while (search-backward "\\label" bound t)
	(setq count (1+ count))
	(setq beg (point)) (forward-char 6) (forward-sexp)
	(delete-region beg (point)) (delete-blank-lines)))))

(defun nbm-latex-uniquify-labels ()
  "Uniquify the labels in the current environment."
  (unless (equal (LaTeX-current-environment) "document")
    (reftex-reset-mode)
    (reftex-access-scan-info)
    (save-excursion
      (let (beg end label bound)
	(LaTeX-find-matching-begin)
	(setq bound (point)) (forward-char)
	(LaTeX-find-matching-end)
	(while (search-backward "\\label{" bound t)
	  (setq beg (+ (point) 7))
	  (search-forward "}")
	  (setq end (1- (point)))
	  (setq label (buffer-substring beg end))
	  (while (string-match-p "[0-9]$" label)
	    (setq label (substring label 0 -1)))
	  (setq label (reftex-uniquify-label label t))
	  (delete-region beg end)
	  (goto-char beg) (setq beg (- beg 7))
	  (insert label) (goto-char beg))))))

(defun nbm-latex-extract-bib-file ()
  "Create a bib file containing the current bibitems from the main bib file."
  (interactive)
  (save-excursion
    (let (new-bib-str bibitem-list bibitem beg end bbl-file)
      (beginning-of-buffer)
      (while (search-forward "\\bibitem" nil t)
	(setq beg (1+ (point))) (forward-sexp) (setq end (1- (point)))
	(unless (member (buffer-substring beg end) bibitem-list)
	  (push (buffer-substring beg end) bibitem-list)))
      (setq bbl-file (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ".bbl"))
      (when (file-exists-p bbl-file)
	(find-file bbl-file)
	(beginning-of-buffer)
	(while (search-forward "\\bibitem" nil t)
	  (setq beg (1+ (point))) (forward-sexp) (setq end (1- (point)))
	  (unless (member (buffer-substring beg end) bibitem-list)
	    (push (buffer-substring beg end) bibitem-list)))
	(kill-buffer))
      (find-file (nbm-f "nbm-user-settings/references/ref.bib"))
      (setq new-bib-str "")
      (while bibitem-list
	(setq bibitem (pop bibitem-list))
	(beginning-of-buffer)
	(when (search-forward (format "{%s," bibitem) nil t)
	  (beginning-of-line) (setq beg (point))
	  (search-forward "{") (backward-char) (forward-sexp) (setq end (point))
	  (setq new-bib-str (concat new-bib-str (buffer-substring beg end) "\n\n"))))
      (kill-buffer)
      (find-file "local-ref.bib") (erase-buffer) (insert new-bib-str)
      (save-buffer) (kill-buffer)
      (message "Created a bib file with file name: \"local-ref.bib\""))))

(defun nbm-latex-toggle-bbl-file ()
  "Insert the bib file or remove it.
If there is a space in the path, replace it by a dash."
  (interactive)
  (save-excursion
    (let (bib-exist beg end)
      (goto-char (point-max))
      (if (search-backward "\\bibliography{" nil t)
	  (progn
	    (setq beg (point))
	    (search-forward "{") (backward-char) (forward-sexp)
	    (if (equal (char-after) ?\n)
		(setq end (1+ (point)))
	      (setq end (point)))
	    (kill-region beg end)
	    (insert-file (concat (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) ".bbl"))
	    (message "Bibtex toggled: bibtex OFF"))
	(progn
	  (if (search-backward "\\begin{thebibliography}" nil t)
	      (progn
		(setq beg (point))
		(search-forward "\\end{thebibliography}")
		(delete-region beg (point)))
	    (progn
	      (search-backward "\\end{document}" nil t)
	      (insert "\n\n") (previous-line 2)
	      (insert "\\bibliographystyle{abbrv}\n")))
	  (insert (format "\\bibliography{%s}"
			  (string-replace " " "-"
					  (nbm-f "nbm-user-settings/references/ref.bib"))))
	  (message "Bibtex toggled: bibtex ON"))))))

(defun nbm-bib-item-create-key (bib-str)
  "Create a key for the bib item given by BIB-STR. "
  (let (authors year key keys a choice)
    (setq choice (read-char "Choose the key scheme. (Suppose the authors are Hwang, Jang, Kim, Song, and Song and the year is 2023.)
1: Hwang2023 (default)
2: HJKSS2023
3: HwangJangKimSongSong2023
4: HJK+23
5: Customized key"))
    (setq year (nbm-get-bibtex-entry "year" bib-str))
    (setq authors (nbm-get-bibtex-entry "author" bib-str))
    (setq authors (split-string authors " and "))
    (setq key "")
    (when (equal choice ?5)
      (setq key (read-string "Enter bibkey: "))
      (setq year nil))
    (while (and authors (not (equal choice ?5)))
      (setq name (pop authors))
      ;; get the last name depending on whether name is written Jack Sparrow or Sparrow, Jack.
      (if (string-match "," name)
	  (setq name (car (split-string name ",")))
	(setq name (car (last (split-string name " ")))))
      (cond ((equal choice ?2)
	     (setq key (concat key (substring name 0 1))))
	    ((equal choice ?3)
	     (setq key (concat key name)))
	    ((equal choice ?4)
	     (cond ((= (length key) 3)
		    (setq key (concat key "+")))
		   ((> (length key) 3))
		   (t
		    (setq key (concat key (substring name 0 1))))))
	    (t (if (equal (length key) 0) (setq key (concat key name)))))) ; choice 1 is default
    (when year
      (when (equal choice ?4)
	(setq year (substring year 2 4)))
      (setq key (concat key year)))
    (setq keys (nbm-latex-get-bib-key-list))
    ;; Check if the key is already used.
    (when (member key keys)
      (setq a ?a)
      (while (member (format "%s%c" key a) keys) ; Attach a or b ... if the key is already used.
	(setq a (+ a 1)))
      (setq key (format "%s%c" key a)))
    (setq key (nbm-modify-paper-filename key))
    (string-replace " " "" key)))

(defun nbm-latex-new-bib-item ()
  "Create a bib item in the main bib file using citation data from arxiv, MathSciNet, or zbMATH."
  (interactive)
  (save-excursion
    (let (data str beg end done choice)
      (with-output-to-temp-buffer "bib-item-temp-buffer"
	(setq data (current-kill 0))
	(switch-to-buffer "bib-item-temp-buffer")
	(insert data)
	(beginning-of-buffer)
	(when (search-forward "archivePrefix={arXiv}," nil t)
	  (beginning-of-buffer)
	  (search-forward "eprint" nil t) (search-forward "{" nil t)
	  (setq arxiv (buffer-substring (point) (- (search-forward "}") 1)))
	  (beginning-of-buffer)
	  (setq url (concat "https://arxiv.org/abs/" arxiv))
	  (end-of-line)
	  (insert (concat "\n      howpublished = {{\\it Preprint}, \\href{" url "}{arXiv:" arxiv "}},")))
	(beginning-of-buffer)
	;; If title has {...} make it {{...}}.
	(search-forward "title") (search-forward "{")
	(unless (equal (buffer-substring (- (point) 1) (+ (point) 1)) "{{")
	  (insert "{") (search-forward "}") (insert "}"))
	;; If the bibitem contains month, remove it.
	(beginning-of-buffer)
	(when (search-forward "month" nil t)
	  (beginning-of-line) (kill-line 2))

	(while (not done)
	  (setq key (nbm-bib-item-create-key data))
	  ;; Replace the original bib key with the new key.
	  (beginning-of-buffer)
	  (search-forward "{") (setq beg (point))
	  (search-forward ",") (setq end (- (point) 1))
	  (delete-region beg end) (backward-char) (insert key)
	  (setq choice (read-char "Do you want to save this bib item?\ny: yes\nn: no\nother key: retry"))
	  (cond ((equal choice ?y)
		 (setq done t)
		 (setq str (buffer-string))
		 (find-file (nbm-f "nbm-user-settings/references/ref.bib"))
		 (end-of-buffer)
		 (while (not (equal (buffer-substring (- (point-max) 2) (point-max)) "\n\n"))
		   (insert "\n") (save-buffer))
		 (insert str) (save-buffer) (kill-buffer))
		((equal choice ?n)
		 (setq done t)))))
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

(defun nbm-latex-find-duplicated-bib-keys ()
  "Go to the first duplicated bib item if there is any in the main bib file."
  (interactive)
  (let (duplicated)
    (setq duplicated (nbm-find-duplicated-items (nbm-latex-get-bib-key-list)))
    (if duplicated
	(progn
	  (find-file (nbm-f "nbm-user-settings/references/ref.bib"))
	  (beginning-of-buffer)
	  (search-forward (car duplicated)))
      (message "No duplicated items."))))

(defun nbm-latex-insert-figure (env &optional quick)
  "Insert the most recent file from *nbm-screenshots* to ./figures.
If ENV is non-nil, insert a figure environment.
If QUICK is non-nil, use the default options."
  (let (fig files ext file choice dir)
    (setq files '())
    (dolist (dir *nbm-screenshots*)
      (if (file-exists-p dir)
	  (setq files (append files (directory-files dir t "[.]jpeg\\|[.]png\\|[.]jpg")))))
    (setq newest (nbm-newest-file files))
    (setq ext (file-name-extension newest))
    (setq choice
	  (if quick ?y
	    (read-char (concat "Move this file?: (Type y for yes.)\n" newest))))
    (when (equal choice ?y)
      (unless (file-directory-p "./figures/") (make-directory "./figures/"))
      (setq fig (file-name-nondirectory
		 (file-name-sans-extension (nbm-make-unique-filename "./figures/" "image" ext))))
      (unless quick
	(setq fig (read-string "Enter the figure name: " fig)))
      (if (file-exists-p (format "./figures/%s.%s" fig ext))
	  (message (format "./figures/%s.%s already exists!" fig ext))
	(copy-file newest (format "./figures/%s.%s" fig ext)))
      (setq choice
	    (if quick ?n		; Do not delete for the quick option.
	      (read-char (concat "Delete this file?: (Type y for yes.)\n" newest))))
      (when (eq choice ?y) (delete-file newest))
      (end-of-line)
      (if env
	  (progn
	    (insert (concat " See Figure~\\ref{fig:" fig "}.\n"
			    "\n\\begin{figure}\n"
			    "  \\centering\n"
			    "  \\includegraphics[scale=.25]{./figures/" fig "." ext "}\n"
			    "  \\caption{}\n"
			    "  \\label{fig:" fig "}\n"
			    "\\end{figure}\n"))
	    (search-backward "\\caption{") (search-forward "{"))
	(insert (concat "\\begin{center}\n"
			"  \\includegraphics[scale=.25]{./figures/" fig "." ext "}\n"
			"\\end{center}"))))))

(defun nbm-latex-insert-figure-with-env ()
  "Insert the most recent file from *nbm-screenshots* to ./figures with a figure environment."
  (interactive)
  (nbm-latex-insert-figure t))

(defun nbm-latex-insert-figure-quick ()
  "Insert the most recent file from *nbm-screenshots* to ./figures quickly with default options."
  (interactive)
  (nbm-latex-insert-figure nil t))

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
      (setq beg (point)) (end-of-line) (setq end (point))
      (goto-char beg)
      (if (search-forward "{" end t)
	  (progn
	    (setq beg (point)) (backward-char)
	    (forward-sexp) (backward-char) (setq end (point)))
	(progn
	  (goto-char beg)
	  (re-search-forward "= *") (setq beg (point)) (backward-char)
	  (when (search-forward "," end t) (setq end (- (point) 1)))))
      (string-clean-whitespace (buffer-substring beg end)))))

(defun nbm-mathscinet-make-filename ()
  "The bibtex file must be copied from mathscinet.
Return the string \"Author1, Author2. Year. Title.pdf\"."
  (let (title authors year temp filename str)
    (setq str (current-kill 0))
    (setq title (nbm-modify-paper-filename
		 (nbm-get-bibtex-entry "title" str)))
    (setq year (nbm-get-bibtex-entry "year" str))
    (setq authors (split-string (nbm-get-bibtex-entry "author" str) " and "))
    (setq authors (reverse authors))
    (setq filename (format ". %s. %s.pdf" year title))
    (while authors
      (setq filename (concat (car (split-string (car authors) ",")) filename))
      (setq authors (cdr authors))
      (if (> (length authors) 0) (setq filename (concat ", " filename))))
    (nbm-modify-paper-filename filename)))

(defun nbm-modify-paper-filename (title)
  "Modify the string TITLE so that it is suitable for a filename."
  (setq title (replace-regexp-in-string "\\\\`\\|\\\\'\\|\\\\^\\|\\\\\"\\|\\\\H\\|\\\\~\\|\\\\c\\|\\\\k\\|\\\\=\\|\\\\b\\|\\\\[.]\\|\\\\d\\|\\\\r\\|\\\\u\\|\\\\v" "" title))
  (setq title (replace-regexp-in-string "{\\|}\\|\\$\\|\n\\|`\\|''" "" title))
  (setq title (replace-regexp-in-string "\\\\" "" title))
  (setq title (replace-regexp-in-string "\"" "" title))
  (setq title (replace-regexp-in-string ":" "-" title))
  (xah-asciify-string title))

(defun nbm-arxiv-make-filename ()
  "The two lines with title and authors from arxiv homepage must be copied. Return the string \"Author1, Author2. Title.pdf\"."
  (let (title authors temp filename line arxiv)
    (setq temp (split-string (current-kill 0) "\n"))
    (setq arxiv '())
    (dolist (line temp)
      (unless (equal line "")     ; Sometimes the newline \n is copied in between.
	(setq arxiv (nbm-append line arxiv))))
    (setq title (car arxiv))
    (setq authors (replace-regexp-in-string " *([^)]*)" "" (nth 1 arxiv)))
    (setq authors (split-string authors ","))
    (setq filename "")
    (while authors
      (if (> (length filename) 0)
	  (setq filename (concat filename ", ")))
      (setq filename (concat filename
			     (car (last (split-string (car authors) " ")))))
      (setq authors (cdr authors)))
    (setq filename (nbm-modify-paper-filename (concat filename ". " title ".pdf")))))

(defun nbm-move-pdf-from-downloads ()
  "Move the most recent PDF from the downloads folder to the pdf folder.
Two lines from arxiv or a bibtex code from mathscinet must be copied first.
If a string is copied from mathscinet, then ask if the user wants to
add a new bib item."
  (interactive)
  (let (file choice temp file-name mathscinet)
    (setq pdf (nbm-newest-file (directory-files *nbm-downloads* t
						"\\`[^.$#].*\\([.]pdf\\|[.]djvu\\)$")))
    (if (not pdf)
	(message (format "There is no pdf file in %s." *nbm-downloads*)))
    (when pdf
      (setq choice (read-char (format "Move %s into the following folder?\n%s\n\ny: yes\nq: quit

(Note: Two lines from arxiv or a bibtex item from mathscinet must be copied first.)" pdf *nbm-pdf*)))
      (when (equal choice ?y)
	(setq temp (current-kill 0))
	(while (equal (substring temp 0 1) "\n")
	  (setq temp (substring temp 1 nil)))
	(if (equal (substring temp 0 1) "@") (setq mathscinet t))
	(setq temp (split-string temp "\n"))
	(setq file-name (if mathscinet (nbm-mathscinet-make-filename)
			  (nbm-arxiv-make-filename)))
	(setq file-name (string-replace "/" "-" file-name))
	(setq file-name (read-string "Enter a suitable file name: " file-name))
	(setq choice (read-char (format "Move \"%s\"\ninto \"%s\"\nunder the following name?\n%s\n\n(Type y for yes)."
					pdf *nbm-pdf* file-name)))
	(when (equal choice ?y)
	  (rename-file pdf (concat *nbm-pdf* file-name) 1)
	  (message "File moved!")
	  (when (and mathscinet
		     (equal ?y (read-char "Do you want to add a bib item? (Type y for yes.): ")))
	    (nbm-latex-new-bib-item)))
	(if (equal choice ?q) (message "Aborted."))))))

(defun nbm-latex-ref ()
  "Reftex with ref."
  (interactive)
  (let ((reftex-refstyle "\\ref"))
    (reftex-reset-mode)
    (reftex-reference " ")))

(defun nbm-latex-Cref ()
  "Reftex with Cref."
  (interactive)
  (let ((reftex-refstyle "\\Cref"))
    (reftex-reset-mode)
    (reftex-reference " ")))

(defun nbm-latex-eqref ()
  "Reftex with eqref."
  (interactive)
  (let ((reftex-refstyle "\\eqref"))
    (reftex-reset-mode)
    (reftex-reference "e")))

(defun nbm-latex-fig-ref ()
  "Reftex with figure."
  (interactive)
  (let ((reftex-refstyle "\\Cref"))
    (reftex-reset-mode)
    (reftex-reference "f")))

(defun nbm-latex-sec-ref ()
  "Reftex with section."
  (interactive)
  (let ((reftex-refstyle "\\Cref"))
    (reftex-reset-mode)
    (reftex-reference "s")))

(defun nbm-latex-section ()
  "Reftex with section."
  (interactive)
  (LaTeX-section 2))

(defun nbm-latex-toggle-star ()
  "Toggle the current environment between ENV with ENV*.
Delete or insert a label accordingly."
  (interactive)
  (let (env math)
    (save-excursion
      (setq math (nbm-latex-find-math-mode nil))
      (when (and math (not (s-prefix? "\\" (car math))))
	(goto-char (nth 1 math))
	(setq env (LaTeX-current-environment))
	(if (s-suffix? "*" env)
	    (progn
	      (setq env (substring env 0 -1))
	      (LaTeX-modify-environment env)
	      (nbm-latex-new-label))
	  (progn
	    (setq env (concat env "*"))
	    (LaTeX-modify-environment env)
	    (nbm-latex-delete-label))))
      (unless env (message "You are not in a proper math environment.")))))

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

;; latex diff

(defun nbm-latex-diff ()
  "Compare the current file with its previous version."
  (interactive)
  (let (old new)
    (setq old (read-file-name "Choose an older version to compare: "))
    (setq new (file-name-nondirectory (buffer-file-name)))
    (shell-command (format "latexdiff \"%s\" \"%s\" > diff.tex" old new))
    (find-file "diff.tex")))

;; changing reftex toc behavior for tabline

(defun nbm-reftex-toc-quit ()
  "Quit the toc buffer with keeping the tabline buffer list."
  (interactive)
  (other-window 1) (delete-window) (kill-buffer))

(defun nbm-reftex-toc-goto-line-and-hide ()
  "Visit the current line in toc and quit the toc buffer with keeping the tabline buffer list."
  (interactive)
  (let (pos)
    (reftex-toc-goto-line)
    (setq pos (point))
    (other-window 1)
    (nbm-reftex-toc-quit)
    (goto-char pos)))

;; jump sections

(defun nbm-latex-jump-section-mode ()
  "Start jump-mode to a section in the current tex file."
  (interactive)
  (let (key)
    (setq key ?j)
    (while (member key '(?j ?k))
      (setq key (read-char "k) go to the previous section
j) go to the next section
other key) stop"))
      (if (equal key ?j) (outline-next-heading))
      (if (equal key ?k) (outline-previous-heading)))))

(defun nbm-latex-jump-section ()
  "Jump to a section in the current tex file."
  (interactive)
  (let (section-list section beg end)
    (save-excursion
      (beginning-of-buffer)
      (setq section-list '())
      (while (re-search-forward "\\\\section\\|\\\\subsection\\|\\\\subsubsection\\|\\\\chapter\\|\\\\part" nil t)
	(setq end (point))
	(search-backward "\\") (setq beg (point))
	(goto-char end) (forward-sexp) (setq end (point))
	(setq section (buffer-substring beg end))
	(setq section-list (nbm-append section section-list))))
    (setq section (completing-read "Choose a section to jump: "
				   section-list))
    (beginning-of-buffer)
    (search-forward section)))

(defun nbm-reftex-insert-ref ()
  "Insert a reference in the helm style."
  (interactive)
  (reftex-reset-mode)
  (reftex-access-scan-info)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
	 (label (completing-read "Choose a reference to insert: "
				 docstruct
				 (lambda (x) (stringp (car x))) t)))
    (if (equal (substring label 0 3) "eq:")
	(insert "\\eqref")
      (insert "\\Cref"))
    (insert (format "{%s}" label))))

(defun nbm-reftex-goto-label (&optional other-window)
  "Modified from reftex-goto-label so that Cref and eqref work as default.
Prompt for a label (with completion) and jump to the location of this label."
  (interactive "P")
  (reftex-reset-mode)
  (reftex-access-scan-info)
  (let* ((wcfg (current-window-configuration))
	 (docstruct (symbol-value reftex-docstruct-symbol))
	 ;; If point is inside a \ref{} or \pageref{}, use that as
	 ;; default value.
	 (default (when (looking-back "\\\\\\(?:\\(page\\|eq\\|C\\)\\)?ref{[-a-zA-Z0-9_*.:\\=\\+--/^() ]*"
				      (line-beginning-position))
		    (reftex-this-word "-a-zA-Z0-9_*.:\\=\\+--/^() ")))
	 (label (completing-read (format-prompt "Label" default)
				 docstruct
				 (lambda (x) (stringp (car x))) t nil nil
				 default))
	 (selection (assoc label docstruct))
	 (where (progn
		  (reftex-show-label-location selection t nil 'stay)
		  (point-marker))))
    (unless other-window
      (set-window-configuration wcfg)
      (switch-to-buffer (marker-buffer where))
      (goto-char where))
    (reftex-unhighlight 0)))

(defvar *nbm-latex-compile-section* nil)

(defun nbm-latex-compile ()
  "Compile the current tex file. "
  (interactive)
  (let ((TeX-command-force t))
    (save-buffer)
    (if (member (current-buffer) *nbm-latex-compile-section*)
	(LaTeX-command-section)
      (TeX-command-master))))

(defun nbm-latex-toggle-compile-section ()
  "Toggle the membership of the current buffer in the alist *nbm-latex-compile-section*."
  (interactive)
  (let (level buf)
    (setq buf (current-buffer))
    (if (member buf *nbm-latex-compile-section*)
	(progn
	  (setq *nbm-latex-compile-section*
		(remove buf *nbm-latex-compile-section*))
	  (message "Section compile mode is turned off."))
      (progn
	(setq level (read-char "Choose the section level: (default 2)
1) chapter
2) section
3) subsection"))
	(cond ((equal level ?1) (setq level 1))
	      ((equal level ?3) (setq level 3))
	      (t (setq level 2)))
	(setq LaTeX-command-section-level level)
	(add-to-list '*nbm-latex-compile-section* buf)
	(message "Section compile mode is turned on.")))))

(defun nbm-latex-find-main-tex-file ()
  "Find the main tex file associated to the current _region_.tex."
  (save-excursion
    (beginning-of-buffer) (re-search-forward "!name(\\(.+[.]tex\\))"))
  (find-file (match-string 1)))

(defun nbm-latex-switch-between-main-and-region ()
  "Go to the main tex file at the position corresponding to the _region_.tex file or vice versa."
  (interactive)
  (let (offset section beg end level)
    (save-excursion
      (if (equal (file-name-nondirectory (buffer-file-name)) "_region_.tex")
	  (progn
	    (nbm-latex-find-main-tex-file)
	    (setq level LaTeX-command-section-level)
	    (find-file "_region_.tex"))
	(setq level LaTeX-command-section-level))
      (setq offset (point))
      (cond ((equal level 1)
	     (search-backward "\\chapter{"))
	    ((equal level 2)
	     (search-backward "\\section{"))
	    ((equal level 3)
	     (search-backward "\\subsection{")))
      (setq beg (point)) (search-forward "{") (backward-char) (forward-sexp)
      (setq end (point)) (setq offset (- offset (point)))
      (setq section (buffer-substring beg end)))
    (if (equal (file-name-nondirectory (buffer-file-name)) "_region_.tex")
	(nbm-latex-find-main-tex-file)
      (find-file "_region_.tex"))
    (beginning-of-buffer) (search-forward section) (forward-char offset)))

(defun nbm-latex-switch-between-main-and-region-hook ()
  "Switch to the main tex file if _region_.tex file is called from an external pdf viewer."
  (interactive)
  (when (equal (file-name-nondirectory (buffer-file-name)) "_region_.tex")
    (nbm-latex-switch-between-main-and-region)))

;; The following is for the inverse search from _region_.pdf.
(if (equal system-type 'gnu/linux)
    (setq TeX-raise-frame-function
	  (lambda ()
	    (call-process
	     "wmctrl" nil nil nil "-i" "-R"
	     (frame-parameter (selected-frame) 'outer-window-id))
	    (delete-other-windows)
	    (nbm-latex-switch-between-main-and-region-hook)))
  (add-hook 'server-switch-hook 'nbm-latex-switch-between-main-and-region-hook))

(defun nbm-latex-view-pdf ()
  "View the pdf file associated to the current tex file.
If the current buffer is in *nbm-latex-compile-section*, then open the pdf associated to _region_tex."
  (interactive)
  (let ((buf (current-buffer)))
    (if (member buf *nbm-latex-compile-section*)
	(progn
	  (nbm-latex-switch-between-main-and-region)
	  (latex-mode) (TeX-command "View" #'TeX-master-file 0)
	  (switch-to-buffer buf))
      (TeX-view))))

(defun nbm-latex-new-study ()
  "Create a new tex file for studying a paper in the pdf folder."
  (interactive)
  (let (dir pdf-name tex-name temp)
    (setq pdf-name (completing-read "Choose a file to study: "
				    (directory-files-recursively (nbm-f "pdf/") ".*[.]pdf$\\|.*[.]djvu$")))
    (setq pdf-name (file-name-nondirectory (file-name-sans-extension pdf-name)))
    (setq tex-name (concat (car (split-string pdf-name "[.]")) ".tex")) ; the author names
    (setq dir (nbm-f "tex/study/"))
    (unless (file-exists-p dir) (make-directory dir))
    (setq dir (concat dir pdf-name "/"))
    (unless (file-exists-p dir) (make-directory dir))
    (nbm-latex-new-file-from-template dir tex-name pdf-name)
    (message "Create a tex file.")))

(defun nbm-latex-start-study ()
  "Choose a pdf file associated to one in the study directory.
Open the pdf file and the corresponding tex file."
  (interactive)
  (let (topic tex-file tex-files)
    (setq topic (completing-read "Choose what to study: "
				 (directory-files (nbm-f "tex/study/") nil "^[^.]")))
    (setq tex-files (directory-files (nbm-f (format "tex/study/%s/" topic)) nil "[.]tex$"))
    (setq tex-files (remove "_region_.tex" tex-files))
    (if (equal (length tex-files) 1)
	(setq tex-file (car tex-files))
      (setq tex-file (completing-read "Choose a tex file to open: " tex-files)))
    (find-file (nbm-f (format "tex/study/%s/%s" topic tex-file)))
    (find-file (car (directory-files-recursively (nbm-f "pdf/")
						 (format "%s[.]pdf$\\|%s[.]djvu$"
							 (regexp-quote topic)
							 (regexp-quote topic)))))))

(defun nbm-latex-toggle-solutions ()
  "Comment or uncomment all solutions.
When comment all solutions, copy the pdf with solutions
to another pdf file with \"(solutions)\" added to the file name."
  (interactive)
  (save-excursion
    (let (visible invisible)
      (setq visible 0 invisible 0)
      (beginning-of-buffer)
      (search-forward "\\begin{document}")
      (while (re-search-forward "\\\\begin{proof}[ \t\n]*\\[Solution]" nil t)
	(save-excursion
	  (LaTeX-mark-environment)
	  (beginning-of-line)
	  (comment-or-uncomment-region (region-beginning) (region-end))
	  (deactivate-mark)
	  (if (looking-at "%")
	      (setq invisible (1+ invisible))
	    (setq visible (1+ visible)))))
      (when (and (equal visible 0) (> invisible 0))
	(copy-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")
		   (concat (file-name-sans-extension (buffer-file-name)) "(solutions).pdf") t))
      (message (format "The number of visible solutions is %s.
The number of invisible solutions is %s." visible invisible)))))

(defun nbm-latex-toggle-dollars ()
  "Toggle $ to \\( \\) and $$ to \\[ \\]."
  (interactive)
  (save-excursion
    (let (double single)
      (when (equal (buffer-substring (point) (+ (point) 2)) "$$")
	(setq double t))
      (when (equal (buffer-substring (- (point) 1) (+ (point) 1)) "$$")
	(setq double t) (backward-char))
      (when double
	(delete-region (point) (+ (point) 2))
	(insert "\\[")
	(search-forward "$$")
	(delete-region (- (point) 2) (point))
	(insert "\\]"))
      (unless double
	(when (equal (buffer-substring (point) (+ (point) 1)) "$")
	  (setq single t))
	(delete-region (point) (+ (point) 1))
	(insert "\\(")
	(search-forward "$")
	(delete-region (- (point) 1) (point))
	(insert "\\)")))))

(defun nbm-latex-insert-webpage ()
  "Insert a link to the webpage of the user's browser.
This uses org-mac-link so only works in mac."
  (interactive)
  (when (equal system-type 'darwin)
    (let (browser link)
      (setq browser (nbm-get-user-variable "nbm-browser"))
      (unless browser
	(nbm-set-user-variable "nbm-browser"
			       (completing-read "Select your browser: "
						'("chrome" "safari" "firefox")))
	(setq browser (nbm-get-user-variable "nbm-browser")))
      (cond ((equal browser "chrome")
	     (setq link (org-mac-link-chrome-get-frontmost-url)))
	    ((equal browser "safari")
	     (setq link (org-mac-link-safari-get-frontmost-url)))
	    ((equal browser "firefox")
	     (setq link (org-mac-link-firefox-get-frontmost-url))))
      (insert (substring link 1 -1))
      (backward-sexp) (nbm-replace-parentheses "{}")
      (backward-char) (backward-sexp) (nbm-replace-parentheses "{}")
      (backward-char) (insert "\\href"))))

(defun nbm-replace-parentheses (paren)
  "Replace the current parentheses with PAREN.
PAREN must be a string of the form \"[]\", \"()\", etc.
The cursor must be placed before the opening parenthesis."
  (save-excursion
    (forward-sexp) (delete-char -1)
    (insert (substring paren 1 2)))
  (delete-char 1) (insert (substring paren 0 1)))

(defun nbm-latex-toggle-refcheck ()
  "Comment or uncomment \\usepackage{refcheck}."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let (beg end)
      (if (search-forward "\\usepackage{refcheck}" nil t)
	  (progn
	    (comment-line 1) (setq end (point))
	    (previous-line) (setq beg (point))
	    (if (comment-only-p beg end)
		(message "The refcheck package is disabled.")
	      (message "The refcheck package is enabled.")))
	(message "There is no line containing \"\\usepackage{refcheck}\".")))))

(defun nbm-latex-rename-environment ()
  (interactive)
  (if (equal (LaTeX-current-environment) "document")
      (message "You cannot rename rename the \"document\" environment!")
    (LaTeX-environment t)))

(defun nbm-latex-insert-environment (env &optional label auto)
  "Insert an environment ENV.
If LABEL is non-nil, insert a label.
If AUTO is non-nil, insert a label automatically." 
  (insert (format "\\begin{%s}\n\n\\end{%s}\n" env env))
  (previous-line 2)
  (cond ((equal env "figure")
	 (insert "\\centering\n\n\\caption{}")
	 (previous-line))
	((member env '("enumerate" "itemize"))
	 (insert "\\item "))
	((member env '("align" "align*"))
	 (insert " &=  \\\\\n") (insert " &=  \\\\")
	 (previous-line) (beginning-of-line)))
  (when label (nbm-latex-new-label auto))
  (save-excursion
    (LaTeX-mark-environment)
    (indent-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun nbm-latex-insert-custom-theorem (theorem &optional auto)
  "Insert a theorem environment THEOREM using a macro like
\"\\newtheorem{thm}{Theorem}\" at the beginning of the current tex file.
If AUTO is non-nil, insert a label automatically."
  (let (thm)
    (save-excursion
      (beginning-of-buffer)
      (when (re-search-forward (format "^ *\\\\newtheorem{\\([a-zA-Z]+\\)}\\([[][a-zA-Z]+[]]\\)*{%s}" theorem) nil t)
	(setq thm (match-string 1))))
    (if thm
	(nbm-latex-insert-environment thm t auto)
      (message (format "There is no macro for theorem. Insert a line like
\"\\newtheorem{%s}{%s}\" at the beginning of the current tex file." theorem theorem)))))

(defun nbm-latex-new-environment ()
  "Insert a new environment."
  (interactive)
  (let (prompt choice auto)
    (setq prompt "Select an environment. (Type a capital letter for a custom label.)\n\n")
    (setq prompt (concat prompt (concat (nbm-string-key "t") ": theorem  ")))
    (setq prompt (concat prompt (concat (nbm-string-key "l") ": lemma  ")))
    (setq prompt (concat prompt (concat (nbm-string-key "p") ": proposition  ")))
    (setq prompt (concat prompt (concat (nbm-string-key "c") ": corollary  ")))
    (setq prompt (concat prompt (concat (nbm-string-key "d") ": definition\n\n")))
    (setq prompt (concat prompt (concat (nbm-string-key "i") ": itemize  ")))
    (setq prompt (concat prompt (concat (nbm-string-key "n") ": enumerate\n\n")))
    (setq prompt (concat prompt (concat (nbm-string-key "e") ": equation ")))
    (setq prompt (concat prompt (concat (nbm-string-key "m") ": multline  ")))
    (setq prompt (concat prompt (concat (nbm-string-key "u") ": multline*  ")))
    (setq prompt (concat prompt (concat (nbm-string-key "a") ": align  ")))
    (setq prompt (concat prompt (concat (nbm-string-key "g") ": align*\n\n")))
    (setq prompt (concat prompt (concat (nbm-string-key "f") ": figure   ")))
    (setq prompt (concat prompt (concat (nbm-string-key "z") ": tikzpicture             ")))
    (setq prompt (concat prompt (concat (nbm-string-key "x") ": other environment\n\n")))
    (setq prompt (concat prompt (concat (nbm-string-key "SPC") ": proof\n")))
    (setq prompt (concat prompt (concat (nbm-string-key "RET") ": Rename the current environment")))
    (setq choice (read-char prompt))
    (setq auto t)
    (when (and (>= choice ?A) (<= choice ?Z))
      (setq auto nil)
      (setq choice (+ choice 32)))
    (cond ((equal choice ?t) (nbm-latex-insert-custom-theorem "Theorem" auto))
	  ((equal choice ?l) (nbm-latex-insert-custom-theorem "Lemma" auto))
	  ((equal choice ?p) (nbm-latex-insert-custom-theorem "Proposition" auto))
	  ((equal choice ?c) (nbm-latex-insert-custom-theorem "Corollary" auto))
	  ((equal choice ?d) (nbm-latex-insert-custom-theorem "Definition" auto))
	  ((equal choice ?i) (nbm-latex-insert-environment "itemize"))
	  ((equal choice ?n) (nbm-latex-insert-environment "enumerate"))
	  ((equal choice ?e) (nbm-latex-insert-environment "equation" t auto))
	  ((equal choice ?a) (nbm-latex-insert-environment "align"))
	  ((equal choice ?g) (nbm-latex-insert-environment "align*"))
	  ((equal choice ?m) (nbm-latex-insert-environment "multline" t auto))
	  ((equal choice ?u) (nbm-latex-insert-environment "multline*"))
	  ((equal choice ?f) (nbm-latex-insert-environment "figure" t auto))
	  ((equal choice ?z) (nbm-latex-insert-environment "tikzpicture"))
	  ((equal choice ?\^M) (nbm-latex-rename-environment))
	  ((equal choice ? ) (nbm-latex-insert-environment "proof"))
	  ((equal choice ?x) (LaTeX-environment nil)))))

