;; This file contains simple functions for newbiemacs.

(defun nbm-latex-environment-update ()
  (interactive)
  (LaTeX-environment t))

(defun nbm-org-roam-today ()
  "Open org-roam-today file and move the point at the end."
  (interactive)
  (org-roam-dailies-goto-today)
  (goto-char (point-max)))

(defun nbm-get-file-name ()
  (if (equal major-mode 'dired-mode)
      (dired-get-filename)
    (buffer-file-name)))

(defun nbm-get-dir-name ()
  (cond ((equal major-mode 'dired-mode)
	 (dired-current-directory))
	((not buffer-file-name)
	 (file-name-directory *newbie-current-file*))
	(t (file-name-directory (buffer-file-name)))))

(defun nbm-copy-file-name ()
  (interactive)
  (kill-new (file-name-nondirectory (nbm-get-file-name)))
  (message (file-name-nondirectory (nbm-get-file-name))))

(defun nbm-copy-file-path ()
  (interactive)
  (kill-new (nbm-get-file-name))
  (message (nbm-get-file-name)))

(defun nbm-copy-directory-path ()
  (interactive)
  (kill-new (file-name-directory (nbm-get-file-name)))
  (message (file-name-directory (nbm-get-file-name))))

(defun nbm-other-window ()
  (interactive)
  (other-window 1))

(defun nbm-comment-or-uncomment-line ()
  (interactive)
  (evilnc-comment-or-uncomment-lines 1))

(defun nbm-find-sys-key-tree ()
  (interactive)
  (find-file (nbm-root-f "nbm-sys-key-tree.org")))

(defun nbm-find-user-key-tree ()
  (interactive)
  (find-file (nbm-f "nbm-user-settings/user-key-tree.org")))

(defun nbm-toggle-valign ()
  "Toggle valign mode."
  (interactive)
  (if valign-mode (valign-mode -1) (valign-mode)))

(defun nbm-set-counter ()
  "Save a number to a register."
  (interactive)
  (let (num)
    (setq num (string-to-number (read-string "Enter a number for counter (default 0): ")))
    (kmacro-set-counter num)))

(defun nbm-add-counter ()
  "Save a number to a register."
  (interactive)
  (let (num)
    (setq num (string-to-number (read-string "Enter a number to add to counter (default 1): \n" nil nil "1")))
    (kmacro-add-counter num)))

(defun nbm-insert-counter ()
  "Save a number to a register."
  (interactive)
    (kmacro-insert-counter 1))

(defun nbm-org-capture ()
  (interactive)
  (org-capture nil "t")
  (evil-append nil))

(defun nbm-shell-commend ()
  (interactive)
  (with-output-to-temp-buffer
      (shell-command (read-string "Shell command: "))))

(defun nbm-update-newbiemacs ()
  "Update Newbiemacs."
  (interactive)
  (let (buf)
    (find-file (nbm-root-f ""))
    (shell-command "git pull")
    (setq buf (current-buffer))
    (kill-buffer buf)))

(defun nbm-cheat-sheet ()
  "Open Newbiemacs cheat sheet."
  (interactive)
  (if (equal (nbm-get-user-variable "editing-style") "windows")
      (browse-url "https://jangsookim.github.io/newbiemacs/newbiemacs_cheat_sheet_windows.pdf")
    (browse-url "https://jangsookim.github.io/newbiemacs/newbiemacs_cheat_sheet_vim.pdf")))

(defun nbm-change-editing-style ()
  "Change the editing style to emacs or vim."
  (interactive)
  (let (choice prompt)
    (if (nbm-get-user-variable "editing-style")
	(setq prompt (format "The current editing style is \"%s\".\n"
			     (nbm-get-user-variable "editing-style")))
      (setq prompt ""))
    (setq prompt (concat prompt "Choose a new editing style:
v) vim
e) emacs
w) windows"))
    (setq choice (read-char prompt))
    (cond ((equal ?e choice)
	   (nbm-set-user-variable "editing-style" "emacs"))
	  ((equal ?w choice)
	   (nbm-set-user-variable "editing-style" "windows"))
	  ((equal ?v choice)
	   (nbm-set-user-variable "editing-style" "vim")))
    (message "The editing style is set to be \"%s\"." (nbm-get-user-variable "editing-style"))))
