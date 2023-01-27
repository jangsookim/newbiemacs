;; This file contains simple functions for newbiemacs.

(defun nbm-key-tree-org-mode ()
  (interactive)
  (nbm-key-tree-mode "org-mode"))

(defun nbm-magit-init ()
  (interactive)
  (magit-init (file-name-directory (nbm-get-file-name))))

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
  (if (equal major-mode 'dired-mode)
      (dired-current-directory)
    (file-name-directory (buffer-file-name))))

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

(defun nbm-toggle-projectile-caching ()
  "Toggle projectile-enable-caching."
  (interactive)
  (if projectile-enable-caching
      (setq projectile-enable-caching nil)
    (setq projectile-enable-caching t))
  (message (format "Now projectile-enable-caching is %s." projectile-enable-caching)))

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
  (org-capture nil "t"))

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

(defun nbm-open-file (file)
  "Open FILE using system's default program."
  (interactive)
  (cond ((equal system-type 'windows-nt)
	 (shell-command (format "start %s" file)))
	((equal system-type 'darwin)
	 (shell-command (format "open \"%s\"" file)))
	((equal system-type 'gnu/linux)
	 (shell-command (format "xdg-open \"%s\"" file)))))

(defun nbm-cheat-sheet ()
  "Open Newbiemacs cheat sheet."
  (interactive)
  (nbm-open-file (nbm-root-f "newbiemacs_cheat_sheet.pdf")))
