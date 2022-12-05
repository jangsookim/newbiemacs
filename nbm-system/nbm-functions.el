;; This file contains simple functions for newbiemacs.

(defun nbm-key-tree-org-mode ()
  (interactive)
  (nbm-key-tree-mode "org-mode"))

(defun nbm-find-function ()
  (interactive)
  (find-function (read-command "Find function: ")))

(defun nbm-magit-init ()
  (interactive)
  (magit-init (file-name-directory (nbm-get-file-name))))

(defun nbm-latex-environment ()
  (interactive)
  (LaTeX-environment nil))

(defun nbm-latex-environment-update ()
  (interactive)
  (LaTeX-environment t))

(defun nbm-tex-build ()
  (interactive)
  (TeX-command-run-all nil))

(defun nbm-M-x ()
  (interactive)
  (helm-M-x nil))

(defun nbm-find-files ()
  (interactive)
  (helm-find-files nil))

(defun nbm-org-time-stamp ()
  (interactive)
  (org-time-stamp nil))

(defun nbm-org-roam-today ()
  "Open org-roam-today file and move the point at the end."
  (interactive)
  (org-roam-dailies-goto-today)
  (goto-char (point-max)))

(defun nbm-get-file-name ()
  (if (equal major-mode 'dired-mode)
      (dired-get-filename)
    (buffer-file-name)))

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

(defun nbm-open-key-tree ()
  (interactive)
  (find-file (nbm-root-f "nbm-sys-key-tree.org")))

(defun nbm-org-deadline ()
  (interactive)
  (org-deadline nil))

(defun nbm-org-schedule ()
  (interactive)
  (org-schedule nil))

(defun nbm-toggle-valign ()
  "Toggle valign mode."
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
