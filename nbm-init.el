(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; If ~/nbm-root/nbm-home.txt does not exist,
;; create a new newbiemacs folder under the home folder.
;; The newbiemacs folder may be moved to a different place later.
(unless (file-exists-p (concat (getenv "HOME") "/nbm-root/nbm-home.txt"))
  (find-file (concat (getenv "HOME") "/nbm-root/nbm-home.txt"))
  (insert (concat (getenv "HOME") "/newbiemacs/"))
  (save-buffer) (kill-buffer)
  (unless (file-exists-p (concat (getenv "HOME") "/newbiemacs/"))
    (copy-directory (concat (getenv "HOME") "/nbm-root/newbiemacs/")
		    (concat (getenv "HOME") "/"))
    (make-directory (concat (getenv "HOME") "/newbiemacs/tex"))
    (make-directory (concat (getenv "HOME") "/newbiemacs/tex/symlinks"))
    (make-directory (concat (getenv "HOME") "/newbiemacs/pdf"))
    (make-directory (concat (getenv "HOME") "/newbiemacs/el"))
    (make-directory (concat (getenv "HOME") "/newbiemacs/sage"))
    (make-directory (concat (getenv "HOME") "/newbiemacs/misc"))
    (make-directory (concat (getenv "HOME") "/newbiemacs/misc/symlinks"))
    (find-file (concat (getenv "HOME") "/newbiemacs/tex"))
    (with-temp-buffer (shell-command "git init" t))
    (find-file (concat (getenv "HOME") "/newbiemacs/pdf"))
    (with-temp-buffer (shell-command "git init" t))
    (find-file (concat (getenv "HOME") "/newbiemacs/el"))
    (with-temp-buffer (shell-command "git init" t))
    (find-file (concat (getenv "HOME") "/newbiemacs/sage"))
    (with-temp-buffer (shell-command "git init" t))
    (find-file (concat (getenv "HOME") "/newbiemacs/misc"))
    (with-temp-buffer (shell-command "git init" t))

    (find-file (concat (getenv "HOME") "/newbiemacs/nbm-user-settings/nbm-variables/nbm-desktop.txt"))
    (insert (concat (getenv "HOME") "/Desktop"))
    (save-buffer) (kill-buffer)

    (find-file (concat (getenv "HOME") "/newbiemacs/nbm-user-settings/nbm-variables/nbm-downloads.txt"))
    (insert (concat (getenv "HOME") "/Downloads"))
    (save-buffer) (kill-buffer)

    (find-file (concat (getenv "HOME") "/newbiemacs/nbm-user-settings/nbm-variables/nbm-screenshots.txt"))
    (insert (concat (getenv "HOME") "/Desktop\n" ))
    (insert (concat (getenv "HOME") "/Downloads" ))
    (save-buffer) (kill-buffer)))

;; On Windows create a file for latex with Sumatra PDF
(when (equal system-type 'windows-nt)
  (unless (file-exists-p (concat (getenv "HOME") "/nbm-root/nbm-windows-config.el"))
    (find-file (concat (getenv "HOME") "/nbm-root/nbm-windows-config.el"))
    (insert "(setq TeX-view-program-list '((\"Sumatra PDF\" (\"\\\"")
    (insert (getenv "HOME"))
    (insert "/AppData/Local/SumatraPDF/SumatraPDF.exe\\\" -reuse-instance\" ")
    (insert "(mode-io-correlate \" -forward-search \\\"%b\\\" %n \") \" %o\"))))\n")
    (insert "(setq TeX-view-program-selection '((output-pdf \"Sumatra PDF\")))")
    (save-buffer) (kill-buffer)))

(defvar *nbm-home*)	; defvar only assigns the inital value and will not be updated
(defvar *nbm-pdf*)
(defvar *nbm-desktop*)
(defvar *nbm-downloads*)
(defvar *nbm-screenshots*)
(defvar *newbie-current-file*)

(setq *nbm-home* (with-temp-buffer (insert-file-contents (concat (getenv "HOME") "/nbm-root/nbm-home.txt"))
					 (beginning-of-buffer) (end-of-line)
					 (buffer-substring (point-min) (point))))

(setq *nbm-pdf* (concat *nbm-home* "pdf/"))

(setq *nbm-desktop*
  (with-temp-buffer
    (insert-file-contents (concat *nbm-home* "nbm-user-settings/nbm-variables/nbm-desktop.txt"))
    (beginning-of-buffer) (end-of-line)
    (buffer-substring (point-min) (point))))

(setq *nbm-downloads*
  (with-temp-buffer
    (insert-file-contents (concat *nbm-home* "nbm-user-settings/nbm-variables/nbm-downloads.txt"))
    (beginning-of-buffer) (end-of-line)
    (buffer-substring (point-min) (point))))

(setq *nbm-screenshots*
  (let (temp line dirs)
    (setq temp (split-string
		(with-temp-buffer
		  (insert-file-contents (concat *nbm-home* "nbm-user-settings/nbm-variables/nbm-screenshots.txt"))
		  (buffer-string)) "\n"))
    (dolist (line temp dirs)
      (if (> (length line) 0) (setq dirs (cons line dirs))))))

(setq *newbie-current-file* *nbm-home*)

(defvar *nbm-magnet-height-adjust* 0)
(when (file-exists-p (concat *nbm-home* "nbm-user-settings/nbm-variables/nbm-magnet.txt"))
  (setq *nbm-magnet-height-adjust*
	(with-temp-buffer
	  (insert-file-contents (concat *nbm-home* "nbm-user-settings/nbm-variables/nbm-magnet.txt"))
	  (beginning-of-buffer) (end-of-line)
	  (string-to-number (buffer-substring (point-min) (point))))))

(defvar *nbm-startup-frame* nil)
(when (file-exists-p (concat *nbm-home* "nbm-user-settings/nbm-variables/nbm-startup-frame.txt"))
  (setq *nbm-startup-frame*
	(with-temp-buffer
	  (insert-file-contents (concat *nbm-home* "nbm-user-settings/nbm-variables/nbm-startup-frame.txt"))
	  (beginning-of-buffer) (end-of-line)
	  (mapcar #'string-to-number
		  (split-string (buffer-substring (point-min) (point)))))))

;; Read the system config file.
(org-babel-load-file (concat (getenv "HOME") "/nbm-root/nbm-config.org"))

;; Read the user init file.
(load-file (concat *nbm-home* "nbm-user-settings/user-init.el"))
