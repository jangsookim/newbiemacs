(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; packages here

(unless (package-installed-p 'password-generator) (package-install 'password-generator))
(unless (package-installed-p 'posframe) (package-install 'posframe))
(unless (package-installed-p 'evil-owl) (package-install 'evil-owl))
(unless (package-installed-p 'diminish) (package-install 'diminish))
(unless (package-installed-p 'undo-tree) (package-install 'undo-tree))
(unless (package-installed-p 'company) (package-install 'company))
(unless (package-installed-p 'spaceline) (package-install 'spaceline))
(unless (package-installed-p 'org-bullets) (package-install 'org-bullets))
(unless (package-installed-p 'auctex-latexmk) (package-install 'auctex-latexmk))
(unless (package-installed-p 'evil-surround) (package-install 'evil-surround))
(unless (package-installed-p 'winum) (package-install 'winum))
(unless (package-installed-p 'evil-nerd-commenter) (package-install 'evil-nerd-commenter))
(unless (package-installed-p 'anzu) (package-install 'anzu))
(unless (package-installed-p 'helm-bibtex) (package-install 'helm-bibtex))
(unless (package-installed-p 'rainbow-delimiters) (package-install 'rainbow-delimiters))
(unless (package-installed-p 'smartparens) (package-install 'smartparens))
(unless (package-installed-p 'evil-org) (package-install 'evil-org))
(unless (package-installed-p 'yasnippet) (package-install 'yasnippet))
(unless (package-installed-p 'valign) (package-install 'valign))
(unless (package-installed-p 'beacon) (package-install 'beacon))
(unless (package-installed-p 'which-key) (package-install 'which-key))
(unless (package-installed-p 'avy) (package-install 'avy))
(unless (package-installed-p 'org-projectile-helm) (package-install 'org-projectile-helm))
(unless (package-installed-p 'helm-org-rifle) (package-install 'helm-org-rifle))
(unless (package-installed-p 'helm-org) (package-install 'helm-org))
(unless (package-installed-p 'org-projectile) (package-install 'org-projectile))
(unless (package-installed-p 'auctex) (package-install 'auctex))
(unless (package-installed-p 'openwith) (package-install 'openwith))
(unless (package-installed-p 'evil) (package-install 'evil))
(unless (package-installed-p 'helm) (package-install 'helm))
(unless (package-installed-p 'helm-projectile) (package-install 'helm-projectile))
(unless (package-installed-p 'spacemacs-theme) (package-install 'spacemacs-theme))

;; On Windows these packages are optional.
(unless (equal system-type 'windows-nt)
  (unless (package-installed-p 'org-roam-ui) (package-install 'org-roam-ui))
  (unless (package-installed-p 'org-roam) (package-install 'org-roam))
  )

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
    (insert (concat (getenv "HOME") "/Donwloads" ))
    (save-buffer) (kill-buffer)
    )
  )

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

(org-babel-load-file (concat (getenv "HOME") "/nbm-root/nbm-config.org"))

;; Read the user init file.
(load-file (concat *nbm-home* "nbm-user-settings/user-init.el"))
