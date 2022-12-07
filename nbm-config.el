(require 'evil)
(evil-mode 1)
(dolist (file (directory-files (concat (getenv "HOME") "/nbm-root/nbm-system") t "[.]el$"))
  (load-file file))
(setq bookmark-default-file (concat *nbm-home* "nbm-user-settings/references/bookmark.el"))

(nbm-key-tree-load)
(nbm-key-tree-appear-in-which-key)
(load-file (nbm-f "nbm-user-settings/nbm-which-key.el"))

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)

(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-insert-state-map (kbd "C-v") 'scroll-up-command)
(define-key evil-insert-state-map (kbd "M-v") 'scroll-down-command)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-y") 'yank)
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-o") 'open-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)

(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-motion-state-map (kbd "RET") nil)

(setq evil-undo-system 'undo-redo)

(evil-define-key '(normal visual motion) 'global (kbd "SPC") 'nbm-key-tree-global)
(evil-define-key '(normal visual motion) 'global (kbd ",") 'nbm-key-tree-mode)
(evil-define-key '(normal visual motion insert) 'global (kbd "M-RET") 'nbm-key-tree-mode)
(evil-define-key 'motion help-mode-map (kbd "TAB") 'forward-button)

(global-evil-surround-mode 1)
(define-key evil-visual-state-map (kbd "s") 'evil-surround-region)

(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/plugins/evil-org-mode"))
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(evil-define-key 'insert 'evil-org-mode (kbd "C-d") 'delete-char)
(evil-define-key 'insert 'evil-org-mode (kbd "RET") 'org-return-and-maybe-indent)

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(evil-owl-mode)
(setq evil-owl-display-method 'posframe
      evil-owl-extra-posframe-args '(:width 50 :height 30
                                            :internal-border-width 1
                                            :internal-border-color "Darkolivegreen1")
      evil-owl-max-string-length 50)
(setq evil-owl-local-mark-format " %m: %s")
(setq evil-owl-global-mark-format " %m: %s")
;; (setq evil-owl-local-mark-format " %m: [l: %-5l, c: %-5c]\n    %s")
;; (setq evil-owl-global-mark-format " %m: [l: %-5l, c: %-5c] %b\n    %s")
(setq evil-owl-idle-delay 0.5)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'apple)
(setq x-select-enable-clipboard t)
(setq delete-by-moving-to-trash t)
(setq trash-directory (nbm-f "trash-bin/"))
(unless (file-exists-p trash-directory)
  (make-directory trash-directory))

(setq default-input-method "korean-hangul")
(global-visual-line-mode)
(global-hl-line-mode)

(setq help-window-select t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq large-file-warning-threshold nil)

(setq max-mini-window-height 0.8)

(setq find-file-visit-truename t)
(save-place-mode)
(setq max-mini-window-height nil)

(recentf-mode 1)
(setq recentf-max-menu-items 55)
(setq recentf-max-saved-items 55)

(setq which-key-prefix-prefix nil)

(load-theme 'spacemacs-dark t)

(tool-bar-mode -1)
(setq scroll-step 1) ;; keyboard scroll one line at a time

(set-face-attribute 'default nil :height 150)
(setq ring-bell-function 'ignore)
(setq resize-mini-windows t)
(setq show-trailing-whitespace t)
(set-cursor-color "LightGreen")

(setq inhibit-startup-screen t)

;; (require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode)
(spaceline-info-mode)
(setq winum-auto-setup-mode-line nil)

(setq mode-line-position (list "(%l,%c)"))
(setq display-time-mode t)
(setq size-indication-mode t)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(spaceline-toggle-input-method-on)
(spaceline-toggle-version-control-off)
;; (spaceline-toggle-window-number-off)
(spaceline-toggle-buffer-encoding-abbrev-off)
(setq spaceline-org-clock-p t)

(server-start)                        ; This is needed for Skim to inverse search
(setq TeX-source-correlate-mode 1)
(setq reftex-default-bibliography (concat *nbm-home* "nbm-user-settings/references/ref.bib"))

(setq TeX-save-query nil)
(setq TeX-auto-save t)
(setq TeX-electric-math (cons "\\( " " \\)"))
;; (setq TeX-insert-braces nil)
(setq reftex-plug-into-AUCTeX t)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LatexMk")))

(setq LaTeX-math-list
      '((?, "dots" nil nil )
        (?. "cdots" nil nil )
        (?5 "widetilde" nil nil )	; for no good reason (or 5 is next to 6)
        (?6 "widehat" nil nil )	; 6 with shift is ^
        (?9 "qquad" nil nil )		; 9 is pronouced Q in Japanese
        (?- "overline" nil nil )
        (?3 "comment" nil nil )
        (?= "equiv" nil nil )
        (? "" nil nil )
        ))

(setq TeX-view-program-list
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o")))
(setq TeX-view-program-selection '((output-pdf "Skim")))
;; The following is helpful for recognizing ^ and _ in latex code.
(custom-set-faces
 '(font-latex-script-char-face ((t (:foreground "Systemorangecolor"))))
 '(font-latex-sectioning-2-face ((t (:foreground "Systemyellowcolor"))))
 )

(setq font-latex-user-keyword-classes
      '(
        ("mathcomment" ("comment") custom-changed noarg)
        ("mathnoarg1" ("frac" "binom") success noarg)
        ("mathnoarg2" ("left" "right" ) custom-variable-tag noarg)
        ("mathnoarg3" ("le" "ge") epa-mark noarg)
        ("Greek" ("Gamma" "Delta" "Theta" "Lambda" "Phi" "Psi" "Omega")
         package-status-unsigned noarg)
        ("greek" ("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta"
                  "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron"
                  "pi" "rho" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")
         imenu-list-entry-face-0 noarg)
        ("mathnoarg6" ("sum" "prod") message-header-subject noarg)
        ("mathnoarg7" ("delta") message-mml noarg)
        ("mathnoarg8" ("epsilon") message-header-other noarg)
        ("mathnoarg9" ("kappa") org-level-4 noarg)
        ))

(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)
;; To make latexmk use pdflatex we need ~/.latexmkrc file.
(unless (file-exists-p (concat (getenv "HOME") "/.latexmkrc"))
  (find-file (concat (getenv "HOME") "/.latexmkrc"))
  (insert "$pdf_mode = 1;")
  (save-buffer) (kill-buffer))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords '((type "TODO" "WAIT" "|" "DONE")))
(setq org-todo-keyword-faces '(("WAIT" . "gray")))
(setq org-log-done 'time)

(setq org-return-follows-link t)
(evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
(evil-define-key 'insert org-mode-map (kbd "C-d") 'delete-char)

(evil-define-key 'motion org-agenda-mode-map
  (kbd "h") 'org-agenda-earlier
  (kbd "l") 'org-agenda-later
  (kbd "v") 'org-agenda-view-mode-dispatch
  (kbd "s") 'org-save-all-org-buffers)

(setq org-directory (concat *nbm-home* "org/"))
(setq org-default-notes-file (concat org-directory "/capture.org"))
(setq org-agenda-span 1)
(setq org-log-into-drawer t)
(setq org-startup-with-inline-images nil)
(setq org-duration-format (quote h:mm))
(setq org-startup-indented t)
(setq org-ref-default-bibliography(concat *nbm-home* "nbm-user-settings/references/ref.bib")
      bibtex-completion-bibliography (concat *nbm-home* "nbm-user-settings/references/ref.bib"))

(setq org-roam-directory (concat *nbm-home* "org/"))
(setq org-roam-graph-viewer "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
(org-roam-db-autosync-mode)
(setq org-roam-capture-templates '(("d" "default" plain "%?"
                                    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                       "#+title: ${title}\n "
                                                       ;; "#+title: ${title}\n#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup\n "
                                                       )
                                    :unnarrowed t)))

(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (mapcar '(lambda (file)
             (when (file-exists-p file)
               (push file org-agenda-files)))
          (org-projectile-todo-files)))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-set-key (kbd "M-x") 'helm-M-x)
                                        ;(setq helm-echo-input-in-header-line t)
(setq helm-autoresize-mode t)
(setq helm-display-header-line nil)
(helm-mode 1)
(add-hook 'helm-minibuffer-set-up-hook 'helm-exchange-minibuffer-and-header-line)

(with-eval-after-load 'helm-bibtex
  (require 'bibtex-completion)
  (message "helm-bibtex executed")

  ;; changing the default action of helm-bibtex
  (helm-add-action-to-source "Insert BibTeX key" 'helm-bibtex-insert-citation helm-source-bibtex 0)
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  (setq bibtex-completion-display-formats (quote ((t . "${year:4} ${author:26} ${title:**} ")))))

;; (add-hook 'dired-mode-hook 'evil-evilified-state)
(with-eval-after-load 'dired
  (require 'dired-x)
  ;; Set dired-x global variables here.  For example:
  ;; (setq dired-guess-shell-gnutar "gtar")
  )
(setq dired-omit-files "\\`[.]")
(setq dired-omit-files (concat dired-omit-files "\\|[.]db\\'"))
(setq dired-omit-files (concat dired-omit-files "\\|[.]ini\\'"))
(setq dired-omit-files (concat dired-omit-files "\\|[.]dbx-passwords\\'"))
(setq dired-omit-files (concat dired-omit-files "\\|\\`Icon
(define-key dired-mode-map (kbd "SPC") 'nbm-key-tree-global)
(define-key dired-mode-map (kbd ",") 'nbm-key-tree-mode)
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (dired-omit-mode 1) ; this causes the annoying "omitting..." in minibuffer.
            ;; (dired-hide-details-mode)
            (setq dired-omit-verbose nil) ; this prevents showing "omitting..."
            ))
(setq dired-dwim-target t)

(eval-after-load "grep" '(grep-compute-defaults))

(setq ispell-program-name "/usr/local/bin/ispell")
(setq ispell-personal-dictionary (concat *nbm-home* "nbm-user-settings/references/my-dictionary"))

; To install magit do the following.
                                        ; M-x package-refresh-contents RET
                                        ; M-x package-install RET magit RET
(when (package-installed-p 'magit)
  (setq transient-values
        '((magit-log:magit-log-mode "-n256" "--graph" "--color" "--decorate")))
  (add-hook 'magit-mode-hook
            (lambda ()
              (local-set-key (kbd "j") #'next-line)
              (local-set-key (kbd "k") #'previous-line)
              (local-set-key (kbd "x") #'magit-discard)
              ))
  )

(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "open" (file))
                              ("\\.hwp\\'" "open" (file))
                              ("\\.xlsx\\'" "open" (file))
                              ("\\.djvu\\'" "/Applications/DjView.app/Contents/bin/djview" (file))))

(require 'yasnippet)
(setq yas/root-directory (list (concat *nbm-home* "nbm-user-settings/snippets/")))
(yas-global-mode 1)
(yas-reload-all)
(setq yas/triggers-in-field t); Enable nested triggering of snippets

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)

(require 'anzu)
(global-anzu-mode +1)
(setq anzu-cons-mode-line-p nil)
(custom-set-faces
 `(lazy-highlight ((t (:foreground "Systemyellowcolor" :background "Violetred1")))))

(add-to-list 'load-path "/path/to/install/folder/emacs-winum/")
(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-`") 'winum-select-window-by-number)
        (define-key map (kbd "C-²") 'winum-select-window-by-number)
        (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
        (define-key map (kbd "M-1") 'winum-select-window-1)
        (define-key map (kbd "M-2") 'winum-select-window-2)
        (define-key map (kbd "M-3") 'winum-select-window-3)
        (define-key map (kbd "M-4") 'winum-select-window-4)
        (define-key map (kbd "M-5") 'winum-select-window-5)
        (define-key map (kbd "M-6") 'winum-select-window-6)
        (define-key map (kbd "M-7") 'winum-select-window-7)
        (define-key map (kbd "M-8") 'winum-select-window-8)
        map))
(require 'winum)
(winum-mode)

(setq avy-background t)
(setq avy-keys (number-sequence ?a ?z))
(setq avy-all-windows nil)

(which-key-mode)

(beacon-mode 1)

(require 'smartparens-config)
(smartparens-global-mode)

;; To use sage in emacs, set up these lines correctly.
;; (add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))
;; (setq sage-shell:use-prompt-toolkit nil)
;; (setq sage-shell:sage-root "/Users/your-user-name/sage")
;; (setq sage-shell:ask-command-options nil)
;; (setenv "PATH" (concat "/Users/your-user-name/sage:" (getenv "PATH")))

(global-company-mode)

;; (evil-set-undo-system 'undo-tree)
;; (global-undo-tree-mode 1)

(add-hook 'org-mode-hook #'valign-mode)

(projectile-mode)

(require 'diminish)
(with-eval-after-load 'evil-owl (diminish 'evil-owl-mode))
(with-eval-after-load 'valign (diminish 'valign-mode))
(with-eval-after-load 'org-indent (diminish 'org-indent-mode))
(with-eval-after-load 'projectile (diminish 'projectile-mode))
(with-eval-after-load 'company (diminish 'company-mode))
(with-eval-after-load 'beacon (diminish 'beacon-mode))
(with-eval-after-load 'which-key (diminish 'which-key-mode))
(with-eval-after-load 'anzu (diminish 'anzu-mode))
(with-eval-after-load 'yasnippet (diminish 'yas-minor-mode))
(with-eval-after-load 'helm (diminish 'helm-mode))
(with-eval-after-load 'reftex (diminish 'reftex-mode))
(with-eval-after-load 'evil-org (diminish 'evil-org-mode))
(with-eval-after-load 'autorevert (diminish 'auto-revert-mode))
(with-eval-after-load 'smartparens (diminish 'smartparens-mode))
(with-eval-after-load 'eldoc (diminish 'eldoc-mode))
(diminish 'visual-line-mode)

(nbm-magnet-move-frame ?l)
(newbie)