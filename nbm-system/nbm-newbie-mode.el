;; This is newbie-mode.

(defun newbie-key ()
  (interactive)
  (if (buffer-file-name)
      (setq *newbie-current-file* (buffer-file-name))
    (setq *newbie-current-file* *nbm-home*))
  (if (equal major-mode 'dired-mode)
      (setq *newbie-current-file* (dired-current-directory)))
  (if (get-buffer "newbie") (kill-buffer "newbie"))
  (split-window-below)
  (other-window -1)
  (switch-to-buffer "newbie")
  (erase-buffer)
  (newbie-mode)
  (read-only-mode)
  (newbie-init))

(defun newbie ()
  (interactive)
  (if (buffer-file-name)
      (setq *newbie-current-file* (buffer-file-name))
    (setq *newbie-current-file* *nbm-home*))
  (if (equal major-mode 'dired-mode)
      (setq *newbie-current-file* (dired-current-directory)))
  (if (get-buffer "newbie") (kill-buffer "newbie"))
  (switch-to-buffer "newbie")
  (erase-buffer)
  (newbie-mode)
  (read-only-mode)
  (newbie-init)
  (evil-local-set-key 'normal (kbd "?") 'nbm-newbiemacs-help)
  (evil-local-set-key 'normal (kbd "a") 'nbm-arxiv-add-pdf-bibtex)
  ;; (evil-local-set-key 'normal (kbd "b") ')
  ;; (evil-local-set-key 'normal (kbd "c") ')
  ;; (evil-local-set-key 'normal (kbd "d") ')
  (evil-local-set-key 'normal (kbd "e") 'nbm-change-editing-style)
  ;; (evil-local-set-key 'normal (kbd "f") 'newbie-latex-insert-figure)
  (evil-local-set-key 'normal (kbd "g") 'newbie-games)
  ;; (evil-local-set-key 'normal (kbd "h") 'newbie-latex-convert-to-hwp)
  ;; (evil-local-set-key 'normal (kbd "i") 'newbie-org-roam-node-insert)
  ;; (evil-local-set-key 'normal (kbd "j") ')
  ;; (evil-local-set-key 'normal (kbd "k") ')
  ;; (evil-local-set-key 'normal (kbd "l") ')
  (evil-local-set-key 'normal (kbd "m") 'nbm-move-pdf-from-downloads)
  (evil-local-set-key 'normal (kbd "n") 'nbm-latex-new-file)
  ;; (evil-local-set-key 'normal (kbd "o") ')
  (evil-local-set-key 'normal (kbd "p") 'nbm-find-pdf)
  (evil-local-set-key 'normal (kbd "q") 'newbie-quit)
  ;; (evil-local-set-key 'normal (kbd "r") 'org-roam-node-find)
  (evil-local-set-key 'normal (kbd "s") 'newbie-search)
  (evil-local-set-key 'normal (kbd "t") 'nbm-find-tex)
  ;; (evil-local-set-key 'normal (kbd "u") 'org-roam-ui-mode)
  ;; (evil-local-set-key 'normal (kbd "v") 'newbie-latex-change-variables)
  ;; (evil-local-set-key 'normal (kbd "w") ')
  (evil-local-set-key 'normal (kbd "x") 'newbie-setting)
  ;; (evil-local-set-key 'normal (kbd "y") 'newbie-yank-file-name)
  ;; (evil-local-set-key 'normal (kbd "z") ')
  ;; (evil-local-set-key 'normal (kbd "A") ')
  ;; (evil-local-set-key 'normal (kbd "B") ')
  ;; (evil-local-set-key 'normal (kbd "C") ')
  ;; (evil-local-set-key 'normal (kbd "D") ')
  ;; (evil-local-set-key 'normal (kbd "E") ')
  (evil-local-set-key 'normal (kbd "F") 'newbie-finder)
  (evil-local-set-key 'normal (kbd "G") 'nbm-org-gtd)
  ;; (evil-local-set-key 'normal (kbd "H") ')
  ;; (evil-local-set-key 'normal (kbd "I") ')
  ;; (evil-local-set-key 'normal (kbd "J") ')
  ;; (evil-local-set-key 'normal (kbd "K") ')
  ;; (evil-local-set-key 'normal (kbd "L") ')
  ;; (evil-local-set-key 'normal (kbd "M") ')
  ;; (evil-local-set-key 'normal (kbd "N") ')
  ;; (evil-local-set-key 'normal (kbd "O") ')
  ;; (evil-local-set-key 'normal (kbd "P") ')
  ;; (evil-local-set-key 'normal (kbd "Q") ')
  (evil-local-set-key 'normal (kbd "l") 'nbm-set-user-level)
  ;; (evil-local-set-key 'normal (kbd "S") 'newbie-add-to-symlinks)
  ;; (evil-local-set-key 'normal (kbd "T") 'newbie-toggle-dotemacs)
  (evil-local-set-key 'normal (kbd "U") 'newbie-update)
  ;; (evil-local-set-key 'normal (kbd "V") ')
  ;; (evil-local-set-key 'normal (kbd "W") ')
  ;; (evil-local-set-key 'normal (kbd "X") ')
  ;; (evil-local-set-key 'normal (kbd "Y") 'newbie-yas-new-snippet)
  ;; (evil-local-set-key 'normal (kbd "Z") ')
  (evil-force-normal-state))

(defun newbie-mode ()
  (setq major-mode 'newbie-mode)
  (font-lock-mode)
  (setq mode-name "Newbie")
  (use-local-map newbie-mode-map))

(defun newbie-init ()
  "Start newbie."
  (newbie-print-all))

(defun nbm-insert (color string)
  "Insert STRING with foreground color COLOR."
  (cond
   ((equal color 1) (insert (propertize string 'font-lock-face '(:foreground "#98989d"))))
   ((equal color 2) (insert (propertize string 'font-lock-face '(:foreground "#Ff9f0a"))))
   ((equal color 3) (insert (propertize string 'font-lock-face '(:foreground "#Ffd60a"))))
   ((equal color 4) (insert (propertize string 'font-lock-face '(:foreground "#Bf5af2"))))
   ((equal color 6) (insert (propertize string 'font-lock-face '(:foreground "#32d74b"))))
   ((equal color 7) (insert (propertize string 'font-lock-face '(:foreground "#Ff453a"))))
   ((equal color 8) (insert (propertize string 'font-lock-face '(:foreground "#0a84ff"))))
   ((equal color 9) (insert (propertize string 'font-lock-face '(:foreground "#Ac8e68"))))
   ((equal color 5) (insert (propertize string 'font-lock-face '(:foreground "#Ff375f"))))))

(defun newbie-print-version ()
  "Start newbie."
    (nbm-insert 1 (format "%83s" "Newbiemacs 1.76")))

(defun newbie-print-logo ()
  "Start newbie."
  (nbm-insert 6 "
  +-+    +-+ +----; +-+    +-+ +----,  +-+ +----; +-+    +-+   ,--,   +----; +----;
  |  \\   | | | +--' | |    | | | ,_, \\ | | | +--' |  \\  /  |  / __ \\  | +--' | +--'
  | . \\  | | | |    | |    | | | | | | | | | |    | . \\/ . | / /  \\ \\ | |    | |
  | |\\ \\ | | | +--; | | /\\ | | | '-' / | | | +--; | |\\  /| | | |__| | | |    | +--+
  | | \\ \\| | | +--' | |/  \\| | | ,-, \\ | | | +--' | | \\/ | | | ,__, | | |    +--+ |
  | |  \\ ' | | |    | ' /\\ ' | | | | | | | | |    | |    | | | |  | | | |       | |
  | |   \\  | | +--; |  /  \\  | | '-' / | | | +--; | |    | | | |  | | | +--; ;--+ |
  +-+    +-+ +----' +-+    +-+ +----'  +-+ +----' +-+    +-+ +-+  +-+ +----' '----+
")
  (nbm-insert 1 "          Newbiemacs is designed for mathematicians who are new to Emacs.          \n\n"))

(setq nbm-meta-key (if (equal system-type 'darwin) "Command" "Alt"))

(defun newbie-print-menu ()
  "Start newbie."
  (nbm-insert 9 (format "%24s%s-Backspace : Newbiemacs screen                         \n" "" nbm-meta-key))
  (nbm-insert 9 (format "%24s%s-o         : Global command                            \n" "" nbm-meta-key))
  (nbm-insert 9 (format "%24s%s-Enter     : Local command                             \n\n" "" nbm-meta-key))
  (nbm-insert 3 (format "%5s%-19s" "" "p: pdf find"))
  (nbm-insert 3 (format "%5s%-19s" "" "t: tex find"))
  (nbm-insert 3 (format "%5s%-33s\n" "" "F: File manager"))
  (nbm-insert 4 (format "%5s%-19s" "" "a: arxiv paper"))
  (nbm-insert 4 (format "%5s%-19s" "" "G: GTD"))
  (nbm-insert 4 (format "%5s%-33s\n" "" "s: search"))
  (nbm-insert 6 (format "%5s%-19s" "" "m: move pdf"))
  (nbm-insert 6 (format "%5s%-19s" "" "n: new tex file"))
  (nbm-insert 6 (format "%5s%-33s\n" "" "g: games"))
  (nbm-insert 7 (format "%5s%-19s" "" "?: Help"))
  (nbm-insert 7 (format "%5s%-19s" "" "x: settings"))
  (nbm-insert 7 (format "%5s%-33s\n" "" "U: Update Newbiemacs"))
  (nbm-insert 2 (format "%5s%-19s" "" "l: Set User Level"))
  (nbm-insert 2 (format "%5s%-19s" "" "e: Set Editing Style"))
  (nbm-insert 2 (format "%4s%-33s\n" "" "q: quit"))
  )

(defun newbie-print-all ()
  "Start newbie."
  (let ((inhibit-read-only t))
    (setq tab-width 30)
    (newbie-print-version)
    (newbie-print-logo)
    (insert "\t")
    (insert-image (create-image (nbm-root-f "nbm-logo.jpeg") nil nil :width 180))
    (insert "\n\n")
    (newbie-print-menu)
    ;; (newbie-print-variables)
    (newbie-print-current-file)
    (beginning-of-buffer)))

(defun newbie-quit ()
  (interactive)
  (kill-buffer)
  (delete-window))

(defun newbie-print-variables ()
  (nbm-insert 9 (format "  %-17s: %s\n" "*nbm-home*" *nbm-home*))
  (nbm-insert 9 (format "  %-17s: %s\n" "*nbm-pdf*" *nbm-pdf*))
  (nbm-insert 9 (format "  %-17s: %s\n" "*nbm-downloads*" *nbm-downloads*))
  (nbm-insert 9 (format "  %-17s: %s\n" "*nbm-desktop*" *nbm-desktop*))
  (nbm-insert 9 (format "  %-17s: %s\n" "*nbm-screenshots*" *nbm-screenshots*)))

(defun newbie-print-current-file ()
  (nbm-insert 1 (format "\n  The current file is:\n  %s" *newbie-current-file*)))

;; key bindings
(defvar newbie-mode-map (make-sparse-keymap))
(evil-global-set-key 'normal (kbd "M-<backspace>") 'newbie)
(evil-global-set-key 'emacs (kbd "M-<backspace>") 'newbie)

(defun newbie-finder ()
  "Open the current file in Finder."
  (interactive)
  (nbm-show-in-finder *newbie-current-file*))

(defun newbie-add-to-symlinks ()
  (interactive) (kill-buffer)
  (nbm-add-to-symlinks))

(defun newbie-latex-convert-to-hwp ()
  (interactive) (kill-buffer)
  (nbm-latex-convert-to-hwp))

(defun newbie-latex-insert-figure ()
  (interactive) (kill-buffer)
  (nbm-latex-insert-figure))

(defun newbie-latex-change-variables ()
  (interactive) (kill-buffer)
  (nbm-latex-change-variables))

(defun newbie-yank-file-name ()
  (interactive)
  (kill-new *newbie-current-file*)
  (message (concat "The following string is copied to the clipboard.\n"
                   *newbie-current-file*)))

(defun newbie-org-roam-node-insert ()
  (interactive) (kill-buffer)
  (org-roam-node-insert))

(defun newbie-setting ()
  (interactive)
  (with-output-to-temp-buffer "newbie-setting"
    (let ((inhibit-read-only t) choice editor path buf)
      (switch-to-buffer "newbie-setting")
      (setq buf (current-buffer))
      (insert "\n  Current variables are shown below.\n\n")
      (newbie-print-variables)
      (insert "\n  To change a variable ")
      (nbm-insert 9 "*VAR*")
      (insert ", edit the content of the file \"VAR.txt\".")
      (insert "\n\n  ")
      (nbm-insert 9 "*nbm-home*")
      (insert " and ")
      (nbm-insert 9 "*nbm-pdf*")
      (nbm-insert 5 " cannot be changed.")
      (insert "\n  Each of ")
      (nbm-insert 9 "*nbm-downloads*")
      (insert " and ")
      (nbm-insert 9 "*nbm-desktop*")
      (insert " must have only one folder path.")
      (nbm-insert 9 "\n\n  *nbm-screenshots*")
      (insert " may have several folder paths, ")
      (nbm-insert 8 "one folder path in one line.")
      (nbm-insert 5 "\n\n  Warning:")
      (insert " Make sure that the last string of a folder path is \"/\".")
      (insert "\n  For example, it should be something like \"~/Desktop/\" rather than \"~/Desktop\".")
      (insert "\n  There must be ")
      (nbm-insert 5 "no space")
      (insert " before or after each folder path.")
      (nbm-insert 2 "\n\n  See the minibuffer at the bottom of this screen.
  You need to reload Newbiemacs if you change a variable.")
      (setq choice (read-char "What do you want to do?
u) Update Newbiemacs
1) Change the variable *nbm-desktop*
2) Change the variable *nbm-downloads*
3) Change the variable *nbm-screenshots*
4) Modify template tex files (The default file is \"template.tex\". You can add any number of tex files here.)
5) Modify the main bib file \"ref.bib\"
6) Modify my favorite strings in \"favorites.txt\"
q) quit"))
      (setq path (concat *nbm-home* "nbm-user-settings/"))
      (cond
       ((equal choice ?1) (setq path (concat path "nbm-variables/nbm-desktop.txt")))
       ((equal choice ?2) (setq path (concat path "nbm-variables/nbm-downloads.txt")))
       ((equal choice ?3) (setq path (concat path "nbm-variables/nbm-screenshots.txt")))
       ((equal choice ?4) (setq path (concat path "templates/template.tex")))
       ((equal choice ?5) (setq path (concat path "references/ref.bib")))
       ((equal choice ?6) (setq path (concat path "references/favorites.txt")))
       ((equal choice ?u) (nbm-update-newbiemacs)))
      (when (member choice '(?1 ?2 ?3 ?4 ?5 ?6))
	(if (equal system-type 'windows-nt) (setq editor "notepad ") (setq editor "open "))
	(shell-command (concat editor (nbm-path-string path))))
      (kill-buffer buf))))

(defun newbie-config ()
  "Open one of the user configuration files."
  (interactive)
  (let (choice)
    (setq choice (read-char "Which file do you want to open?
k) nbm-user-key-tree.org
u) user-init.el"))
    (if (equal choice ?k)
	(find-file (nbm-f "nbm-user-settings/user-key-tree.org")))
    (if (equal choice ?u)
	(find-file (nbm-f "nbm-user-settings/user-init.el")))))

(defun newbie-update ()
  (interactive)
  (nbm-update-newbiemacs)
  (package-refresh-contents)
  (newbie-reload))

(defun newbie-reload ()
  (interactive)
  (load-file (concat (getenv "HOME") "/nbm-root/nbm-init.el")))

(defun newbie-games ()
  (interactive)
  (let (choice)
    (setq choice (read-char "Which game do you want to play?
1) torus
2) tofus
3) tetris"))
    (cond ((equal choice ?1) (torus))
	  ((equal choice ?2) (tofus))
	  ((equal choice ?3) (tetris)))))

(defun newbie-search ()
  (interactive)
  (let (choice)
    (setq choice (read-char "Choose the search page.
a) arxiv
m) mathscinet
z) zbmath"))
    (cond ((equal choice ?a) (nbm-paper-search-arxiv))
	  ((equal choice ?m) (nbm-paper-search-mathscinet))
	  ((equal choice ?z) (nbm-paper-search-zbmath)))))
