(defun nbm-key-tree-org-mode ()
  (interactive)
  (nbm-key-tree-mode "org-mode"))

(defun nbm-find-function ()
  (interactive)
  (find-function (read-command "Find function: ")))

(defun nbm-tex-build ()
  (interactive)
  (TeX-command-run-all nil))

(defun nbm-M-x ()
  (interactive)
  (helm-M-x nil))

(defun nbm-find-files ()
  (interactive)
  (helm-find-files nil))

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

(defun nbm-config ()
  "Open one of the nbm configuration files."
  (interactive)
  (let (choice)
    (setq choice (read-char "Which file do you want to open?
c) nbm-config.org
k) nbm_key_tree.org"))
    (if (equal choice ?k)
	(find-file (nbm-root-f "nbm-system/nbm_key_tree.org")))
    (if (equal choice ?c)
	(find-file (nbm-root-f "nbm-system/nbm_config.org")))
    ))

(defun nbm-other-window ()
  (interactive)
  (other-window 1))

(defun nbm-comment-or-uncomment-line ()
  (interactive)
  (evilnc-comment-or-uncomment-lines 1))

(defun nbm-open-key-tree ()
  (interactive)
  (find-file (nbm-root-f "nbm-system/nbm_key_tree.org")))

(defun nbm-org-roam-search ()
  "Grep my org roam folder."
  (interactive)
  (setq keyword (read-string "Enter the search keyword (regexp) for org-roam: " nil nil nil nil))
  (rgrep keyword "*.org" (nbm-f "org/") nil))

(defun nbm-org-toggle-strike-through ()
  "Toggle strike though the current item."
  (interactive)
  (let (end)
    (end-of-line) (setq end (point))
    (beginning-of-line) (re-search-forward "[*]+ \\|[-+] \\|[0-9]+[.)] " end t)
    (when (string= (char-to-string (char-after)) "[")
	(forward-sexp) (forward-char))
    (if (equal (char-after) ?+)
	(progn
	  (delete-char 1) (end-of-line) (delete-char -1))
      (progn
	(insert "+") (end-of-line) (insert "+")))))

(defun nbm-org-toggle-checkbox ()
  "Toggle checkbox in the current item.
If there is no checkbox, create one."
  (interactive)
  (save-excursion
    (let (end)
      (end-of-line) (setq end (point))
      (beginning-of-line) (re-search-forward "\\(^[*]+ \\|^[-+] \\|^[0-9]+[.)] \\)" end t)
      (when (match-string 1)
	(if (string= (char-to-string (char-after)) "[")
	    (org-toggle-checkbox)
	  (if (equal (substring (match-string 1) 0 1) "*")
	      (insert "[/] ")
	    (insert "[ ] ")))))))

(defun nbm-org-deadline ()
  (interactive)
  (org-deadline nil))

(defun nbm-org-schedule ()
  (interactive)
  (org-schedule nil))

(defun nbm-magnet()
  "Adjust the current frame as Magnet does. Like Vim, h means left and l means right."
  (interactive)
  (let (choice)
    (setq choice (read-char (format
			     "Select the position: (Like Vim, h means left and l means right.)\n
%36s%18s\n
%18s%18s%18s%18s\n\n
%36s%18s
"
			     (concat (nbm-string-key "u") ": upper-left ")
			     (concat (nbm-string-key "i") ": upper-right")
			     (concat (nbm-string-key "h") ": left       ")
			     (concat (nbm-string-key "j") ": lower-left ")
			     (concat (nbm-string-key "k") ": lower-right")
			     (concat (nbm-string-key "l") ": right      ")
			     (concat (nbm-string-key "c") ": center     ")
			     (concat (nbm-string-key "m") ": max        ")
			     )))
    (nbm-magnet-move-frame choice)))

(defun nbm-magnet-move-frame (pos)
  "Move the current frame as Magnet does."
  (let (x y height width monitor))
  (setq monitor (nth 1 (frame-monitor-attributes))) ; workable area
  (setq x (nth 1 monitor)    ; x-coordinate of the current monitor's upper right corner
	y (nth 2 monitor)    ; y-coordinate of the current monitor's upper right corner
	width (nth 3 monitor)
	height (nth 4 monitor))
  (if (= pos ?c)
      (setq x (+ x (/ width 4))
	    y (+ y (/ height 4))))
  (if (memq pos '(?l ?i ?k))
      (setq x (+ x (/ width 2))))
  (if (memq pos '(?j ?k))
      (setq y (+ y (/ height 2))))
  (unless (= pos ?m)
    (setq width (/ width 2)))
  (if (memq pos '(?u ?j ?i ?k ?c))
      (setq height (/ height 2)))
  (setq width (- width 40))
  (setq height (- height 40))
  (set-frame-position (selected-frame) x y)
  (set-frame-size  (selected-frame) width height t)) ; t means pixelwise dimension

(defun nbm-string-key (string)
  "Return STRING with font for keys."
  (propertize string 'face '(:foreground "MediumSpringGreen" :weight bold)))

(defun nbm-string-terminal-node (string)
  "Return STRING with font for keys."
  (propertize string 'face '(:foreground "SandyBrown")))

(defun nbm-string-internal-node (string)
  "Return STRING with font for keys."
  (propertize string 'face '(:foreground "Deepskyblue1")))

(defun nbm-test ()
  (interactive)
  (message "This is a test."))

