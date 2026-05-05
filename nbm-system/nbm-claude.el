(defun nbm-claude-open-vterm ()
  "Open a vterm session and run the `claude` command.
If starting a new session, prompts the user to choose between the 
current directory and the default Claude directory."
  (interactive)
  (require 'vterm)
  (let ((buf-name "*claude-vterm*"))
    (if (get-buffer buf-name)
        ;; If it's already running, just switch to it
        (switch-to-buffer buf-name)
      
      ;; If creating a new buffer, prompt the user for the directory
      (let* (;; Ask the user: y = current dir, n = claude dir
             (use-current-dir (y-or-n-p "Start claude in the current directory? "))
             ;; Temporarily set default-directory based on the answer
             (default-directory (if use-current-dir
                                    default-directory
                                  claude-dir)))
        
        ;; Open vterm and run the command
        (vterm buf-name)
        (sleep-for 0.1) 
        (vterm-send-string "claude")
        (vterm-send-return)))))

(defun nbm-claude-usage ()
  "Send '/usage' and press Enter in the current vterm buffer."
  (interactive)
  ;; Check if we are actually in a vterm buffer to avoid errors
  (if (derived-mode-p 'vterm-mode)
      (progn
        (vterm-send-string "/usage")
        (vterm-send-return))
    (message "Not in a vterm buffer!")))

(defun nbm-claude-model ()
  "Send '/model' and press Enter in the current vterm buffer."
  (interactive)
  ;; Check if we are actually in a vterm buffer to avoid errors
  (if (derived-mode-p 'vterm-mode)
      (progn
        (vterm-send-string "/model")
        (vterm-send-return))
    (message "Not in a vterm buffer!")))

(defun nbm-claude-insert-newest-file ()
  "Insert path to the newest file in the folders in *nbm-screenshots*."
  (interactive)
  (vterm-send-string (car (nbm-files-from-screenshot nil))))

(defun nbm-vterm-copy-mode-toggle ()
  "Toggle `vterm-copy-mode` and adjust `evil` state accordingly.
Switches to `evil-normal-state` when entering copy mode, and
`evil-emacs-state` when exiting."
  (interactive)
  (if vterm-copy-mode
      ;; If vterm-copy-mode is active, turn it off and switch to emacs state
      (progn
        (vterm-copy-mode -1)
        (evil-emacs-state))
    ;; If vterm-copy-mode is inactive, turn it on and switch to normal state
    (progn
      (vterm-copy-mode 1)
      (evil-normal-state)
      (message "vterm-copy-mode: Press \"i\" to exit."))))

(with-eval-after-load 'vterm
  (evil-define-key '(normal motion) vterm-copy-mode-map
    (kbd "i") #'nbm-vterm-copy-mode-toggle))

(defun nbm-vterm-dnd-insert-path (uri _action)
  "Insert the dropped file's path into the current vterm buffer.
URI is a `file://' URI from a drag-and-drop event (e.g. dragging a file
from Finder). The local path is extracted and sent to vterm with
`shell-quote-argument' so spaces and shell metacharacters are escaped."
  (let ((path (dnd-get-local-file-name uri t)))
    (when path
      (vterm-send-string (shell-quote-argument path)))
    'private))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Override the default file-drop handler (which would visit the
              ;; file) so that dropping a file in vterm inserts its path
              ;; instead. Buffer-local so other buffers are unaffected.
              (setq-local dnd-protocol-alist
                          (cons '("^file:" . nbm-vterm-dnd-insert-path)
                                dnd-protocol-alist)))))

(defun nbm-claude-insert-file-name ()
  "Prompt for a file in the current directory and its subdirectories,
and insert its relative path at the current cursor position."
  (interactive)
  (let* ((dir default-directory)
         ;; Fetch all files recursively (the "" regex matches all file names)
         (files (directory-files-recursively dir ""))
         ;; Convert absolute paths to relative paths for cleaner display
         (rel-files (mapcar (lambda (f) (file-relative-name f dir)) files))
         ;; Prompt the user to select a file (nil for predicate, t to require a match)
         (chosen-file (completing-read "Select file to insert: " rel-files nil t)))
    ;; Insert the chosen file at point (cursor position)
    (when (and chosen-file (not (string-empty-p chosen-file)))
      (vterm-send-string (concat "./" chosen-file)))))

(defun nbm-claude-find-org ()
  "Find an existing .org file in the current directory or create a new one."
  (interactive)
  (let* (;; Get a list of all .org files in the current directory
         (org-files (directory-files default-directory nil "\\.org\\'"))
         ;; Prompt the user with auto-completion based on the existing files
         (input-name (completing-read "Org file: " org-files nil nil))
         ;; Ensure the file ends with .org
         (final-name (if (string-suffix-p ".org" input-name)
                         input-name
                       (concat input-name ".org"))))
    ;; Open the existing file or create a new buffer for the new file
    (find-file final-name)))

(defun nbm-claude-find-most-recent-file ()
  "Find and open the most recently modified file in the current directory,
ignoring the '.claude' folder and files created by LaTeX compilation
(e.g. .aux, .log, .pdf, .synctex.gz, the AUCTeX `auto/' folder, etc.).
If the file is already open, it forcefully closes it and reopens it fresh
from disk."
  (interactive)
  (require 'seq) ;; Ensure seq-remove is available
  (let* ((latex-aux-regexp
          (concat "\\."
                  (regexp-opt
                   '("aux" "log" "toc" "lof" "lot" "out"
                     "nav" "snm" "vrb"
                     "bbl" "blg" "bcf" "run.xml"
                     "fls" "fdb_latexmk"
                     "synctex.gz" "synctex"
                     "idx" "ind" "ilg"
                     "pdf" "dvi"))
                  "\\'"))
         (all-files (directory-files-recursively default-directory ""))
         ;; Filter out: the .claude folder, the AUCTeX auto/ folder,
         ;; and files whose extension marks them as LaTeX build output.
         (filtered-files
          (seq-remove (lambda (f)
                        (or (string-match-p "/\\.claude/" f)
                            (string-match-p "/auto/" f)
                            (string-match-p latex-aux-regexp f)))
                      all-files))
         ;; Sort the filtered files by modification time (newest first)
         (sorted-files (sort filtered-files
                             (lambda (f1 f2)
                               (time-less-p
                                (file-attribute-modification-time (file-attributes f2))
                                (file-attribute-modification-time (file-attributes f1)))))))
    (if sorted-files
        (let* ((file-to-open (car sorted-files))
               (existing-buffer (get-file-buffer file-to-open)))
          
          ;; 1. If the file is already open, kill it
          (when existing-buffer
            (with-current-buffer existing-buffer
              ;; Lie to Emacs: tell it there are no unsaved changes so it doesn't prompt us
              (set-buffer-modified-p nil))
            ;; Now kill the buffer silently
            (kill-buffer existing-buffer))
          
          ;; 2. Open the file fresh from the disk
          (find-file file-to-open)
          
          ;; 3. Turn on auto-reload for future background updates
          (auto-revert-mode 1)
          (setq-local auto-revert-verbose nil)
          
          (message "Closed and re-opened %s" (file-name-nondirectory file-to-open)))
      (message "No files found in %s (excluding .claude)" default-directory))))
