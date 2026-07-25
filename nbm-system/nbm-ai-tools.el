(defun nbm-claude-open-terminal ()
  "Open a new macOS Terminal.app window in the current folder and run `claude'.
Writes a one-shot `.command' script that `cd's into the current directory
and execs `claude', then hands it to Terminal.app via `open -a' so a fresh
window appears every time."
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (script (make-temp-file "nbm-claude-" nil ".command")))
    (with-temp-file script
      (insert "#!/bin/sh\n")
      (insert (format "cd %s\n" (shell-quote-argument dir)))
      (insert "exec claude\n"))
    (set-file-modes script #o755)
    (shell-command
     (format "open -a /System/Applications/Utilities/Terminal.app %s"
             (shell-quote-argument script)))))

(defun nbm-codex-open-terminal ()
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (script (make-temp-file "nbm-codex-" nil ".command")))
    (with-temp-file script
      (insert "#!/bin/sh\n")
      (insert (format "cd %s\n" (shell-quote-argument dir)))
      (insert "exec codex\n"))
    (set-file-modes script #o755)
    (shell-command
     (format "open -a /System/Applications/Utilities/Terminal.app %s"
             (shell-quote-argument script)))))

(defun nbm-ai-command (cmd)
  "Send CMD to the system terminal with the current file name and line number.
If a region is active, send the starting and ending line numbers of the
region instead.  CMD is chosen from a list of presets, or entered manually
when \"Enter command\" is selected."
  (interactive
   (let ((ch (read-char
              (concat "Command:\n"
                      "r) Resolve the comment\n"
                      "g) Check the grammar\n"
                      "m) Check if the content is mathematically correct\n"
                      "c) Enter command\n"
                      "s) Check this with sage\n"
                      "anything else) Other (direct message)\n"
                      "Choice: "))))
     (list (pcase ch
             (?r "Resolve the comment")
             (?g "Check the grammar")
             (?m "Check if the content is mathematically correct")
             (?c (read-string "Command: "))
             (?s (cons 'sage "Check this with sage"))
             (_ (cons 'direct (read-string "Message: ")))))))
  (let* ((direct (and (consp cmd) (eq (car cmd) 'direct)))
         (sage (and (consp cmd) (eq (car cmd) 'sage)))
         (cmd (if (consp cmd) (cdr cmd) cmd)))
    (when (and sage (not (use-region-p)))
      (error "Region must be active for sage check"))
    (let ((file (unless direct (buffer-file-name))))
      (unless (or direct file)
        (error "Current buffer is not visiting a file"))
      (let* ((text (cond
                    (direct cmd)
                    ((use-region-p)
                     (let* ((beg (region-beginning))
                            (end (region-end))
                            (sl (line-number-at-pos beg))
                            (sc (save-excursion (goto-char beg) (current-column)))
                            (el (line-number-at-pos end))
                            (ec (save-excursion (goto-char end) (current-column))))
                       (format "%s: %s from line %d column %d to line %d column %d"
                               cmd file
                               sl sc el ec)))
                    (t (format "%s: %s line %d"
                               cmd
                               file
                               (line-number-at-pos))))))
        (cond
         ((equal system-type 'darwin)
          (let ((script (format (concat "tell application \"Terminal\" to activate\n"
                                        "delay 0.1\n"
                                        "tell application \"System Events\"\n"
                                        "  keystroke \"%s\"\n"
                                        "  delay 0.1\n"
                                        "  key code 36\n"
                                        "end tell\n"
                                        "delay 0.05\n"
                                        "tell application \"Emacs\" to activate")
                                (replace-regexp-in-string
                                 "[\"\\\\]" "\\\\\\&" text))))
            (call-process "osascript" nil nil nil "-e" script)))
         (t (error "No system-terminal support for this platform")))
        (deactivate-mark)))))
