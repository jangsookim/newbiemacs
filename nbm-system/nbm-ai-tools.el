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
