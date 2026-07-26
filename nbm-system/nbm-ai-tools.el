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

(defconst nbm-ai-command-defaults
  '(("r" . "Resolve the comment")
    ("g" . "Check the grammar")
    ("s" . "Check this with sage")
    ("m" . "Check if the content is mathematically correct"))
  "Commands used to initialize the customizable `nbm-ai-command' menu.")

(defconst nbm-ai-command-reserved-keys '("+" "-")
  "Keys reserved by the `nbm-ai-command' menu.")

(defun nbm-ai-command--initialize ()
  "Create the user command file with the default commands if it is absent."
  (let ((file (concat *nbm-home*
                      "nbm-user-settings/nbm-variables/nbm-ai-command.txt")))
    (unless (file-exists-p file)
      (nbm-set-user-variable
       "ai-command"
       (concat
        (mapconcat
         (lambda (item)
           (format "KEY=%s, VALUE=%s" (car item) (cdr item)))
         nbm-ai-command-defaults
         "\n")
        "\n")))))

(defun nbm-ai-command--read ()
  "Read a command or manage the customizable command menu.
Return nil after adding or deleting a command."
  (nbm-ai-command--initialize)
  (let ((data (reverse (nbm-data-get-all "ai-command")))
        (prompt "Command:\n+) Add a command\n-) Delete a command\n")
        command-list choice item key)
    (dolist (command data)
      (setq command-list
            (concat command-list
                    (format "%s) %s\n" (car command) (cdr command)))))
    (setq prompt
          (concat prompt command-list
                  "anything else) Enter command\n"
                  "Choice: "))
    (setq choice (char-to-string (read-char prompt)))
    (cond
     ((equal choice "+")
      (setq key
            (char-to-string
             (read-char
              "Enter a key for the command. (+ and - are reserved): ")))
      (if (member key nbm-ai-command-reserved-keys)
          (user-error "The key `%s' is reserved" key)
        (nbm-data-add "ai-command" key (read-string "Command: ")))
      nil)
     ((equal choice "-")
      (if data
          (progn
            (setq key
                  (char-to-string
                   (read-char
                    (concat "Enter the key of the command to delete:\n"
                            command-list))))
            (if (nbm-data-get "ai-command" key)
                (nbm-data-delete "ai-command" key)
              (message "There is no command with key `%s'." key)))
        (message "There are no commands to delete."))
      nil)
     ((setq item (assoc choice data))
      (cdr item))
     (t
      (read-string "Command: ")))))

(defun nbm-ai-command (cmd)
  "Send CMD to the system terminal with the current file name and line number.
If a region is active, send the starting and ending line numbers of the
region instead.  Use + and - in the interactive menu to add and delete
commands.  The commands are stored in nbm-ai-command.txt in the user
variables directory."
  (interactive (list (nbm-ai-command--read)))
  (when cmd
    (let* ((direct (and (consp cmd) (eq (car cmd) 'direct)))
           (cmd (if (consp cmd) (cdr cmd) cmd)))
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
          (deactivate-mark))))))
