(defun nbm-ai--run-applescript (script &rest args)
  "Run SCRIPT with ARGS and report any AppleScript error."
  (unless (eq system-type 'darwin)
    (error "No system-terminal support for this platform"))
  (with-temp-buffer
    (let ((status
           (apply #'call-process "osascript" nil t nil
                  "-e" script "--" args)))
      (unless (and (integerp status) (zerop status))
        (error "AppleScript failed: %s" (buffer-string))))))

(defconst nbm-ai--open-terminal-script
  "on run argv
  tell application id \"com.apple.Terminal\"
    do script (item 1 of argv)
    activate
  end tell
end run"
  "AppleScript used to open an AI tool in a new Terminal window.")

(defun nbm-ai--open-terminal (program)
  "Open a new Terminal window in the current folder and run PROGRAM."
  (nbm-ai--run-applescript
   nbm-ai--open-terminal-script
   (format "cd %s && exec %s"
           (shell-quote-argument (expand-file-name default-directory))
           (shell-quote-argument program))))

(defconst nbm-ai-command-defaults
  '(("r" . "Resolve the comment")
    ("g" . "Check the grammar")
    ("s" . "Check this with sage")
    ("m" . "Check if the content is mathematically correct"))
  "Commands used to initialize the customizable `nbm-ai-command' menu.")

(defconst nbm-ai-command-reserved-keys '("+" "-" "o")
  "Keys reserved by the `nbm-ai-command' menu.")

(defun nbm-ai-command--initialize ()
  "Create the user command file with the default commands if it is absent."
  (let ((file (expand-file-name
               "nbm-user-settings/nbm-variables/nbm-ai-command.txt"
               *nbm-home*)))
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
  (let* ((data (reverse (nbm-data-get-all "ai-command")))
         (command-list
          (mapconcat
           (lambda (command)
             (concat (nbm-string-key (car command))
                     ") " (cdr command) "\n"))
           data
           ""))
         (prompt
          (concat "Reserved commands:\n"
                  (nbm-string-key "+") ") Add a custom command\n"
                  (nbm-string-key "-") ") Delete a custom command\n"
                  (nbm-string-key "o") ") Open an AI tool\n\n"
                  "Custom commands:\n"
                  command-list
                  (nbm-string-key "other") ") Enter command\n"))
         (choice (char-to-string (read-char prompt)))
         item)
    (cond
     ((equal choice "+")
      (let ((key
             (char-to-string
              (read-char
               (concat "Enter a key for the command. ("
                       (nbm-string-key "+") ", "
                       (nbm-string-key "-") ", and "
                       (nbm-string-key "o") " are reserved): ")))))
        (if (member key nbm-ai-command-reserved-keys)
            (user-error "The key `%s' is reserved" (nbm-string-key key))
          (nbm-data-add "ai-command" key (read-string "Command: "))))
      nil)
     ((equal choice "-")
      (if data
          (let ((key
                 (char-to-string
                  (read-char
                   (concat "Enter the key of the command to delete:\n"
                           command-list)))))
            (if (nbm-data-get "ai-command" key)
                (nbm-data-delete "ai-command" key)
              (message "There is no command with key `%s'."
                       (nbm-string-key key))))
        (message "There are no commands to delete."))
      nil)
     ((equal choice "o")
      (pcase (char-to-string
              (read-char
               (concat "Open an AI tool:\n"
                       (nbm-string-key "x") ") Codex\n"
                       (nbm-string-key "l") ") Claude\n")))
        ("x" (nbm-ai--open-terminal "codex"))
        ("l" (nbm-ai--open-terminal "claude"))
        (key (user-error "There is no AI tool with key `%s'"
                         (nbm-string-key key))))
      nil)
     ((setq item (assoc choice data))
      (cdr item))
     (t
      (read-string "Command: ")))))

(defconst nbm-ai-command--terminal-script
  "on run argv
  set commandText to item 1 of argv
  tell application id \"com.apple.Terminal\"
    if not (exists front window) then error \"No Terminal window is open\"
    do script commandText in selected tab of front window
    activate
  end tell
  tell application \"System Events\"
    set focusDeadline to (current date) + 3
    repeat until frontmost of application process \"Terminal\"
      if (current date) > focusDeadline then error \"Timed out waiting for Terminal to receive focus\"
      delay 0.01
    end repeat
    key code 36
  end tell
end run"
  "AppleScript used to send and submit text in Terminal.")

(defun nbm-ai-command--send-to-terminal (text)
  "Send TEXT to Terminal's selected tab and submit it."
  (nbm-ai--run-applescript nbm-ai-command--terminal-script text))

(defun nbm-ai-command--format-context (cmd)
  "Append the current file and active region location to CMD."
  (let ((file (buffer-file-name)))
    (unless file
      (error "Current buffer is not visiting a file"))
    (if (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (start-line (line-number-at-pos beg))
               (start-column
                (save-excursion
                  (goto-char beg)
                  (current-column)))
               (end-line (line-number-at-pos end))
               (end-column
                (save-excursion
                  (goto-char end)
                  (current-column))))
          (format "%s: %s from line %d column %d to line %d column %d"
                  cmd file start-line start-column end-line end-column))
      (format "%s: %s" cmd file))))

(defun nbm-ai-command (cmd)
  "Send CMD to the system terminal with the current file name.
If a region is active, also send its starting and ending line and column
numbers.  Use + and - in the interactive menu to add and delete
commands, or o to open Codex or Claude.  The commands are stored in
nbm-ai-command.txt in the user variables directory."
  (interactive (list (nbm-ai-command--read)))
  (when cmd
    (nbm-ai-command--send-to-terminal
     (nbm-ai-command--format-context cmd))
    (deactivate-mark)))
