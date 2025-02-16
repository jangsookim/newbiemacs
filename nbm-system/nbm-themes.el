(defun nbm-theme-change ()
  "Change theme"
  (interactive)
  (let (theme)
    (setq theme (completing-read (format "Choose theme (current theme is %s): "
					 (nbm-get-user-variable "theme"))
				 (custom-available-themes) nil t))
    (nbm-set-user-variable "theme" theme)
    (load-theme (intern theme) t)))

(defun nbm-theme-help ()
  "Show help on themes."
  (interactive)
  (browse-url "https://github.com/doomemacs/themes/tree/screenshots")
  (read-char "How to change themes.

1. Type SPC N T c
2. Choose your favorite theme. (This will change theme for the future session.)
3. Restart Emacs. (This is needed to avoid conflict with the current theme.)

You can find many themes in the link below, which will be launched automatically.
https://github.com/doomemacs/themes/tree/screenshots
Type any key to exit."))
