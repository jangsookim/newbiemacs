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
  (browse-url "https://emacsthemes.com/")
  (read-char "How to change themes.

You can find many themes in the link below, which will be launched automatically.
https://emacsthemes.com/

First, see if your favorite theme is available by doing the following.

1. Type SPC N T c
2. Choose your favorite theme. (This will change theme for the future session.)
3. Restart Emacs. (This is needed to avoid conflict with the current theme.)

If you found your favorite theme, it's done.
Otherwise, you first need to install it as follows.

1. Type M-x package-install
2. Enter the theme name, such as spacemacs-dark

Note that some themes may not be installed in this way.

Type any key to exit."))
