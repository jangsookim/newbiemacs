(when (equal (nbm-get-user-variable "editing-style") "windows")
  (global-set-key (kbd "C-o") ctl-x-map)
  (global-set-key (kbd "C-p") mode-specific-map))

(defun nbm-windows-ctrl-x ()
  "Cut the region if a region is selected and run ctl-x-map otherwise."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (setq unread-command-events (listify-key-sequence "\C-o"))))

(defun nbm-windows-ctrl-c ()
  "Copy the region if a region is selected and run mode-specific-map otherwise."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (setq unread-command-events (listify-key-sequence "\C-p"))))
