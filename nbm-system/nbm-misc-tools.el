(defun nbm-magnet()
  "Adjust the current frame as Magnet does. Like Vim, h means left and l means right."
  (interactive)
  (let (choice)
    (setq choice (read-char (format
			     "Select the position: (Like Vim, h means left and l means right.)\n
%36s%18s\n
%18s%18s%18s%18s\n\n
%18s%18s    %s
"
			     (concat (nbm-string-key "u") ": upper-left ")
			     (concat (nbm-string-key "i") ": upper-right")
			     (concat (nbm-string-key "h") ": left       ")
			     (concat (nbm-string-key "j") ": lower-left ")
			     (concat (nbm-string-key "k") ": lower-right")
			     (concat (nbm-string-key "l") ": right      ")
			     (concat (nbm-string-key "c") ": center     ")
			     (concat (nbm-string-key "m") ": max        ")
			     (concat (nbm-string-key "a") ": adjust height")
			     )))
    (if (equal ?a choice)
	(nbm-magnet-adjust-height)
      (nbm-magnet-move-frame choice))
    ))

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
  (if (equal system-type 'windows-nt)
      (set-frame-size  (selected-frame) width (+ height -60 *nbm-magnet-height-adjust*) t) ; t means pixelwise dimension
    (set-frame-size  (selected-frame) width (+ height *nbm-magnet-height-adjust*) t)
    )
  )

(defun nbm-magnet-adjust-height ()
  "Adjust the off-set of max frame height."
  (interactive)
  (let (ht done)
    (while (not done)
      (setq *nbm-magnet-height-adjust*
	    (string-to-number
	     (read-string "Enter the number to increase the max frame height. (e.g. 30 or -20)
Be careful not to enter a big number, which will make the frame go below the screen.
Enter here: ")))
      (nbm-magnet-move-frame ?m)
      (when (equal ?y (read-char "Do you want to set this as the max frame height? (type y or n): "))
	(setq done t)
	(find-file (concat *nbm-home* "nbm-user-settings/nbm-variables/nbm-magnet.txt"))
	(erase-buffer) (insert (number-to-string *nbm-magnet-height-adjust*))
	(save-buffer) (kill-buffer)))))

(defun nbm-yank-favorite-string ()
  "Copy a frequently used string to the kill-ring."
  (interactive)
  (let (choice string-list prompt key str beg end temp buf)
    (save-excursion
      (find-file (nbm-f "nbm-user-settings/references/favorites.txt"))
      (setq buf (current-buffer))
      (beginning-of-buffer)
      (setq prompt "Choose the string to yank: (Only the first line of each string is shown.)\n")
      (while (re-search-forward "^\\(.\\)) " nil t)
	(setq key (string-to-char (match-string 0)))
	(setq beg (point))
	(if (re-search-forward "^.) " nil t)
	    (setq end (- (point) 4))
	  (setq end (point-max)))
	(setq str (buffer-substring beg end))
	(setq string-list (cons (list key str) string-list))
	(setq prompt (format "%s\n%c) %s" prompt key
			     (car (split-string str "\n"))))
	(goto-char end))
      (kill-buffer buf)
      (setq choice (read-char prompt))
      (dolist (temp string-list)
	(when (equal choice (nth 0 temp))
	  (kill-new (nth 1 temp))
	  (message (format "Copied in clipboard: %s" (nth 1 temp))))))))
