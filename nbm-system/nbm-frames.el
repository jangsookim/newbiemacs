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

