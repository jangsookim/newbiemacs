(defun nbm-magnet ()
  "Adjust the current frame as Magnet does. Like Vim, h means left and l means right.
Use upper case to cycle through multiple monitors."
  (interactive)
  (let (choice)
    (setq choice (read-char "Select the position: (Like Vim, h means left and l means right. Use upper case to cycle through multiple monitors.)

h: left
j: lower
k: upper
l: right
d: left third
e: center third  
f: right third
m: max
a: adjust height
s: save as startup frame"))
    (when (and (<= ?A choice) (<= choice ?Z))
      (nbm-magnet-next-monitor)
      (setq choice (downcase choice)))
    (cond ((equal ?a choice) (nbm-magnet-adjust-height))
	  ((equal ?s choice) (nbm-save-frame-as-startup))
	  (t (nbm-magnet-move-frame choice) (nbm-magnet-move-frame choice)))))

(defun nbm-magnet-next-monitor ()
  "Move the frame to the next monitor."
  (let (k x y height width monitor)
    (setq k (-elem-index (frame-monitor-attributes) (display-monitor-attributes-list)))
    (setq k (% (1+ k) (length (display-monitor-attributes-list))))
    (setq monitor (nth 1 (nth k (display-monitor-attributes-list))))
    (setq x (nth 1 monitor) y (nth 2 monitor))
    (set-frame-position (selected-frame) x y)))

(defun nbm-magnet-move-frame (pos)
  "Move the current frame as Magnet does."
  (let (x y height width monitor wrong-key)
    (setq monitor (nth 1 (frame-monitor-attributes))) ; workable area
    (setq x (nth 1 monitor)    ; x-coordinate of the current monitor's upper right corner
	  y (nth 2 monitor)    ; y-coordinate of the current monitor's upper right corner
	  width (nth 3 monitor)
	  height (nth 4 monitor))
    (cond ((= pos ?h)
	   (setq width (/ width 2)))
	  ((= pos ?j)
	   (setq y (+ y (/ height 2)))
	   (setq height (/ height 2)))
	  ((= pos ?k)
	   (setq height (/ height 2)))
	  ((= pos ?l)
	   (setq x (+ x (/ width 2)))
	   (setq width (/ width 2)))
	  ((= pos ?d)
	   (setq width (/ width 3)))
	  ((= pos ?f)
	   (setq x (+ x (/ width 3)))
	   (setq width (/ width 3)))
	  ((= pos ?g)
	   (setq x (+ x (* (/ width 3) 2)))
	   (setq width (/ width 3)))
	  ((= pos ?m)) 			; nothing to setup for maximize window
	  (t
	   (setq wrong-key t)))
    (if wrong-key
	(message "You typed a wrong key.")
      (progn
	(setq width (- width 40))
	(setq height (- height (if tool-bar-mode 70 40)))

	(set-frame-position (selected-frame) x y)
	(if (equal system-type 'windows-nt)
	    (set-frame-size  (selected-frame) width (+ height -60 *nbm-magnet-height-adjust*) t) ; t means pixelwise dimension
	  (set-frame-size  (selected-frame) width (+ height *nbm-magnet-height-adjust*) t))))))

(defun nbm-magnet-adjust-height ()
  "Adjust the off-set of max frame height."
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
	(save-buffer) (kill-buffer))))
  (message "The adjusted height will be used from now on."))

(defun nbm-save-frame-as-startup ()
  "Make the current frame size as the startup frame."
  (find-file (concat *nbm-home* "nbm-user-settings/nbm-variables/nbm-startup-frame.txt"))
  (erase-buffer)
  (insert (format "%s %s %s %s"
		  (car (frame-position)) (cdr (frame-position))
		  (frame-width) (frame-height)))
  (save-buffer) (kill-buffer)
  (message "The current frame position and size will be used for the start-up frame."))

(defun nbm-set-startup-frame ()
  "Set up the startup frame."
  (set-frame-position (selected-frame)
		      (nth 0 *nbm-startup-frame*)
		      (nth 1 *nbm-startup-frame*))
  (set-frame-size (selected-frame)
		  (nth 2 *nbm-startup-frame*)
		  (nth 3 *nbm-startup-frame*)))

(defun nbm-make-frame-left ()
  "Make a new frame and adjust it to the left."
  (let (buf)
    (setq buf (current-buffer))
    (switch-to-prev-buffer)
    (make-frame)
    (if (equal system-type 'gnu/linux) (other-frame 1))
    (nbm-magnet-move-frame ?h)
    (switch-to-buffer buf)))

(defun nbm-clone-frame ()
  "Clone this frame and adjust it."
  (interactive)
  (clone-frame nil t)
  (if (equal system-type 'gnu/linux) (other-frame 1))
  (nbm-magnet))

(defun nbm-expel-window ()
  "Expel current window to a new frame."
  (interactive)
  (nbm-clone-frame) (other-frame -1)
  (if (> (length (window-list)) 1)
      (delete-window) (switch-to-prev-buffer))
  (other-frame 1))

(defun nbm-string-key (string)
  "Return STRING with font for keys."
  (propertize string 'face '(:foreground "MediumSpringGreen" :weight bold)))
