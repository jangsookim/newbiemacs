;; torus-game.el -- play the torus game in Emacs!
;;
;; @@@  /@\  @@\ @ @  /@@
;;  @   @ @  @ / @ @   \
;;  @   \@/  @ \ \@/  @@/

(if (fboundp 'nbm-f)
    (defconst *torus-game-path* (nbm-f "nbm-user-settings/"))
  (progn
    (defconst *torus-game-path* (concat (getenv "HOME") "/.newbiemacs/"))
    (unless (file-exists-p *torus-game-path*)
      (make-directory *torus-game-path*))))

(defun torus-load-theme (theme)
  (cond ((equal theme 1)
	 (defun torus-color-x (string)
	   (propertize string 'face '(:foreground "Navajowhite1" :weight bold)))
	 (defun torus-color-a (string)
	   (propertize string 'face '(:foreground "Indianred2" :weight bold)))
	 (defun torus-color-b (string)
	   (propertize string 'face '(:foreground "Lightblue1" :weight bold)))
	 (defun torus-color-c (string)
	   (propertize string 'face '(:foreground "#Ffd60a" :weight bold))) ; Systemyellowcolor
	 (defun torus-color-d (string)
	   (propertize string 'face '(:foreground "Deepskyblue3" :weight bold)))
	 (defun torus-color-e (string)
	   (propertize string 'face '(:foreground "MediumSlateBlue" :weight bold)))
	 (defun torus-color-y (string)
	   (propertize string 'face '(:foreground "Navajowhite1" :weight bold)))
	 )
	((equal theme 2)
	 (defun torus-color-x (string)
	   (propertize string 'face '(:foreground "Navajowhite1" :weight bold)))
	 (defun torus-color-a (string)
	   (propertize string 'face '(:foreground "Red1" :weight bold)))
	 (defun torus-color-b (string)
	   (propertize string 'face '(:foreground "DeepSkyBlue" :weight bold)))
	 (defun torus-color-c (string)
	   (propertize string 'face '(:foreground "FloralWhite" :weight bold)))
	 (defun torus-color-d (string)
	   (propertize string 'face '(:foreground "orange" :weight bold)))
	 (defun torus-color-e (string)
	   (propertize string 'face '(:foreground "#Bf5af2" :weight bold))) ; Systempurplecolor
	 (defun torus-color-y (string)
	   (propertize string 'face '(:foreground "Navajowhite1" :weight bold)))
	 )
	((equal theme 3)
	 (defun torus-color-x (string)
	   (propertize string 'face '(:foreground "Navajowhite1" :weight bold))) ; CUD ver4 baige
	   ;(propertize string 'face '(:foreground "#ffff80" :weight bold))) ; CUD ver4 cream
	(defun torus-color-a (string)
	   (propertize string 'face '(:foreground "#FF4B00" :weight bold))) ; CUD ver4 red
	 (defun torus-color-b (string)
	   (propertize string 'face '(:foreground "#FFF100" :weight bold))) ; CUD ver4 yellow
	 (defun torus-color-c (string)
	   (propertize string 'face '(:foreground "#03af7a" :weight bold))) ; CUD ver green
	 (defun torus-color-d (string)
	   (propertize string 'face '(:foreground "#005AFF" :weight bold))) ; CUD ver blue
	 (defun torus-color-e (string)
	   (propertize string 'face '(:foreground "#4dc4ff" :weight bold))) ; CUD ver sky blue
	 (defun torus-color-y (string)
	   (propertize string 'face '(:foreground "#804000" :weight bold))) ; CUD ver4 brown
	 )
	(t
	 (defun torus-color-x (string)
	   (propertize string 'face '(:foreground "Navajowhite1" :weight bold)))
	 (defun torus-color-a (string)
	   (propertize string 'face '(:foreground "Palevioletred1" :weight bold)))
	 (defun torus-color-b (string)
	   (propertize string 'face '(:foreground "#0a84ff" :weight bold))) ; Systembluecolor
	 (defun torus-color-c (string)
	   (propertize string 'face '(:foreground "#Ffd60a" :weight bold))) ; Systemyellowcolor
	 (defun torus-color-d (string)
	   (propertize string 'face '(:foreground "#Ac8e68" :weight bold))) ; Systembrowncolor
	 (defun torus-color-e (string)
	   (propertize string 'face '(:foreground "#32d74b" :weight bold))) ; Systemgreencolor
	 (defun torus-color-y (string)
	   (propertize string 'face '(:foreground "Navajowhite1" :weight bold)))
	)))

(defun torus-change-theme-prompt (theme)
  (torus-load-theme theme)
  (format " %s) %s %s %s %s %s (%s) %s"
	  theme
	  (torus-color-a "@@@")
	  (torus-color-b "@@@")
	  (torus-color-c "@@@")
	  (torus-color-d "@@@")
	  (torus-color-e "@@@")
	  (torus-color-y "@@@")
	  (torus-color-x "---")
	  ))

(defun torus-change-theme ()
  (interactive)
  (let (choice)
    (setq choice (read-char
		  (concat
		   "Choose the color theme:\n"
		   (mapconcat 'torus-change-theme-prompt (list 1 2 3 4) "\n")
		  )))
    (torus-load-theme (string-to-number (char-to-string choice)))
    (if (fboundp 'nbm-set-user-variable) (nbm-set-user-variable "torus" choice))
    ))

(defun torus ()
  (interactive)
  (switch-to-buffer "torus")
  (torus-mode)
  ;; (torus-load-theme 1)
  (torus-load-theme
   (if (fboundp 'nbm-get-user-variable)
       (if (nbm-get-user-variable "torus" nil)
	   (string-to-number (nbm-get-user-variable "torus" nil))
	 1)
     1))
  (torus-init)
  (if (fboundp 'evil-local-set-key)
      (progn
	(evil-local-set-key 'normal [left] 'torus-move-left)
	(evil-local-set-key 'normal [right] 'torus-move-right)
	(evil-local-set-key 'normal [up] 'torus-move-up)
	(evil-local-set-key 'normal [down] 'torus-move-down)
	(evil-local-set-key 'normal (kbd "j") 'torus-move-left)
	(evil-local-set-key 'normal (kbd "l") 'torus-move-right)
	(evil-local-set-key 'normal (kbd "i") 'torus-move-up)
	(evil-local-set-key 'normal (kbd "k") 'torus-move-down)
	(evil-local-set-key 'normal (kbd "n") 'torus-start-game)
	(evil-local-set-key 'normal (kbd "r") 'torus-resume-game)
	(evil-local-set-key 'normal (kbd "q") 'torus-end-game)
	(evil-local-set-key 'normal (kbd "c") 'torus-change-theme)
	(evil-local-set-key 'normal (kbd "s") 'torus-show-score-board)
	(evil-local-set-key 'normal (kbd "p") 'torus-pause-game))
    (progn
      (local-set-key [left] 'torus-move-left)
      (local-set-key [right] 'torus-move-right)
      (local-set-key [up] 'torus-move-up)
      (local-set-key [down] 'torus-move-down)
      (local-set-key (kbd "j") 'torus-move-left)
      (local-set-key (kbd "l") 'torus-move-right)
      (local-set-key (kbd "i") 'torus-move-up)
      (local-set-key (kbd "k") 'torus-move-down)
      (local-set-key (kbd "n") 'torus-start-game)
      (local-set-key (kbd "r") 'torus-resume-game)
      (local-set-key (kbd "q") 'torus-end-game)
      (local-set-key (kbd "c") 'torus-change-theme)
      (local-set-key (kbd "s") 'torus-show-score-board)
      (local-set-key (kbd "p") 'torus-pause-game))))

(defun torus-mode ()
  (setq major-mode 'torus-mode)
  (setq mode-name "Torus")
  (font-lock-mode))

(defun torus-init ()
  "Start a new game of torus."
  (setq *torus-num-cols* 3)
  (setq *torus-pole-height* *torus-num-cols*)
  (setq *torus-box* (make-vector (* *torus-box-height* *torus-num-cols*) nil))
  (setq *torus-pole* (make-vector (* *torus-pole-height* *torus-num-cols*) nil))
  (setq *torus-flying-tori* (make-vector 100 nil))
  (setq *torus-flying-tori-height* (make-vector 100 nil))
  (setq *torus-flying-tori-waiting* (make-vector 100 0))
  (setq *torus-num-tori* (make-vector 100 0))
  (setq *torus-level-up-time* 5)
  (setq *torus-num-colors* 5)
  (setq *torus-num-tori-in-pole* 0)
  (setq *torus-score* 0)
  (setq *torus-level* 0)
  (setq *torus-level-gauge* 0)
  (setq *torus-time* 0)
  (torus-init-pole 0)
  (torus-print-main)
  )

(defun torus-set-difficulty (difficulty)
  (cond ((equal ?1 difficulty)
	 (setq *torus-difficulty* 1)
	 (defconst *torus-game-speed* 0.1)             ; the lower the faster
	 )
	((equal ?2 difficulty)
	 (setq *torus-difficulty* 2)
	 (defconst *torus-game-speed* 0.1)             ; the lower the faster
	 )
	(t
	 (setq *torus-difficulty* 3)
	 (defconst  *torus-game-speed* 0.1)
	 )))
	 
(defun torus-difficulty-as-str (difficulty)
  (cond ((equal 1 difficulty) "Normal")
	((equal 2 difficulty) "Half-glazed/rotate (Testing)")
	(t "Half-glazed (flip)")
	))
    


(defun torus-get-str-difficulty ()
  "Return the current difficulty in string format."
  (torus-difficulty-as-str *torus-difficulty*)
  )

(defun torus-select-difficulty-prompt (difficulty)
  "Return prompt of the option to select difficulty"
  (format " (%s) %s" difficulty (torus-difficulty-as-str difficulty)))

(defun torus-select-difficulty ()
  (interactive)
  (torus-set-difficulty (read-char
			 (concat
			  "Choose difficulty:\n"
			  (mapconcat 'torus-select-difficulty-prompt (list 1 2 3) "  "))
			 )))

(defvar *torus-box* nil
  "The torus box which is a list.")

(defvar *torus-num-tori* nil
  "The vector whose ith entry is the number of tori in column i.")

(defvar *torus-pole* nil
  "The torus pole which is a list.")

(defvar *torus-flying-tori* nil
  "The flying tori which is a list.")

(defvar *torus-flying-tori-height* nil
  "The heights of flying tori which is a list.")

(defvar *torus-flying-tori-waiting* nil
  "The waiting time of flying tori which is a list.")

;(defconst *torus-game-speed* 0.1)             ; the lower the faster
(defconst *torus-gauge-time* 20)
(defconst *torus-flying-torus-speed-factor* 1)
(defconst *torus-box-height* 20)
(defconst *torus-score-per-torus* 300)
(defconst *torus-waiting-time* 10)
(defvar *torus-game-on* 1)
(defvar *torus-num-cols*)
(defvar *torus-timer* nil)
(defvar *torus-pole-height*)
(defvar *torus-pole-pos*)
(defvar *torus-num-colors*)
(defvar *torus-num-tori-in-pole*)
(defvar *torus-score*)
(defvar *torus-level*)
(defvar *torus-time*)
(defvar *torus-level-up-time*)
(defvar *torus-level-gauge*)
(defvar *torus-last-user* nil)
(defvar *torus-difficulty* 1)

;; printing

(defun torus-print-all ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (torus-print-box)
    (torus-print-pole)
    (torus-print-level-gauge)
    (torus-print-score)
    (torus-print-level)))

(defun torus-print-main ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "@@@  /@\\  @@\\ @ @  /@@\n")
    (insert " @   @ @  @ / @ @   \\\n")
    (insert " @   \\@/  @ \\ \\@/  @@/\n")
    (insert "
n: New game
r: Restart game
p: Pause game
s: Score board
c: Change color theme
q: Quit game

Movement keys
left : j or left arrow key
right: l or right arrow key
up   : i or up arrow key
down : k or down arrow key

* Warning *
Due to the heavy use of start-of-art animation,
your computer may be slowed down while playing torus.
In this case you are recommended to play \"torus\" instead.
")
    ))

(defun torus-print-box ()
  (let ((inhibit-read-only t))
    (torus-print-string " |" -1)
    (dotimes (col *torus-num-cols*)
      (torus-print-entry -2))
    (torus-print-string "|\n" -1)
    (dotimes (row *torus-box-height*)
      (torus-print-string " |" -1)
      (dotimes (col *torus-num-cols*)
        (if (and (elt *torus-flying-tori* col)  ; if there is a flying torus
                 (or (equal row (- *torus-box-height* (elt *torus-flying-tori-height* col)))
                     (equal row (1+ (- *torus-box-height* (elt *torus-flying-tori-height* col))))
                     (equal row (1- (- *torus-box-height* (elt *torus-flying-tori-height* col))))))
            (torus-print-flying-torus row col)
	  (torus-print-entry (torus-box-get-raw-entry row col))))
          ;(torus-print-entry (torus-box-get-entry-to-print row col))))
      (torus-print-string "|\n" -1))))


(defun torus-get-torus-color (c angle part)
  "Return color of torus."
  (if (equal 1 *torus-difficulty*) c (if (> 3 (% (+ angle part 5) 6)) c 100 )))

(defun torus-print-flying-torus (row col)
  (let ((inhibit-read-only t))
    (if (equal (% (elt *torus-flying-tori-height* col) 4) 0) ; the flying torus is rotating with cycle 4
        (progn
          (if (equal row (- *torus-box-height* (elt *torus-flying-tori-height* col)))
              (torus-print-string " @ @ "  (torus-get-torus-color (elt *torus-flying-tori* col) 4 1)))
          (if (equal row (1- (- *torus-box-height* (elt *torus-flying-tori-height* col))))
              (torus-print-string " /@\\ " (torus-get-torus-color (elt *torus-flying-tori* col) 4 1)))
          (if (equal row (1+ (- *torus-box-height* (elt *torus-flying-tori-height* col))))
              (torus-print-string " \\@/ " (torus-get-torus-color (elt *torus-flying-tori* col) 1 1))))
      (if (equal (% (elt *torus-flying-tori-height* col) 4) 1) ; the flying torus is rotating with cycle 4
	  (progn
            (if (equal row (- *torus-box-height* (elt *torus-flying-tori-height* col)))
		(torus-print-string " @@@ " (torus-get-torus-color (elt *torus-flying-tori* col) 4 1)))
            (if (equal row (1- (- *torus-box-height* (elt *torus-flying-tori-height* col))))
		(torus-print-string "     " (elt *torus-flying-tori* col)))
            (if (equal row (1+ (- *torus-box-height* (elt *torus-flying-tori-height* col))))
		(torus-print-string "     " (elt *torus-flying-tori* col))))
	(if (equal (% (elt *torus-flying-tori-height* col) 4) 2) ; the flying torus is rotating with cycle 4
            (progn
              (if (equal row (- *torus-box-height* (elt *torus-flying-tori-height* col)))
		  (torus-print-string " @ @ " (torus-get-torus-color (elt *torus-flying-tori* col) 4 1)))
              (if (equal row (1- (- *torus-box-height* (elt *torus-flying-tori-height* col))))
		  (torus-print-string " /@\\ " (torus-get-torus-color (elt *torus-flying-tori* col) 1 1)))
              (if (equal row (1+ (- *torus-box-height* (elt *torus-flying-tori-height* col))))
		  (torus-print-string " \\@/ " (torus-get-torus-color (elt *torus-flying-tori* col) 4 1))))
	  (progn
            (if (equal row (- *torus-box-height* (elt *torus-flying-tori-height* col)))
		(torus-print-string " @@@ " (torus-get-torus-color (elt *torus-flying-tori* col) 1 1)))
            (if (equal row (1- (- *torus-box-height* (elt *torus-flying-tori-height* col))))
		(torus-print-string "     " (elt *torus-flying-tori* col)))
            (if (equal row (1+ (- *torus-box-height* (elt *torus-flying-tori-height* col))))
		(torus-print-string "     " (elt *torus-flying-tori* col)))))))))

(defun torus-print-pole ()
  (let ((inhibit-read-only t))
    (dotimes (row *torus-pole-height*)
      (torus-print-string " |" -1)
      (dotimes (col *torus-num-cols*)
        (torus-print-entry (torus-pole-get-raw-entry row col)))
      (torus-print-string "|\n" -1))
  (torus-print-string " |" -1)
  (dotimes (col *torus-num-cols*)
    (if (= col *torus-pole-pos*)
        (torus-print-entry -2)
        (torus-print-entry nil)))
  (torus-print-string "|\n" -1)))

(defun torus-print-string (string color)
  "Print string with color foreground and  black background."
  (if (equal color -1)
      (insert (torus-color-x string)))
  (if (equal color 0)
      (insert (torus-color-a string)))
  (if (equal color 1)
      (insert (torus-color-b string)))
  (if (equal color 2)
      (insert (torus-color-c string)))
  (if (equal color 3)
      (insert (torus-color-d string)))
  (if (equal color 4)
      (insert (torus-color-e string)))
  (if (equal color 100)
      (insert (torus-color-y string)))
  )

(defun torus-print-horizontal-torus (n)
  (torus-print-string " @" (torus-get-torus-color (elt n 0) (elt n 1) 0))
  (torus-print-string "@" (torus-get-torus-color (elt n 0) (elt n 1) 1))
  (torus-print-string "@ " (torus-get-torus-color (elt n 0) (elt n 1) 2))
  )

(defun torus-print-entry (n)
  (cond ((eq n nil)
	 (torus-print-string "     " -1))
	((equal n "*")
	 (torus-print-string "*" -1))
	((eq n -1)
	 (torus-print-string "  |  " -1))
	((eq n -2)
	 (torus-print-string " --- " -1))
	((sequencep n)
	 (torus-print-horizontal-torus n))
  ))


(defun torus-print-score ()
  (torus-print-string "\n\n Score: " -1)
  (torus-print-string (number-to-string *torus-score*) -1)
  )

;; (defun torus-print-time ()
;;   (torus-print-string "\n\n Time: " -1)
;;   (torus-print-string (number-to-string *torus-time*) -1)
;;   )

(defun torus-print-level ()
  (torus-print-string "\n\n Level: " -1)
  (torus-print-string (number-to-string *torus-level*) -1)
  (torus-print-string "\n" -1)
  )

(defun torus-print-level-gauge ()
  (torus-print-string "\n  " -1)
  (dotimes (x *torus-level-gauge*)
    (torus-print-entry "*"))
  (torus-print-string "\n" -1)
  )


;; get and set entries

(defun torus-box-set-entry (row col value)
  (aset *torus-box*
        (+ col
           (* row
              *torus-num-cols*))
        value))

(defun torus-box-get-entry (row col)
  (if (sequencep (torus-box-get-raw-entry row col))
      (elt (torus-box-get-raw-entry row col) 0)
    (torus-box-get-raw-entry row col)))

(defun torus-box-get-raw-entry (row col)
  (elt *torus-box*
     (+ col
        (* row
           *torus-num-cols*))))

(defun torus-pole-get-raw-entry (row col)
  (elt *torus-pole*
       (+ col
          (* row
             *torus-num-cols*))))

(defun torus-pole-set-entry (row col value)
  (aset *torus-pole*
        (+ col
           (* row
              *torus-num-cols*))
        value))

(defun torus-get-num-tori (col)
  "Return the number of tori in column col."
  (elt *torus-num-tori* col))

(defun torus-pole-get-top-torus ()
    (torus-pole-get-raw-entry
     (- *torus-pole-height* *torus-num-tori-in-pole*)
     *torus-pole-pos*))

;; initiate the pole

(defun torus-init-pole (pos)
  "Initiate the pole at column pos."
  (setq *torus-pole-pos* pos)
  (dotimes (row *torus-pole-height*)
    (dotimes (col *torus-num-cols*)
      (if (= col pos)
          (torus-pole-set-entry row col -1)
        (torus-pole-set-entry row col nil)))))

;; moving the pole

(defun torus-move-pole-left ()
  "Move the pole to the left."
  (if (> *torus-pole-pos* 0)
      (let (old-pos new-pos)
        (setq old-pos *torus-pole-pos*)
        (setq new-pos (1- *torus-pole-pos*))
        (dotimes (row *torus-pole-height*)
          (torus-pole-set-entry row new-pos
                                (torus-pole-get-raw-entry row old-pos))
          (torus-pole-set-entry row old-pos nil))
        (setq *torus-pole-pos* new-pos))))

(defun torus-move-pole-right ()
  "Move the pole to the right."
  (if (< *torus-pole-pos* (1- *torus-num-cols*))
      (let (old-pos new-pos)
        (setq old-pos *torus-pole-pos*)
        (setq new-pos (1+ *torus-pole-pos*))
        (dotimes (row *torus-pole-height*)
          (torus-pole-set-entry row new-pos
                                (torus-pole-get-raw-entry row old-pos))
          (torus-pole-set-entry row old-pos nil))
        (setq *torus-pole-pos* new-pos))))

;; insert and delete a torus in the pole

(defun torus-fliped-torus (torus)
  (if torus
      (if (sequencep torus)
	  (list
	   (elt torus 0)
	   (% (+ (elt torus 1) 3) 6))
	torus)
    nil))

(defun torus-rotated-r-torus (torus)
  (if torus
      (if (sequencep torus)
	  (list
	   (elt torus 0)
	   (% (+ (elt torus 1) 5) 6))
	torus)
    nil))

(defun torus-rotated-l-torus (torus)
  (if torus
      (if (sequencep torus)
	  (list
	   (elt torus 0)
	   (% (+ (elt torus 1) 1) 6))
	torus)
    nil))

(defun torus-get-torus-to-insert-to-pole (torus)
  (if (eq  *torus-difficulty* 2) torus (torus-fliped-torus torus)))

(defun torus-pole-insert ()
  "Insert into the pole the bottom torus in the column where the pole is at."
  (torus-pole-set-entry (- *torus-pole-height* *torus-num-tori-in-pole* 1)
                        *torus-pole-pos*
                        (torus-get-torus-to-insert-to-pole (torus-box-get-raw-entry (1- *torus-box-height*) *torus-pole-pos*)))
  (torus-increase-num-tori-in-pole)
  )

(defun torus-pole-delete-top ()
  "Delete the top torus."
  (torus-pole-set-entry (- *torus-pole-height* *torus-num-tori-in-pole*)
                        *torus-pole-pos* -1)
  (torus-decrease-num-tori-in-pole))



;; flying torus

(defun torus-update-flying-tori ()
  "Update the flying torus in each column."
  (dotimes (col *torus-num-cols*)
    (torus-update-flying-torus col))
  )

(defun torus-update-flying-torus (col)
  "Update the flying torus in column col."
  (if (elt *torus-flying-tori* col)     ; if there is a flying torus in column col
      (if (equal (% *torus-time*        ; if the time is correct for that torus
                    *torus-flying-torus-speed-factor*) 0)
          (progn
            (if (> (elt *torus-flying-tori-height* col) (+ 2 (torus-get-num-tori col)))
                (aset *torus-flying-tori-height* col (1- (elt *torus-flying-tori-height* col)))
              (progn
                (torus-insert-flying-torus col (elt *torus-flying-tori* col))
                (aset *torus-flying-tori* col nil)
                ))))
    (if (equal (elt *torus-flying-tori-waiting* col) *torus-waiting-time*) ; if it's time for a new torus
        (progn
          (aset *torus-flying-tori* col (torus-random-torus))
          (aset *torus-flying-tori-height* col *torus-box-height*)
          (aset *torus-flying-tori-waiting* col 0))
      (aset *torus-flying-tori-waiting* col (1+ (elt *torus-flying-tori-waiting* col)))
      )))

(defun torus-insert-flying-torus (col torus)
  "Insert torus in column col."
  (torus-box-set-entry (- *torus-box-height* (torus-get-num-tori col) 1)
                       col (list
			    torus
			    (if (< (% (+ 3 (elt *torus-flying-tori-height* col)) 4) 2) 4 1)
			    ) )
  (torus-increase-num-tori col))

;; insert and delete a torus in the box

(defun torus-box-insert-new (col)
  "Insert a random torus in column col."
  (torus-box-set-entry (- *torus-box-height* (torus-get-num-tori col) 1)
                       col (list (torus-random-torus) 1))
  (torus-increase-num-tori col)
  )

(defun torus-box-insert-row ()
  "Insert a new torus in each column."
  (dotimes (col *torus-num-cols*)
    (torus-box-insert-new col))
  )

(defun torus-box-insert-from-pole ()
  "Insert the top torus of the pole to the box."
  (let (rr k col)
    (setq col *torus-pole-pos*)
    (setq k (torus-get-num-tori col)) ; k is the number of boxes to modify
    (dotimes (r k)
      (setq rr (+ (- *torus-box-height* k 1) r))
      (torus-box-set-entry rr col
			   (if (equal 2 *torus-difficulty*)
			       (torus-rotated-r-torus (torus-box-get-raw-entry (1+ rr) col))
                               (torus-box-get-raw-entry (1+ rr) col))))
    (setq rr (1- *torus-box-height*))
    (torus-box-set-entry rr col (torus-pole-get-top-torus))
    (torus-increase-num-tori col))
  )

(defun torus-box-remove-torus (row col)
  "Remove the torus at given position."
  (let (rr r k)
    (setq k (- (+ (1+ row) (torus-get-num-tori col)) *torus-box-height*)) ; k is the number of boxes to modify
    (dotimes (r (1- k))
      (setq rr (- row r))
      (torus-box-set-entry rr col
			   (if (equal 2 *torus-difficulty*)
			       (torus-rotated-l-torus (torus-box-get-raw-entry (1- rr) col))
                               (torus-box-get-raw-entry (1- rr) col))))
    (setq rr (- row (1- k)))
    (torus-box-set-entry rr col nil)
    (torus-decrease-num-tori col)
    ))

(defun torus-remove-bottom (col)
  "Remove the bottom torus in column col."
  (torus-box-remove-torus (1- *torus-box-height*) col))

(defun torus-increase-num-tori (col)
  "Increase the number of tori in column col."
  (aset *torus-num-tori* col (1+ (elt *torus-num-tori* col))))

(defun torus-decrease-num-tori (col)
  "Decrease the number of tori in column col."
  (aset *torus-num-tori* col (1- (elt *torus-num-tori* col))))

(defun torus-increase-num-tori-in-pole ()
  "Increase the number of tori in pole in column col."
  (setq *torus-num-tori-in-pole* (1+ *torus-num-tori-in-pole*)))

(defun torus-decrease-num-tori-in-pole ()
  "Decrease the number of tori in pole in column col."
  (setq *torus-num-tori-in-pole* (1- *torus-num-tori-in-pole*)))

(defun torus-random-torus ()
  "Create a random torus as a number."
  (random *torus-num-colors*))


;; check and delete rows

(defun torus-check-row (row)
  "Check whether the row has the same tori."
  (let (same first)
    (setq first (torus-box-get-entry row 0))
    (if first (setq same 1) (setq same 0))
    (dotimes (c (1- *torus-num-cols*))
      (when (eq first (torus-box-get-entry row (1+ c)))
        (setq same (1+ same))))
    (if (eq same *torus-num-cols*)
        t nil)))

(defun torus-delete-row (row)
  "Delete the row in the box."
  (dotimes (col *torus-num-cols*)
    (torus-box-remove-torus row col)))

(defun torus-delete-same-rows ()
  "Delete all rows with the same torus."
  (dotimes (row *torus-box-height*)
    (when (torus-check-row row)
      (torus-increase-score)
      (torus-delete-row row)
      )))

;; update game score, time, and level

(defun torus-increase-score ()
  "Increase the score."
  (setq *torus-score* (+ *torus-score* (* *torus-score-per-torus* *torus-num-cols*))))

(defun torus-increase-time ()
  "Increase the time."
  (if (equal (% *torus-time* *torus-gauge-time*) 0)
      (setq *torus-level-gauge* (1+ *torus-level-gauge*)))
  (setq *torus-time* (1+ *torus-time*)))

(defun torus-increase-level ()
  "Increase the level."
  (setq *torus-level* (1+ *torus-level*))
  (if (> *torus-pole-height* 2)
      (torus-decrease-pole-height)
    (progn
      (torus-increase-box)
      (torus-increase-pole)
      (torus-increase-num-cols)
      (torus-increase-pole-height)
      )))

(defun torus-increase-box ()
  "Increase the box by adding one more column."
  (let (temp)
    (setq temp (make-vector (* *torus-box-height*
                               (1+ *torus-num-cols*)) nil))
    (dotimes (row *torus-box-height*)
      (dotimes (col *torus-num-cols*)
        (aset temp (+ col
                      (* row (1+ *torus-num-cols*)))
              (torus-box-get-raw-entry row col))))
    (setq *torus-box* temp)))

(defun torus-increase-pole ()
  "Increase the pole by adding one more column."
  (let (temp)
    (setq temp (make-vector (* *torus-pole-height*
                               (1+ *torus-num-cols*)) nil))
    (dotimes (row *torus-pole-height*)
      (dotimes (col *torus-num-cols*)
        (aset temp (+ col
                      (* row (1+ *torus-num-cols*)))
              (torus-pole-get-raw-entry row col))))
    (setq *torus-pole* temp)))

;; update pole height

(defun torus-decrease-pole-height ()
  "Decrease the pole height."
  (if (equal *torus-num-tori-in-pole* *torus-pole-height*)
      (if (equal (torus-get-num-tori *torus-pole-pos*) *torus-box-height*)
          (torus-game-over)
        (torus-move-up)))
  (setq *torus-pole-height* (1- *torus-pole-height*))
  (let (temp)
    (setq temp (make-vector (* *torus-pole-height* *torus-num-cols*) nil))
    (dotimes (row *torus-pole-height*)
      (dotimes (col *torus-num-cols*)
        (aset temp (+ col (* row *torus-num-cols*))
              (torus-pole-get-raw-entry (1+ row) col))))
    (setq *torus-pole* temp)))

(defun torus-increase-pole-height ()
  "Increase the pole height to the number of columns."
  (let (temp rr new-ht old-ht)
    (setq old-ht *torus-pole-height*)
    (setq new-ht *torus-num-cols*)
    (setq temp (make-vector (* new-ht *torus-num-cols*) nil))
    (dotimes (row new-ht)
      (dotimes (col *torus-num-cols*)
        (if (< row (- new-ht old-ht))
            (when (equal col *torus-pole-pos*)
              (aset temp (+ col (* row *torus-num-cols*)) -1))
          (progn
            (setq rr (- row (- new-ht old-ht)))
            (aset temp (+ col (* row *torus-num-cols*))
                  (torus-pole-get-raw-entry rr col)))
          )))
    (setq *torus-pole* temp)
    (setq *torus-pole-height* new-ht)))


(defun torus-increase-num-cols ()
  "Increase the number of columns by 1."
  (setq *torus-num-cols* (1+ *torus-num-cols*))
  )

;; system commands

(defun torus-game-over ()
  (setq *torus-game-on* nil)
  (when *torus-timer* (cancel-timer *torus-timer*))
  (let (user-name)
    (setq user-name (read-string "Game Over!\nEnter your name: " nil nil nil nil))
    (unless (equal user-name "")
      (setq user-name (concat "\"" user-name "\""))
      (torus-update-user-score user-name)
      (torus-show-score-board)
      )
    )
  )

(defun torus-show-score-board ()
  (interactive)
  (when (string-equal major-mode "torus-mode")
    (erase-buffer)
    (insert-file-contents (concat *torus-game-path* "torus-data/scores") nil 0 10000)
    (message "n: New game
r: Restart game
p: Pause game
s: Score board
c: Change color theme
q: Quit game")))

(defun torus-update-user-score (user-name)
  "Update the score of user-name in the score file."
  (let (update score buf)
    (unless (file-exists-p (concat *torus-game-path* "torus-data/"))
      (make-directory (concat *torus-game-path* "torus-data/")))
    (find-file (format "%s/torus-data/scores" *torus-game-path*))
    (setq buf (current-buffer))
    (goto-char (point-min))
    (setq update nil)
    (while (not update)
      (if (equal (point) (point-max))
          (progn
            (torus-insert-score)
            (setq update t))
        (progn
          (if (and (torus-read-score) (< *torus-score* (torus-read-score)))
              (progn
                (next-logical-line)
                (beginning-of-line))
            (progn
              (beginning-of-line)
              (torus-insert-score)
              (setq update t))))
          )
      )
    (save-buffer) (kill-buffer buf)
    ))

(defun torus-insert-score ()
  (insert (format "%20s: Score %10d, Level: %3d, Date: %s"
                  user-name *torus-score* *torus-level*
                  (format-time-string "%Y/%m/%d/%T\n")))
  )

(defun torus-read-score ()
  "Return the number appearing after the current position in the current buffer."
  (let (start end)
    (save-excursion
      (when (re-search-forward "\\([0-9]+\\)," nil t nil)
	(string-to-number (match-string 1))
	)
      )))


;; update game

(defun torus-run-game ()
  (setq *torus-game-on* 1)
  (when *torus-timer* (cancel-timer *torus-timer*))
  (setq *torus-timer* (run-at-time nil *torus-game-speed* 'torus-update)))

(defun torus-update ()
  "Update the game after given time period."
  (dotimes (col *torus-num-cols*)
    (if (and *torus-game-on*
	     (= (torus-get-num-tori col) *torus-box-height*))
	(torus-game-over)))
  (unless (string-equal major-mode "torus-mode")
    (torus-pause-game))

  (when *torus-game-on*
    (torus-update-flying-tori)
    (torus-delete-same-rows)
    (torus-increase-time)
    (when (eq *torus-level-gauge* (* *torus-level-up-time* *torus-num-cols*))
      (torus-increase-level)
      (setq *torus-level-gauge* 0))
    (if (or                             ; if it's time to update flying tori
	 (equal (% *torus-time* (+ *torus-flying-torus-speed-factor* 0)) 0)
	 (equal (% *torus-time* (+ *torus-flying-torus-speed-factor* 1)) 0)
	 (equal (% *torus-time* (+ *torus-flying-torus-speed-factor* 2)) 0)
	 (equal (% *torus-time* (+ *torus-flying-torus-speed-factor* 3)) 0)
	 (equal (% *torus-time* (+ *torus-flying-torus-speed-factor* 4)) 0))
	(torus-print-all))))

;; movements

(defun torus-move-left ()
  (interactive)
  (when *torus-game-on*
    (torus-move-pole-left)
    (torus-print-all)
    ))

(defun torus-move-right ()
  (interactive)
  (when *torus-game-on*
    (torus-move-pole-right)
    (torus-print-all)
    ))

(defun torus-move-up ()
  (interactive)
  (when *torus-game-on*
    (if (and (< (torus-get-num-tori *torus-pole-pos*) *torus-box-height*)
             (> *torus-num-tori-in-pole* 0))
        (progn
          (torus-box-insert-from-pole)
          (torus-pole-delete-top)
          (torus-delete-same-rows)
          (torus-print-all))))
  )

(defun torus-move-down ()
  (interactive)
  (when *torus-game-on*
    (if (and (> (torus-get-num-tori *torus-pole-pos*) 0)
             (< *torus-num-tori-in-pole* *torus-pole-height*))
        (progn
          (torus-pole-insert)
          (torus-remove-bottom *torus-pole-pos*)
          (torus-delete-same-rows)
          (torus-print-all)
          )
      ))
  )

(defun torus-start-game ()
  (interactive)
  (torus-pause-game)
  (torus-select-difficulty)
  (torus-init)
  (torus-run-game)
  (message "Game started."))

(defun torus-resume-game ()
  (interactive)
  (unless *torus-game-on*
    (torus-run-game)
    (message "Game resumed.")))

(defun torus-pause-game ()
  (interactive)
  (setq *torus-game-on* nil)
  (if *torus-timer*
      (cancel-timer *torus-timer*)
      )
  (message "Game paused."))

(defun torus-end-game ()
  (interactive)
  (when *torus-timer* (cancel-timer *torus-timer*))
  (kill-buffer))
