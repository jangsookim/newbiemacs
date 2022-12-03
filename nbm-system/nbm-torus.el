;; torus-game.el -- play the torus game in Emacs!
;;
;; @@@  /@\  @@\ @ @  /@@
;;  @   @ @  @ / @ @   \
;;  @   \@/  @ \ \@/  @@/


(defconst *torus-game-path* (nbm-f "nbm-user-settings/"))

(defun torus ()
  (interactive)
  (switch-to-buffer "torus")
  (torus-mode)
  (torus-init)
  (evil-local-set-key 'normal [left] 'torus-move-left)
  (evil-local-set-key 'normal [right] 'torus-move-right)
  (evil-local-set-key 'normal [up] 'torus-move-up)
  (evil-local-set-key 'normal [down] 'torus-move-down)
  (evil-local-set-key 'normal (kbd "n") 'torus-start-game)
  (evil-local-set-key 'normal (kbd "r") 'torus-resume-game)
  (evil-local-set-key 'normal (kbd "q") 'torus-end-game)
  (evil-local-set-key 'normal (kbd "p") 'torus-pause-game)
  )

(defun torus-mode ()
  (setq major-mode 'torus-mode)
  (setq mode-name "Torus")
  (font-lock-mode)
  (use-local-map torus-mode-map))

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

(defconst *torus-game-speed* 0.1)             ; the lower the faster
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
    (insert "\nPress \"n\" to start a new game.")
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
          (torus-print-entry (torus-box-get-entry row col))))
      (torus-print-string "|\n" -1))))

(defun torus-print-flying-torus (row col)
  (let ((inhibit-read-only t))
    (if (equal (% (elt *torus-flying-tori-height* col) 2) 0) ; the flying torus is rotating
        (progn
          (if (equal row (- *torus-box-height* (elt *torus-flying-tori-height* col)))
              (torus-print-string " @ @ " (elt *torus-flying-tori* col)))
          (if (equal row (1- (- *torus-box-height* (elt *torus-flying-tori-height* col))))
              (torus-print-string " /@\\ " (elt *torus-flying-tori* col)))
          (if (equal row (1+ (- *torus-box-height* (elt *torus-flying-tori-height* col))))
              (torus-print-string " \\@/ " (elt *torus-flying-tori* col))))
      (progn
        (if (equal row (- *torus-box-height* (elt *torus-flying-tori-height* col)))
            (torus-print-string " @@@ " (elt *torus-flying-tori* col)))
        (if (equal row (1- (- *torus-box-height* (elt *torus-flying-tori-height* col))))
            (torus-print-string "     " (elt *torus-flying-tori* col)))
        (if (equal row (1+ (- *torus-box-height* (elt *torus-flying-tori-height* col))))
            (torus-print-string "     " (elt *torus-flying-tori* col)))))))

(defun torus-print-pole ()
  (let ((inhibit-read-only t))
    (dotimes (row *torus-pole-height*)
      (torus-print-string " |" -1)
      (dotimes (col *torus-num-cols*)
        (torus-print-entry (torus-pole-get-entry row col)))
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
      (insert (propertize string 'font-lock-face '(:foreground "white" :background "black"))))
  (if (equal color 0)
      (insert (propertize string 'font-lock-face '(:foreground "red" :background "black"))))
  (if (equal color 1)
      (insert (propertize string 'font-lock-face '(:foreground "gray" :background "black"))))
  (if (equal color 2)
      (insert (propertize string 'font-lock-face '(:foreground "yellow" :background "black"))))
  (if (equal color 3)
      (insert (propertize string 'font-lock-face '(:foreground "orange" :background "black"))))
  (if (equal color 4)
      (insert (propertize string 'font-lock-face '(:foreground "dark cyan" :background "black"))))
  )

(defun torus-print-entry (n)
  (if (eq n nil)
      (torus-print-string "     " -1))
  (if (equal n "*")
      (torus-print-string "*" -1))
  (if (eq n -1)
      (torus-print-string "  |  " -1))
  (if (eq n -2)
      (torus-print-string " --- " -1))
  (if (or (equal n 0) (equal n 1) (equal n 2) (equal n 3) (equal n 4))
      (torus-print-string " @@@ " n))
  )


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
  (elt *torus-box*
     (+ col
        (* row
           *torus-num-cols*))))

(defun torus-pole-get-entry (row col)
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
    (torus-pole-get-entry
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
                                (torus-pole-get-entry row old-pos))
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
                                (torus-pole-get-entry row old-pos))
          (torus-pole-set-entry row old-pos nil))
        (setq *torus-pole-pos* new-pos))))

;; insert and delete a torus in the pole

(defun torus-pole-insert ()
  "Insert into the pole the bottom torus in the column where the pole is at."
  (torus-pole-set-entry (- *torus-pole-height* *torus-num-tori-in-pole* 1)
                        *torus-pole-pos*
                        (torus-box-get-entry (1- *torus-box-height*) *torus-pole-pos*))
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
                       col torus)
  (torus-increase-num-tori col))

;; insert and delete a torus in the box

(defun torus-box-insert-new (col)
  "Insert a random torus in column col."
  (torus-box-set-entry (- *torus-box-height* (torus-get-num-tori col) 1)
                       col (torus-random-torus))
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
                           (torus-box-get-entry (1+ rr) col)))
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
                           (torus-box-get-entry (1- rr) col)))
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
              (torus-box-get-entry row col))))
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
              (torus-pole-get-entry row col))))
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
              (torus-pole-get-entry (1+ row) col))))
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
                  (torus-pole-get-entry rr col)))
          )))
    (setq *torus-pole* temp)
    (setq *torus-pole-height* new-ht)))


(defun torus-increase-num-cols ()
  "Increase the number of columns by 1."
  (setq *torus-num-cols* (1+ *torus-num-cols*))
  )

;; key bindings

(defvar torus-mode-map (make-sparse-keymap))
(define-key torus-mode-map (kbd "n") 'torus-start-game)
(define-key torus-mode-map (kbd "r") 'torus-resume-game)
(define-key torus-mode-map (kbd "q") 'torus-end-game)
(define-key torus-mode-map (kbd "p") 'torus-pause-game)
(define-key torus-mode-map [left] 'torus-move-left)
(define-key torus-mode-map [right] 'torus-move-right)
(define-key torus-mode-map [up] 'torus-move-up)
(define-key torus-mode-map [down] 'torus-move-down)

;; system commands


(defun torus-game-over ()
  (setq *torus-game-on* nil)
  (when *torus-timer* (cancel-timer *torus-timer*))
  (let (user-name)
    (setq user-name (read-string "Game Over!\nEnter your name: " *torus-last-user* nil nil nil))
    (setq *torus-last-user* user-name)
    (setq user-name (concat "\"" user-name "\""))
    (torus-update-user-score user-name)
    (when (string-equal major-mode "torus-mode")
      (erase-buffer)
      (insert-file-contents (concat *torus-game-path* "/torus-data/scores") nil 0 10000)
      )
    )
  (message "Press \"n\" to start a new game.")
  )


(defun torus-update-user-score (user-name)
  "Update the score of user-name in the score file."
  (let (update score d)
    (unless (file-exists-p (concat *tofus-game-path* "torus-data/"))
      (make-directory (concat *tofus-game-path* "torus-data/")))
    (find-file (format "%s/torus-data/scores" *torus-game-path*))
    (goto-char (point-min))
    (setq update nil)
    (while (not update)
      (if (equal (point) (point-max))
          (progn
            (torus-insert-score)
            (setq update t))
        (progn
          (if (<= *torus-score* (torus-read-score))
              (progn
                (next-line)
                (beginning-of-line))
            (progn
              (beginning-of-line)
              (torus-insert-score)
              (setq update t))))
          )
      )
    (save-buffer) (kill-buffer)
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
      (search-forward "," nil t nil)
      (setq start (- (point) 11))
      (setq end (1- (point)))
      (message (buffer-substring start end))
      (string-to-number (buffer-substring start end))
      )))


;; update game

(defun torus-run-game ()
  (setq *torus-game-on* 1)
  (when *torus-timer* (cancel-timer *torus-timer*))
  (setq *torus-timer* (run-at-time nil *torus-game-speed* 'torus-update)))

(defun torus-update ()
  "Update the game after given time period."
  (dotimes (col *torus-num-cols*)
    (if (= (torus-get-num-tori col) *torus-box-height*)
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
    (message "move left")))

(defun torus-move-right ()
  (interactive)
  (when *torus-game-on*
    (torus-move-pole-right)
    (torus-print-all)
    (message "move right")))

(defun torus-move-up ()
  (interactive)
  (when *torus-game-on*
    (if (and (< (torus-get-num-tori *torus-pole-pos*) *torus-box-height*)
             (> *torus-num-tori-in-pole* 0))
        (progn
          (torus-box-insert-from-pole)
          (torus-pole-delete-top)
          (torus-delete-same-rows)
          (message "move up")
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
          (message "move down")
          )
      ))
  )

(defun torus-start-game ()
  (interactive)
  (torus-pause-game)
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
