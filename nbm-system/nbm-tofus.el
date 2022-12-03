;; nbm-tofus.el -- play the tofus game in Emacs!

(defconst *tofus-game-path* (nbm-f "nbm-user-settings/"))

(defun tofus ()
  (interactive)
  (switch-to-buffer "tofus")
  (tofus-mode)
  (tofus-init)
  (evil-local-set-key 'normal [left] 'tofus-move-left)
  (evil-local-set-key 'normal [right] 'tofus-move-right)
  (evil-local-set-key 'normal [up] 'tofus-move-up)
  (evil-local-set-key 'normal [down] 'tofus-move-down)
  (evil-local-set-key 'normal (kbd "n") 'tofus-start-game)
  (evil-local-set-key 'normal (kbd "r") 'tofus-resume-game)
  (evil-local-set-key 'normal (kbd "q") 'tofus-end-game)
  (evil-local-set-key 'normal (kbd "p") 'tofus-pause-game)
  )

(defun tofus-mode ()
  (setq major-mode 'tofus-mode)
  (setq mode-name "Tofus")
  (font-lock-mode)
  (use-local-map tofus-mode-map))

(defun tofus-init ()
  "Start a new game of tofus."
  (tofus-set-difficulty)
  (setq *tofus-box-height* 20)
  (setq *tofus-pole-height* *tofus-num-cols*)
  (setq *tofus-box* (make-vector (* *tofus-box-height*
                                    *tofus-num-cols*) nil))
  (setq *tofus-pole* (make-vector (* *tofus-pole-height*
                                      *tofus-num-cols*) nil))
  (setq *tofus-num-tori* (make-vector 100 0))
  (setq *tofus-num-tori-in-pole* 0)
  (setq *tofus-score* 0)
  (setq *tofus-time* 0)
  (tofus-init-pole 0)
  (tofus-run-game)
  )

(defun tofus-get-str-difficulty ()
  "Return the difficulty in string format."
  (if (equal 1 *tofus-difficulty*)
      "Normal"
    " Crazy"))

(defun tofus-set-difficulty ()
  (interactive)
  (if (equal ?1 (read-char "Choose difficulty:\n (1) Normal\n (2) Crazy"))
      (progn
	(setq *tofus-difficulty* 1)
	(setq *tofus-level* 0)
	(setq *tofus-num-cols* 3)
	(setq *tofus-num-colors* 4)
	(setq *tofus-level-gauge* 0)
	(setq *tofus-level-up-time* 5)
	(setq *tofus-speed* 2)
	)
    (progn
      (setq *tofus-difficulty* 2)
      (setq *tofus-num-cols* 3)
      (setq *tofus-num-colors* 5)
      (setq *tofus-level* 0)
      (setq *tofus-level-gauge* 0)
      (setq *tofus-level-up-time* 5)
      (setq *tofus-speed* 2)
      )))
  
(defvar *tofus-box* nil
  "The tofus box which is a list.")

(defvar *tofus-num-tori* nil
  "The vector whose ith entry is the number of tori in column i.")

(defvar *tofus-pole* nil
  "The tofus pole which is a list.")

(defvar *tofus-speed* 3)                  ; the lower the faster

(defconst *tofus-timer* nil)
(defconst *tofus-box-height* 9)
(defconst *tofus-num-cols* 5)

(defvar *tofus-difficulty* 2)
(defvar *tofus-pole-height* 3)
(defvar *tofus-pole-pos* 0)
(defvar *tofus-num-colors* 3)
(defvar *tofus-num-tori-in-pole* 0)
(defvar *tofus-score* 0)
(defvar *tofus-level* 0)
(defvar *tofus-time* 0)
(defvar *tofus-level-up-time* 0)
(defvar *tofus-level-gauge* 0)
(defvar *tofus-game-on* 1)

;; printing

(defun tofus-print-all ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (tofus-print-box)
    (tofus-print-pole)
    (tofus-print-level-gauge)
    (tofus-print-score)
    (tofus-print-time)
    (tofus-print-level)
    (tofus-print-difficulty)
    ;; (tofus-print-num-tori)
    ;; (tofus-print-num-tori-in-pole)
    ))

(defun tofus-print-num-tori ()
  (let ((inhibit-read-only t))
    (dotimes (col *tofus-num-cols*)
      (insert (number-to-string (tofus-get-num-tori col))))
    (insert "\n")))

(defun tofus-print-num-tori-in-pole ()
  (let ((inhibit-read-only t))
    (dotimes (col *tofus-num-cols*)
      (insert (number-to-string *tofus-num-tori-in-pole*)))
      (insert "\n")))

(defun tofus-print-box ()
  (let ((inhibit-read-only t))
    (insert " |")
    (dotimes (col *tofus-num-cols*)
          (tofus-print-entry -2))
    (insert "|\n")
    (dotimes (row *tofus-box-height*)
      (insert " |")
      (dotimes (col *tofus-num-cols*)
        (tofus-print-entry (tofus-box-get-entry row col)))
      (insert "|\n"))))

(defun tofus-print-pole ()
  (let ((inhibit-read-only t))
    (dotimes (row *tofus-pole-height*)
      (insert " |")
      (dotimes (col *tofus-num-cols*)
        (tofus-print-entry (tofus-pole-get-entry row col)))
      (insert "|\n")))
  (insert " |")
  (dotimes (col *tofus-num-cols*)
    (if (= col *tofus-pole-pos*)
        (tofus-print-entry -2)
        (tofus-print-entry nil)))
  (insert "|\n"))

(defun tofus-print-entry (n)
  (if (eq n nil)
      (insert "     "))
  (if (equal n "*")
      (insert "*"))
  (if (eq n -1)
      (insert "  |  "))
  (if (eq n -2)
      (insert " --- "))
  (if (and (equal *tofus-difficulty* 2)
    (equal (% *tofus-time* 2) 0))
      (progn
        (if (eq n 0)
            (insert " @@@ "))
        (if (eq n 1)
            (insert " @@@ "))
        (if (eq n 2)
            (insert " @@@ "))
        (if (eq n 3)
            (insert " @@@ "))
        (if (eq n 4)
            (insert " @@@ "))
        )
    (progn
      (if (eq n 0)
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "red" :weight "bold"))))
      (if (eq n 1)
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "Palegreen3"))))
      (if (eq n 2)
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "yellow"))))
      (if (eq n 3)
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "orange"))))
      (if (eq n 4)
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "purple"))))
      )))

(defun tofus-print-score ()
  (insert "\n\n Score: ")
  (insert (number-to-string *tofus-score*))
  )

(defun tofus-print-time ()
  (insert "\n\n Time: ")
  (insert (number-to-string *tofus-time*))
  )

(defun tofus-print-level ()
  (insert "\n\n Level: ")
  (insert (number-to-string *tofus-level*))
  )

(defun tofus-print-difficulty ()
  (insert "\n\n Difficulty: ")
  (when (equal *tofus-difficulty* 1)
    (insert "Normal"))
  (when (equal *tofus-difficulty* 2)
    (insert " Crazy"))
  )

(defun tofus-print-level-gauge ()
  (insert "\n  ")
  (dotimes (x *tofus-level-gauge*)
    (tofus-print-entry "*"))
  (insert "\n")
  )


;; get and set entries

(defun tofus-box-set-entry (row col value)
  (aset *tofus-box*
        (+ col
           (* row
              *tofus-num-cols*))
        value))

(defun tofus-box-get-entry (row col)
  (elt *tofus-box*
     (+ col
        (* row
           *tofus-num-cols*))))

(defun tofus-pole-get-entry (row col)
  (elt *tofus-pole*
       (+ col
          (* row
             *tofus-num-cols*))))

(defun tofus-pole-set-entry (row col value)
  (aset *tofus-pole*
        (+ col
           (* row
              *tofus-num-cols*))
        value))

(defun tofus-get-num-tori (col)
  "Return the number of tori in column col."
  (elt *tofus-num-tori* col))

(defun tofus-pole-get-top-tofus ()
    (tofus-pole-get-entry
     (- *tofus-pole-height* *tofus-num-tori-in-pole*)
     *tofus-pole-pos*))

;; initiate the pole

(defun tofus-init-pole (pos)
  "Initiate the pole at column pos."
  (setq *tofus-pole-pos* pos)
  (dotimes (row *tofus-pole-height*)
    (dotimes (col *tofus-num-cols*)
      (if (= col pos)
          (tofus-pole-set-entry row col -1)
        (tofus-pole-set-entry row col nil)))))

;; moving the pole

(defun tofus-move-pole-left ()
  "Move the pole to the left."
  (if (> *tofus-pole-pos* 0)
      (let (old-pos new-pos)
        (setq old-pos *tofus-pole-pos*)
        (setq new-pos (1- *tofus-pole-pos*))
        (dotimes (row *tofus-pole-height*)
          (tofus-pole-set-entry row new-pos
                                (tofus-pole-get-entry row old-pos))
          (tofus-pole-set-entry row old-pos nil))
        (setq *tofus-pole-pos* new-pos))))

(defun tofus-move-pole-right ()
  "Move the pole to the right."
  (if (< *tofus-pole-pos* (1- *tofus-num-cols*))
      (let (old-pos new-pos)
        (setq old-pos *tofus-pole-pos*)
        (setq new-pos (1+ *tofus-pole-pos*))
        (dotimes (row *tofus-pole-height*)
          (tofus-pole-set-entry row new-pos
                                (tofus-pole-get-entry row old-pos))
          (tofus-pole-set-entry row old-pos nil))
        (setq *tofus-pole-pos* new-pos))))

(defun tofus-pole-insert ()
  "Insert into the pole the bottom tofus in the column where the pole is at."
  (tofus-pole-set-entry (- *tofus-pole-height* *tofus-num-tori-in-pole* 1)
                        *tofus-pole-pos*
                        (tofus-box-get-entry (1- *tofus-box-height*) *tofus-pole-pos*))
  (tofus-increase-num-tori-in-pole)
  )

(defun tofus-pole-delete-top ()
  "Delete the top tofus."
  (tofus-pole-set-entry (- *tofus-pole-height* *tofus-num-tori-in-pole*)
                        *tofus-pole-pos* -1)
  (tofus-decrease-num-tori-in-pole))

(defun tofus-box-insert-new (col)
  "Insert a random tofus in column col."
  (tofus-box-set-entry (- *tofus-box-height* (tofus-get-num-tori col) 1)
                       col (tofus-random-tofus))
  (tofus-increase-num-tori col)
  )

(defun tofus-box-insert-row ()
  "Insert a new tofus in each column."
  (dotimes (col *tofus-num-cols*)
    (tofus-box-insert-new col))
  )

(defun tofus-box-insert-from-pole ()
  "Insert the top tofus of the pole to the box."
  (let (rr k col)
    (setq col *tofus-pole-pos*)
    (setq k (tofus-get-num-tori col)) ; k is the number of boxes to modify
    (dotimes (r k)
      (setq rr (+ (- *tofus-box-height* k 1) r))
      (tofus-box-set-entry rr col
                           (tofus-box-get-entry (1+ rr) col)))
    (setq rr (1- *tofus-box-height*))
    (tofus-box-set-entry rr col (tofus-pole-get-top-tofus))
    (tofus-increase-num-tori col))
  )

(defun tofus-box-remove-tofus (row col)
  "Remove the tofus at given position."
  (let (rr r k)
    (setq k (- (+ (1+ row) (tofus-get-num-tori col)) *tofus-box-height*)) ; k is the number of boxes to modify
    (dotimes (r (1- k))
      (setq rr (- row r))
      (tofus-box-set-entry rr col
                           (tofus-box-get-entry (1- rr) col)))
    (setq rr (- row (1- k)))
    (tofus-box-set-entry rr col nil)
    (tofus-decrease-num-tori col)
    ))

(defun tofus-remove-bottom (col)
  "Remove the bottom tofus in column col."
  (tofus-box-remove-tofus (1- *tofus-box-height*) col))

(defun tofus-increase-num-tori (col)
  "Increase the number of tori in column col."
  (aset *tofus-num-tori* col (1+ (elt *tofus-num-tori* col))))

(defun tofus-decrease-num-tori (col)
  "Decrease the number of tori in column col."
  (aset *tofus-num-tori* col (1- (elt *tofus-num-tori* col))))

(defun tofus-increase-num-tori-in-pole ()
  "Increase the number of tori in pole in column col."
  (setq *tofus-num-tori-in-pole* (1+ *tofus-num-tori-in-pole*)))

(defun tofus-decrease-num-tori-in-pole ()
  "Decrease the number of tori in pole in column col."
  (setq *tofus-num-tori-in-pole* (1- *tofus-num-tori-in-pole*)))

(defun tofus-random-tofus ()
  "Create a random tofus as a number."
  (random *tofus-num-colors*))


;; check rows

(defun tofus-check-row (row)
  "Check whether the row has the same tori."
  (let (same first)
    (setq first (tofus-box-get-entry row 0))
    (if first (setq same 1) (setq same 0))
    (dotimes (c (1- *tofus-num-cols*))
      (when (eq first (tofus-box-get-entry row (1+ c)))
        (setq same (1+ same))))
    (if (eq same *tofus-num-cols*)
        t nil)))

(defun tofus-delete-row (row)
  "Delete the row in the box."
  (dotimes (col *tofus-num-cols*)
    (tofus-box-remove-tofus row col)))

(defun tofus-delete-same-rows ()
  "Delete all rows with the same tofus."
  (dotimes (row *tofus-box-height*)
    (when (tofus-check-row row)
      (tofus-increase-score)
      (tofus-delete-row row)
      )))

; Score scale: 3,4,5,6,7 columns give 100,200,300,400,500 scaling factors.
(defun tofus-increase-score ()
  "Increase the score."
  (setq *tofus-score* (+ *tofus-score*
			 (* 100 (- *tofus-num-cols* 2)
			    (expt 2 *tofus-difficulty*)))))

(defun tofus-increase-time ()
  "Increase the time."
  (setq *tofus-level-gauge* (1+ *tofus-level-gauge*))
  (setq *tofus-time* (1+ *tofus-time*)))

(defun tofus-increase-level ()
  "Increase the level."
  (setq *tofus-level* (1+ *tofus-level*))
  (if (> *tofus-pole-height* 2)
      (tofus-decrease-pole-height)
    (progn
      (tofus-increase-box)
      (tofus-increase-pole)
      (tofus-increase-num-cols)
      (tofus-increase-pole-height)
      )))

(defun tofus-increase-box ()
  "Increase the box by adding one more column."
  (let (temp)
    (setq temp (make-vector (* *tofus-box-height*
                               (1+ *tofus-num-cols*)) nil))
    (dotimes (row *tofus-box-height*)
      (dotimes (col *tofus-num-cols*)
        (aset temp (+ col
                      (* row (1+ *tofus-num-cols*)))
              (tofus-box-get-entry row col))))
    (setq *tofus-box* temp)))

(defun tofus-increase-pole ()
  "Increase the pole by adding one more column."
  (let (temp)
    (setq temp (make-vector (* *tofus-pole-height*
                               (1+ *tofus-num-cols*)) nil))
    (dotimes (row *tofus-pole-height*)
      (dotimes (col *tofus-num-cols*)
        (aset temp (+ col
                      (* row (1+ *tofus-num-cols*)))
              (tofus-pole-get-entry row col))))
    (setq *tofus-pole* temp)))

(defun tofus-decrease-pole-height ()
  "Decrease the pole height."
  (if (equal *tofus-num-tori-in-pole* *tofus-pole-height*)
      (if (equal (tofus-get-num-tori *tofus-pole-pos*) *tofus-box-height*)
          (tofus-game-over)
        (tofus-move-up)))
  (setq *tofus-pole-height* (1- *tofus-pole-height*))
  (let (temp)
    (setq temp (make-vector (* *tofus-pole-height* *tofus-num-cols*) nil))
    (dotimes (row *tofus-pole-height*)
      (dotimes (col *tofus-num-cols*)
        (aset temp (+ col (* row *tofus-num-cols*))
              (tofus-pole-get-entry (1+ row) col))))
    (setq *tofus-pole* temp)))

(defun tofus-increase-pole-height ()
  "Increase the pole height to the number of columns."
  (let (temp rr new-ht old-ht)
    (setq old-ht *tofus-pole-height*)
    (setq new-ht *tofus-num-cols*)
    (setq temp (make-vector (* new-ht *tofus-num-cols*) nil))
    (dotimes (row new-ht)
      (dotimes (col *tofus-num-cols*)
        (if (< row (- new-ht old-ht))
            (when (equal col *tofus-pole-pos*)
              (aset temp (+ col (* row *tofus-num-cols*)) -1))
          (progn
            (setq rr (- row (- new-ht old-ht)))
            (aset temp (+ col (* row *tofus-num-cols*))
                  (tofus-pole-get-entry rr col)))
          )))
    (setq *tofus-pole* temp)
    (setq *tofus-pole-height* new-ht)))


(defun tofus-increase-num-cols ()
  "Increase the number of columns by 1."
  (setq *tofus-num-cols* (1+ *tofus-num-cols*))
  )

;; key bindings

(defvar tofus-mode-map (make-sparse-keymap))
(define-key tofus-mode-map (kbd "n") 'tofus-start-game)
(define-key tofus-mode-map (kbd "r") 'tofus-resume-game)
(define-key tofus-mode-map (kbd "q") 'tofus-end-game)
(define-key tofus-mode-map (kbd "p") 'tofus-pause-game)
(define-key tofus-mode-map [left] 'tofus-move-left)
(define-key tofus-mode-map [right] 'tofus-move-right)
(define-key tofus-mode-map [up] 'tofus-move-up)
(define-key tofus-mode-map [down] 'tofus-move-down)

;; system commands


(defun tofus-game-over ()
  (setq *tofus-game-on* nil)
  (when *tofus-timer* (cancel-timer *tofus-timer*))
  (let (user-name)
    (setq user-name (read-string "Game Over!\nEnter your name: " nil nil nil nil))
    (setq user-name (concat "\"" user-name "\""))
    (tofus-update-user-score user-name)
    (tofus-show-score-board)
    )
  (message "Press \"n\" to start a new game.")
  )

(defun tofus-show-score-board ()
  (when (string-equal major-mode "tofus-mode")
    (erase-buffer)
    (insert-file-contents (concat *tofus-game-path* "tofus-data/scores") nil 0 10000)))


(defun tofus-update-user-score (user-name)
  "Update the score of user-name in the score file."
  (let (update score d)
    (unless (file-exists-p (concat *tofus-game-path* "tofus-data/"))
      (make-directory (concat *tofus-game-path* "tofus-data/")))
    (find-file (format "%s/tofus-data/scores" *tofus-game-path*))
    (goto-char (point-min))
    (setq update nil)
    (while (not update)
      (if (equal (point) (point-max))
          (progn
            (tofus-insert-score)
            (setq update t))
        (progn
          (if (<= *tofus-score* (tofus-read-score))
              (progn
                (next-line)
                (beginning-of-line))
            (progn
              (beginning-of-line)
              (tofus-insert-score)
              (setq update t))))
          )
      )
    (save-buffer) (kill-buffer)
    ))


(defun tofus-insert-score ()
  (insert (format "%20s: Score %10d, Difficulty: %s, Level: %3d, Date: %s"
                  user-name *tofus-score* (tofus-get-str-difficulty) 
		  *tofus-level*
                  (format-time-string "%Y/%m/%d/%T\n"))))

(defun tofus-read-score ()
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

(defun tofus-run-game ()
  (setq *tofus-game-on* 1)
  (when *tofus-timer* (cancel-timer *tofus-timer*))
  (setq *tofus-timer* (run-at-time nil *tofus-speed* 'tofus-update)))

(defun tofus-update ()
  "Update the game after give time period."
  (dotimes (col *tofus-num-cols*)
    (if (= (tofus-get-num-tori col) *tofus-box-height*)
        (tofus-game-over)))
  (unless (string-equal major-mode "tofus-mode")
    (tofus-pause-game))

  (when *tofus-game-on*
    (tofus-box-insert-row)
    (tofus-delete-same-rows)
    (tofus-increase-time)
    (when (eq *tofus-level-gauge* (* *tofus-level-up-time* *tofus-num-cols*))
      (tofus-increase-level)
      (setq *tofus-level-gauge* 0))
    (tofus-print-all)))

;; movements

(defun tofus-move-left ()
  (interactive)
  (when *tofus-game-on*
    (tofus-move-pole-left)
    (tofus-print-all)
    (message "move left")))

(defun tofus-move-right ()
  (interactive)
  (when *tofus-game-on*
    (tofus-move-pole-right)
    (tofus-print-all)
    (message "move right")))

(defun tofus-move-up ()
  (interactive)
  (when *tofus-game-on*
    (if (and (< (tofus-get-num-tori *tofus-pole-pos*) *tofus-box-height*)
             (> *tofus-num-tori-in-pole* 0))
        (progn
          (tofus-box-insert-from-pole)
          (tofus-pole-delete-top)
          (tofus-delete-same-rows)
          (message "move up")
          (tofus-print-all))))
  )

(defun tofus-move-down ()
  (interactive)
  (when *tofus-game-on*
    (if (and (> (tofus-get-num-tori *tofus-pole-pos*) 0)
             (< *tofus-num-tori-in-pole* *tofus-pole-height*))
        (progn
          (tofus-pole-insert)
          (tofus-remove-bottom *tofus-pole-pos*)
          (tofus-delete-same-rows)
          (tofus-print-all)
          (message "move down")
          )
      ))
  )

(defun tofus-start-game ()
  (interactive)
  (tofus-pause-game)
  (tofus-init)
  (message "Game started."))

(defun tofus-resume-game ()
  (interactive)
  (unless *tofus-game-on*
    (tofus-run-game)
    (message "Game resumed.")))

(defun tofus-pause-game ()
  (interactive)
  (setq *tofus-game-on* nil)
  (cancel-timer *tofus-timer*)
  (message "Game paused."))

(defun tofus-end-game ()
  (interactive)
  (when *tofus-timer* (cancel-timer *tofus-timer*))
  (kill-buffer))


