;; my_torus.el -- play the torus game in Emacs!

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
  (torus-set-difficulty)
  (setq *torus-box-height* 20)
  (setq *torus-pole-height* *torus-num-cols*)
  (setq *torus-box* (make-vector (* *torus-box-height*
                                    *torus-num-cols*) nil))
  (setq *torus-pole* (make-vector (* *torus-pole-height*
                                      *torus-num-cols*) nil))
  (setq *torus-num-tori* (make-vector 100 0))
  (setq *torus-num-tori-in-pole* 0)
  (setq *torus-score* 0)
  (setq *torus-time* 0)
  (torus-init-pole 0)
  (torus-run-game)
  )

(defun torus-get-str-difficulty ()
  "Return the difficulty in string format."
  (let (d)
    (when (equal 1 *torus-difficulty*) (setq d "Easy       "))
    (when (equal 2 *torus-difficulty*) (setq d "Normal     "))
    (when (equal 3 *torus-difficulty*) (setq d "Hard       "))
    (when (equal 4 *torus-difficulty*) (setq d "Impossible "))
    d))

(defun torus-set-difficulty ()
  (interactive)
  (let (choice)
    (setq choice
          (read-char "Choose difficulty:\n (1) Easy\n (2) Normal\n (3) Hard\n (4) Impossible"))
    (when t
      (setq *torus-difficulty* 1)
      (setq *torus-level* 0)
      (setq *torus-num-cols* 3)
      (setq *torus-num-colors* 3)
      (setq *torus-level-gauge* 0)
      (setq *torus-level-up-time* 5)
      (setq *torus-speed* 5)
      )
    (when (equal choice ?2)
      (setq *torus-difficulty* 2)
      (setq *torus-level* 0)
      (setq *torus-num-cols* 3)
      (setq *torus-num-colors* 3)
      (setq *torus-level-gauge* 0)
      (setq *torus-level-up-time* 5)
      (setq *torus-speed* 3)
      )
    (when (equal choice ?3)
      (setq *torus-difficulty* 3)
      (setq *torus-level* 0)
      (setq *torus-num-cols* 3)
      (setq *torus-num-colors* 4)
      (setq *torus-level-gauge* 0)
      (setq *torus-level-up-time* 5)
      (setq *torus-speed* 2)
      )
    (when (equal choice ?4)
      (setq *torus-difficulty* 4)
      (setq *torus-num-cols* 3)
      (setq *torus-num-colors* 5)
      (setq *torus-level* 0)
      (setq *torus-level-gauge* 0)
      (setq *torus-level-up-time* 5)
      (setq *torus-speed* 2)
      )
    )
  )


(defvar *torus-box* nil
  "The torus box which is a list.")

(defvar *torus-num-tori* nil
  "The vector whose ith entry is the number of tori in column i.")

(defvar *torus-pole* nil
  "The torus pole which is a list.")

(defvar *torus-speed* 3)                  ; the lower the faster

(defconst *torus-timer* nil)
(defconst *torus-box-height* 9)
(defconst *torus-num-cols* 5)

(defvar *torus-difficulty* 2)
(defvar *torus-pole-height* 3)
(defvar *torus-pole-pos* 0)
(defvar *torus-num-colors* 3)
(defvar *torus-num-tori-in-pole* 0)
(defvar *torus-score* 0)
(defvar *torus-level* 0)
(defvar *torus-time* 0)
(defvar *torus-level-up-time* 0)
(defvar *torus-level-gauge* 0)
(defvar *torus-game-on* 1)

;; printing

(defun torus-print-all ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (torus-print-box)
    (torus-print-pole)
    (torus-print-level-gauge)
    (torus-print-score)
    (torus-print-time)
    (torus-print-level)
    (torus-print-difficulty)
    ;; (torus-print-num-tori)
    ;; (torus-print-num-tori-in-pole)
    ))

(defun torus-print-num-tori ()
  (let ((inhibit-read-only t))
    (dotimes (col *torus-num-cols*)
      (insert (number-to-string (torus-get-num-tori col))))
    (insert "\n")))

(defun torus-print-num-tori-in-pole ()
  (let ((inhibit-read-only t))
    (dotimes (col *torus-num-cols*)
      (insert (number-to-string *torus-num-tori-in-pole*)))
      (insert "\n")))

(defun torus-print-box ()
  (let ((inhibit-read-only t))
    (insert " |")
    (dotimes (col *torus-num-cols*)
          (torus-print-entry -2))
    (insert "|\n")
    (dotimes (row *torus-box-height*)
      (insert " |")
      (dotimes (col *torus-num-cols*)
        (torus-print-entry (torus-box-get-entry row col)))
      (insert "|\n"))))

(defun torus-print-pole ()
  (let ((inhibit-read-only t))
    (dotimes (row *torus-pole-height*)
      (insert " |")
      (dotimes (col *torus-num-cols*)
        (torus-print-entry (torus-pole-get-entry row col)))
      (insert "|\n")))
  (insert " |")
  (dotimes (col *torus-num-cols*)
    (if (= col *torus-pole-pos*)
        (torus-print-entry -2)
        (torus-print-entry nil)))
  (insert "|\n"))

(defun torus-print-entry (n)
  (if (eq n nil)
      (insert "     "))
  (if (equal n "*")
      (insert "*"))
  (if (eq n -1)
      (insert "  |  "))
  (if (eq n -2)
      (insert " --- "))
  (if (and (equal *torus-difficulty* 4)
    (equal (% *torus-time* 2) 0))
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
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "red"))))
      (if (eq n 1)
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "Palegreen3"))))
      (if (eq n 2)
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "yellow"))))
      (if (eq n 3)
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "orange"))))
      (if (eq n 4)
          (insert (propertize " @@@ " 'font-lock-face '(:foreground "purple"))))
      )))

(defun torus-print-score ()
  (insert "\n\n Score: ")
  (insert (number-to-string *torus-score*))
  )

(defun torus-print-time ()
  (insert "\n\n Time: ")
  (insert (number-to-string *torus-time*))
  )

(defun torus-print-level ()
  (insert "\n\n Level: ")
  (insert (number-to-string *torus-level*))
  )

(defun torus-print-difficulty ()
  (insert "\n\n Difficulty: ")
  (when (equal *torus-difficulty* 1)
    (insert "Easy"))
  (when (equal *torus-difficulty* 2)
    (insert "Normal"))
  (when (equal *torus-difficulty* 3)
    (insert "Hard"))
  (when (equal *torus-difficulty* 4)
    (insert "Impossible"))
  )

(defun torus-print-level-gauge ()
  (insert "\n  ")
  (dotimes (x *torus-level-gauge*)
    (torus-print-entry "*"))
  (insert "\n")
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


;; check rows

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

(defun torus-increase-score ()
  "Increase the score."
  (setq *torus-score* (+ *torus-score* (* 100 *torus-num-cols* *torus-difficulty*))))

(defun torus-increase-time ()
  "Increase the time."
  (setq *torus-level-gauge* (1+ *torus-level-gauge*))
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
    (setq user-name (read-string "Game Over!\nEnter your name: " nil nil nil nil))
    (setq user-name (concat "\"" user-name "\""))
    (torus-update-user-score user-name)
    (when (string-equal major-mode "torus-mode")
      (erase-buffer)
      (insert-file-contents (concat *torus-game-path* "torus-data/scores") nil 0 10000)
      )
    )
  (message "Press \"n\" to start a new game.")
  )


(defun torus-update-user-score (user-name)
  "Update the score of user-name in the score file if new record is set."
  (let (update score d)
    (unless (file-exists-p (concat *torus-game-path* "torus-data/"))
      (make-directory (concat *torus-game-path* "torus-data/")))
    (find-file (format "%storus-data/scores" *torus-game-path*))
    (goto-char (point-min))
    (setq update nil)
    (if (search-forward user-name nil t nil)
        (progn
          (when (< (torus-read-score) *torus-score*) ; update if new record
            (beginning-of-line) (kill-line) (kill-line)
            (setq update t)
            )
          )
      (setq update t))                  ; update if new user
    (when update
      (goto-char (point-min))
      (insert (format "%20s: Score %10d, Difficulty: %s, Time: %5d, Level: %3d, Date: %s"
                      user-name *torus-score* (torus-get-str-difficulty) *torus-time* *torus-level*
                      (format-time-string "%Y/%m/%d/%T\n")))
      )
    (save-buffer) (kill-buffer)
    ))


(defun torus-read-score ()
  "Return the number appearing after the current position in the current buffer."
  (let (start end)
    (save-excursion
      (setq start (+ 8 (point)))
      (search-forward "," nil t nil)
      (setq end (1- (point)))
      (message (buffer-substring start end))
      (string-to-number (buffer-substring start end))
      )))


;; update game

(defun torus-run-game ()
  (setq *torus-game-on* 1)
  (when *torus-timer* (cancel-timer *torus-timer*))
  (setq *torus-timer* (run-at-time nil *torus-speed* 'torus-update)))

(defun torus-update ()
  "Update the game after give time period."
  (dotimes (col *torus-num-cols*)
    (if (= (torus-get-num-tori col) *torus-box-height*)
        (torus-game-over)))
  (unless (string-equal major-mode "torus-mode")
    (torus-pause-game))

  (when *torus-game-on*
    (torus-box-insert-row)
    (torus-delete-same-rows)
    (torus-increase-time)
    (when (eq *torus-level-gauge* (* *torus-level-up-time* *torus-num-cols*))
      (torus-increase-level)
      (setq *torus-level-gauge* 0))
    (torus-print-all)))

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
  (message "Game started."))

(defun torus-resume-game ()
  (interactive)
  (unless *torus-game-on*
    (torus-run-game)
    (message "Game resumed.")))

(defun torus-pause-game ()
  (interactive)
  (setq *torus-game-on* nil)
  (cancel-timer *torus-timer*)
  (message "Game paused."))

(defun torus-end-game ()
  (interactive)
  (when *torus-timer* (cancel-timer *torus-timer*))
  (kill-buffer))


