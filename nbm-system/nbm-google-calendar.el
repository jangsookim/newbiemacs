(defun nbm-google-calendar-add-event ()
  "Add an event in google calendar."
  (interactive)
  (let (title time-from time-to duration details)
    (setq title (read-string "Enter a title for a google calendar event: "))
    (setq time-from (nbm-org-time-stamp-string))
    (if (string-match time-from ":")
	(progn
	  (setq duration (read-number "For how long in hours? " 1))
	  (setq time-to (nbm-org-time-stamp-string-add-time time-from duration t)))
      (setq time-to (nbm-org-time-stamp-string)))
    (when (org-time> time-from time-to)
      (setq time-to time-from))
    (setq details (read-string "Enter details: "))
    (nbm-add-event-to-calendar title time-from time-to details)
    (nbm-google-calendar-add-event-run title time-from time-to details)))

(defun nbm-google-calendar-add-event-run (title time-from time-to details)
  "Open browser to add an event in google calendar.
TIME-FROM and TIME-TO must be in org time format."
  (let (all-day)
    (unless (string-match time-from ":")
      (setq all-day t))
    (setq title (string-replace " " "+" title))
    (setq title (string-replace "(" "[" title))
    (setq title (string-replace ")" "]" title))
    (setq details (string-replace " " "+" details))
    (setq details (string-replace "(" "[" details))
    (setq details (string-replace ")" "]" details))
    (setq details (string-replace "&" "\\&" details))
    (setq time-from (nbm-time-convert (nbm-time-encode-time time-from t) 4))
    (setq time-to (nbm-time-convert (nbm-time-encode-time time-to t) 4))
    (when all-day
      (setq time-from (substring time-from 0 8))
      (setq time-to (nbm-org-time-stamp-string-add-time time-to 24 t))
      (setq time-to (nbm-time-convert (nbm-time-encode-time time-to t) 4))
      (setq time-to (substring time-to 0 8)))
    (browse-url (string-replace " " "+"
				(format "https://calendar.google.com/calendar/u/1/r/eventedit?text=%s&dates=%s/%s&details=%s"
					title time-from time-to details)))))

(defun nbm-add-event-to-calendar (title time-from time-to details)
  "Add an event in calendar.org.
TIME-FROM and TIME-TO must be in org time format."
  (unless (file-exists-p (nbm-f "org/calendar.org"))
    (copy-file "~/nbm-root/newbiemacs/org/calendar.org" (nbm-f "org/calendar.org"))
    (find-file (nbm-f "org/calendar.org"))
    (nbm-org-agenda-add))
  (find-file (nbm-f "org/calendar.org"))
  (end-of-buffer)
  (setq time-from (string-replace "[" "<" time-from))
  (setq time-from (string-replace "]" ">" time-from))
  (setq time-to (string-replace "[" "<" time-to))
  (setq time-to (string-replace "]" ">" time-to))
  (insert (format "\n** %s\n%s--%s\n%s" title time-from time-to details))
  (save-buffer) (kill-buffer))
