(defun nbm-org-time-stamp-string ()
  "Return an org time stamp string using input from prompt."
  (with-temp-buffer
    (org-mode)
    (org-time-stamp nil t)
    (buffer-substring (point-min) (point-max))))

(defun nbm-org-time-stamp-string-to-ISO-format (str &optional offset)
  "Convert org time stamp string to ISO 8601 format with UTC offset.
OFFSET must be of the form +09:00 or -01:00."
  (let (year month day hour minute)
    (setq year (substring str 1 5))
    (setq month (substring str 6 8))
    (setq day (substring str 9 11))
    (setq hour (substring str 9 11))
    (setq str (split-string str ":"))
    (setq hour "00")
    (setq minute "00")
    (when (> (length str) 1)
	(setq hour (substring (nth 0 str) -2 nil))
	(setq minute (substring (nth 1 str) 0 2)))
    (unless offset (setq offset "+00:00"))
    (format "%s-%s-%sT%s:%s%s" year month day hour minute offset)))

(defun nbm-time-add-hour (time hours)
  "Add HOURS to TIME. TIME must be of the form (25938 11232)."
  (list (car time) (+ (nth 1 time) (floor (* hours 3600)))))

(defun nbm-time-convert (time format &optional zone)
  "Convert TIME to FORMAT.
ZONE must be of the form \"Asia/Seoul\".
TIME must be of the form (25938 11232).
FORMAT should be one of the numbers 1,2 or 3.
1. ISO 8601: 2023-11-13T14:00+00:00 (The last +00:00 is the UTC offset.)
2. org time: [2023-11-13 Mon 14:00] or [2023-11-13 14:00]
3. readible: 02:00 PM, November 13 (Mon), 2023
4. google calendar: 20231113T140000"
  (unless zone (setq zone t))
  (cond ((equal format 1)
	 (format-time-string "%Y-%m-%dT%H:%M+00:00" time zone))
	((equal format 2)
	 (format-time-string "[%Y-%m-%d %a %H:%M]" time zone))
	((equal format 3)
	 (format-time-string "%H:%M, %B %d (%a), %Y" time zone))
	((equal format 4)
	 (format-time-string "%Y%m%dT%H%M00" time zone))
	))

(defun nbm-time-encode-time (str &optional zone)
  "Encode time with time string STR.
ZONE must be of the form \"Asia/Seoul\".
STR must be of one of the following forms.
1. ISO 8601: 2023-11-13T14:00+00:00 (The last +00:00 is the UTC offset.)
2. org time: [2023-11-13 Mon 14:00] or [2023-11-13 14:00]"
  (when (string-search "[" str)
    (setq str (nbm-org-time-stamp-string-to-ISO-format str)))
  (encode-time (append (butlast (parse-time-string str)) (list zone))))

(defun nbm-org-time-stamp-string-add-time (str hours &optional zone)
  "Add hours HOURS to org time stamp string STR."
  (let (time)
    (setq time (nbm-time-encode-time str zone))
    (setq time (nbm-time-add-hour time hours))
    (nbm-time-convert time 2 zone)))

