(defun xah-asciify-text (&optional Begin End)
  "Remove accents in some letters. e.g. café → cafe.
Change European language characters into equivalent ASCII ones.
When called interactively, work on current line or text selection.

URL `http://xahlee.info/emacs/emacs/emacs_zap_gremlins.html'
Version 2018-11-12 2021-09-17"
  (interactive)
  (let (($charMap
          [
           ["ß" "ss"]
           ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
           ["æ" "ae"]
           ["ç\\|č\\|ć" "c"]
           ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
           ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
           ["ñ\\|ň\\|ń" "n"]
           ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ\\|ő" "o"]
           ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự\\|ů"     "u"]
           ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
           ["þ" "th"]
           ["ď\\|ð\\|đ" "d"]
           ["ĩ" "i"]
           ["ľ\\|ĺ\\|ł" "l"]
           ["ř\\|ŕ" "r"]
           ["š\\|ś" "s"]
           ["ť" "t"]
           ["ž\\|ź\\|ż" "z"]
           [" " " "]       ; thin space etc
           ["–" "-"]       ; dash
           ["—\\|一" "--"] ; em dash etc
           ])
         ($p1 (if Begin Begin
                (if (region-active-p)
                    (region-beginning)
                  (line-beginning-position))))
         ($p2 (if End End
                (if (region-active-p)
                    (region-end)
                  (line-end-position)))))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $p1 $p2)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (re-search-forward (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

(defun xah-asciify-string (String)
  "Returns a new string. e.g. café → cafe.
See `xah-asciify-text'
Version 2015-06-08"
  (with-temp-buffer
      (insert String)
      (xah-asciify-text (point-min) (point-max))
      (buffer-string)))
