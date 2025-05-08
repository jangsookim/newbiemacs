(require 'org-ref-arxiv)

(defun nbm-arxiv-download-pdf (url)
  "Download the pdf file for the paper in the URL of an arXiv abstract page."
  (let (id pdf id-without-slash)
    (setq id (biblio-arxiv--extract-id url))
    (setq id-without-slash (string-replace "/" "-" id))
    (setq pdf (biblio-arxiv--pdf-url id))
    (url-copy-file pdf (format "%s/%s.pdf" *nbm-downloads* id-without-slash) t)))

(defun nbm-arxiv-get-bibtex (url)
  "Get the bibtex file from url."
  (let (id)
    (setq id (biblio-arxiv--extract-id url))
    (arxiv-get-bibtex-entry-via-arxiv-api id)))

(defun nbm-arxiv-add-pdf-bibtex ()
  "Download a pdf and add a bibtex item for an arxiv paper.
The URL of an arxiv abstract page must be copied or
(on MacOS only) the current browser must be visiting an arxiv abstract page."
  (interactive)
  (let (id pdf url bed end)
    (if (equal system-type 'darwin)
	(nbm-get-url)
      (setq url (current-kill 0)))
    (if (string-match "arXiv" url)
	(progn
	  (setq id (biblio-arxiv--extract-id url))
	  (setq pdf (biblio-arxiv--pdf-url id))
	  (nbm-arxiv-download-pdf url)
	  (kill-new (nbm-arxiv-get-bibtex url))
	  (nbm-move-pdf-from-downloads t))
      (message "You must copy the URL of an arXiv abstract page first!"))))

