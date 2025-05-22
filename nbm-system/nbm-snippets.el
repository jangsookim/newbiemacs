(defun nbm-snippet-latex-matrix ()
  "Insert a latex snippet for a matrix."
  (interactive)
  (nbm-snippet-latex-matrix-bracket (read-char "Choose the style for a matrix:
b) bmatrix (default)
p) pmatrix
v) matrix enclosed by vertical lines")))

(defun nbm-snippet-latex-matrix-bracket (&optional bracket)
  "Insert a latex snippet for a matrix using the matrix environment with BRACKET.
BRACKET can be a character equal to p, b or v.
b: bmatrix (default)
p: pmatrix
v: matrix enclosed by vertical lines"
  (interactive)
  (unless (texmathp)
    (insert "\\[\n\n\\]\n")
    (backward-char 4))
  (cond ((equal bracket ?p)
	 (insert "\\begin{pmatrix}\n"))
	((equal bracket ?v)
	 (insert "\\left|\n\\begin{matrix}\n"))
	(t
	 (insert "\\begin{bmatrix}\n")))
  (nbm-snippet-insert-matrix-entries
   (read-from-minibuffer
    "Instructions: The matrix entries must be separated by a space.
For example, the 2x2 identity matrix is written as follows.
1 0
0 1
(Type M-j to create a new line.)
Enter the entries below:
"))
  (cond ((equal bracket ?p)
	 (insert "\\end{pmatrix}"))
	((equal bracket ?v)
	 (insert "\\end{matrix}\n\\right|"))
	(t
	 (insert "\\end{bmatrix}"))))

(defun nbm-snippet-latex-ytableau ()
  "Insert a latex snippet for a young tableau."
  (interactive)
  (insert "\\begin{ytableau}\n")
  (nbm-snippet-insert-matrix-entries
   (read-from-minibuffer
    "Instructions: The entries must be separated by a space.
Each empty cell must be written using a dot.
For example, the following is an input for a Young tableau
of shape (4,3,1)/(2,1).
. . 1 3
. 1 2
2
(Type M-j to create a new line.)
Enter the entries below:
"))
  (insert "\\end{ytableau}\n"))

(defun nbm-snippet-insert-matrix-entries (str)
  "Parse STR and insert the result in the current buffer.
To support the ytableau package, a dot \".\" is converted
to \"{}\"."
  (let (row col this-row)
    (setq str (split-string str "\n"))
    (setq row (length str))
    (setq col (length (car str)))
    (dotimes (i row)
      (setq this-row (split-string (nth i str)))
      (dotimes (j (length this-row))
	(unless (equal j 0) (insert " & "))
	(if (equal (nth j this-row) ".")
	    (insert "{}")
	  (insert (nth j this-row))))
      (if (equal i (1- row))
	  (insert "\n")
	(insert "\\\\\n")))))
