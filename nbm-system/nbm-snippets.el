(defun nbm-snippet-latex-matrix ()
  "Create a latex snippet for a matrix."
  (interactive)
  (insert "\\begin{pmatrix}\n")
  (nbm-snippet-insert-matrix-entries
   (read-from-minibuffer
    "Instructions: The matrix entries must be separated by a space.
For example, the 2x2 identity matrix is written as follows.
1 0
0 1
(Type C-j to create a new line.)
Enter the entries below:
"))
  (insert "\\end{pmatrix}"))

(defun nbm-snippet-latex-ytableau ()
  "Create a latex snippet for a young tableau."
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
(Type C-j to create a new line.)
Enter the entries below:
"))
  (insert "\\end{ytableau}"))

(defun nbm-snippet-insert-matrix-entries (str)
  "Parse STR and insert the result in the current buffer.
To support the ytableau package, a dot \".\" is converted
to \"\\none\"."
  (let (row col this-row)
    (setq str (split-string str "\n"))
    (setq row (length str))
    (setq col (length (car str)))
    (dotimes (i row)
      (setq this-row (split-string (nth i str)))
      (dotimes (j (length this-row))
	(unless (equal j 0) (insert " & "))
	(if (equal (nth j this-row) ".")
	    (insert "\\none")
	  (insert (nth j this-row))))
      (if (equal i (1- row))
	  (insert "\n")
	(insert "\\\\\n")))))
