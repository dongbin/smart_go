(defconstant board-size 7)
(defconstant white 1)
(defconstant black 2)

(defstruct stone-string (stones (make-array 4 :fill-pointer 0 :element-type 'cons :adjustable t ) ) (liberties (make-array 4 :fill-pointer 0 :element-type 'cons :adjustable t )))
(defstruct board (stone-strings (make-array (list board-size board-size) :element-type 'stone-string :initial-element NIL )) (stones (make-array (list board-size board-size))))

(defvar neighbor-map (make-array (list board-size board-size 4)  ))

(defun init-neighbor-map () 
			     (dotimes (i board-size) (
						      dotimes (j board-size) 
						       (setf (aref neighbor-map i j 0) (if (> i 1) (cons (- i 1) j) NIL ) ) 
						       (setf (aref neighbor-map i j 1) (if (> j 1) (cons i (- j 1)) NIL ) ) 
						       (setf (aref neighbor-map i j 2) (if (< i (- board-size 1)) (cons (+ i 1) j) NIL ))
						       (setf (aref neighbor-map i j 3) (if (< j (- board-size 1)) (cons i (+ j 1)) NIL )))))
						      
(defun add-stone-on-board (board position color)
  (setf (aref (board-stones board) (car position) (cdr position)) color )
  (dolist (point (aref neighbor-map  (car position) (cdr position)))
    (if point (
	       ))
    )
)

