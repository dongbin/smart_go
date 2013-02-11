(defconstant board-size 7)
(defconstant white 1)
(defconstant black 2)

(defstruct stone-string color  (stones '()) (liberties '() ))
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
  (let ( (x  (car position)) (y (cdr position))  )
    (setf (aref (board-stones board) x y) color )
    (dolist (point (aref neighbor-map  x y))
      ()
      (if point (progn 
		  (let ( (neighbor-string (aref (board-stones board) (car point) (cdr point) ) ) )
		     (cond ((= neighbor-string  color) (append-stone-string board neighbor-string x y ) )))
			(if (not (aref (board-stone-strings board) x y)) (create-stone-string board position color) )
			
			)))))


(defun append-stone-string (board neighbor-string x y)
  (if (aref (board-stone-strings board) x y)  ( merge-stone-strings neighbor-string (aref (board-stone-strings board) x y) board)
											 
	 								       )
)

(defun stone-string-push (board string x y)
  (push (list x y) (stone-string-stones  string)  )
  ( setf (stone-string-liberties  string) (union (stone-string-liberties  string) (search-liberties  board  (list x y) ) :test #'equal))
  (setf (aref (board-stone-strings board) x y ) string)
)

  (defun merge-stone-strings (string string2 board) 
    
    ( setf (stone-string-stones  string) (append (stone-string-stones  string) (stone-string-stones  string2)))
    ( setf (stone-string-liberties  string) (union (stone-string-liberties  string) (stone-string-liberties  string2) :test #'equal))
    (dolist (position (stone-string-stones  string2))
      (setf (aref (board-stone-strings board) (car position) (cdr position)) string)
      )
    )

  (defun search-liberties  (board position) (
					     mapcan (lambda (point) (if (aref (board-stones board) (car point) (cdr point)) (list point) nil))  (aref neighbor-map  (car position) (cdr position))))



  (defun create-stone-string (board position color) 
    (setf (aref (board-stone-strings board) (car position) (cdr position)) (make-stone-string  :color color :stones  (make-array 4 :element-type 'cons :adjustable t :initial-contents (list position)  ) :liberties (search-liberties board position)  )  )
    )

;;  (defun position-available? (board position color)
;;    ()
;;    )
