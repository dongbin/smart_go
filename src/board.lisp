(defconstant board-size 3)

(defconstant empty 0)
(defconstant white 1)
(defconstant black 2)

(defstruct stone-string color  (stones '()) (liberties '() ))
(defstruct board (stone-strings (make-array (list board-size board-size) :element-type 'stone-string :initial-element NIL )) (stones (make-array (list board-size board-size))))

(defvar neighbor-map (make-array (list board-size board-size) :element-type 'list :initial-element '()  ))

(defun init-neighbor-map () 
  (dotimes (i board-size) (
			   dotimes (j board-size) 
			    (if (> i 0) (push (cons (- i 1) j) (aref neighbor-map i j)))
			    (if (> j 0) (push (cons i (- j 1) ) (aref neighbor-map i j)))
			    (if (< i (- board-size 1))  (push (cons (+ i 1) j)  (aref neighbor-map i j)))
			    (if (< j (- board-size 1)) (push (cons i (+ j 1) )  (aref neighbor-map i j)))
			    )))

(defun add-stone-on-board (board position color)
  (and (position-available? board position color)
  (let ( (x  (car position)) (y (cdr position))  )
    (setf (aref (board-stones board) x y) color )
    (dolist (point (aref neighbor-map  x y))
      
      (cond ((= (aref (board-stones board) (car point) (cdr point) )  color) (append-stone-string board (aref (board-stone-strings board) (car point) (cdr point) ) x y ) ))
      (if (not (aref (board-stone-strings board) x y)) (create-stone-string board position color) )
      
      ))))


(defun append-stone-string (board neighbor-string x y)
  (let ( (current-string (aref (board-stone-strings board) x y))) 
    (if   (and current-string neighbor-string)
	  ( merge-stone-strings neighbor-string current-string board)
	  (stone-string-push board neighbor-string x y )
	  )
    )
  )

(defun stone-string-push (board string x y)
  (push (cons x y) (stone-string-stones  string)  )
  ( setf (stone-string-liberties  string) (union (stone-string-liberties  string) (search-liberties  board  (cons x y) ) :test #'equal))
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
					   mapcan (lambda (point) (if (= (aref (board-stones board) (car point) (cdr point)) 0) (list point) nil))  (aref neighbor-map  (car position) (cdr position))))



(defun create-stone-string (board position color) 
  (setf (aref (board-stone-strings board) (car position) (cdr position)) (make-stone-string  :color color :stones  (list position)  :liberties (search-liberties board position)  )  )
  )
 (defun position-available? (board position color)
   (and (= empty (aref (board-stones board) (car position) (cdr position)))
   (or (search-liberties board position) 
    (member-if (lambda (point) (and (= (aref (board-stones board) (car point) (cdr point) )  (enemy-color color) ) (= 1 (list-length (aref (board-stone-strings board) (car point) (cdr point))))
  )) (aref neighbor-map  (car position) (cdr position))))))

(defun enemy-color (color) (if (= black color) white  black ))
(init-neighbor-map)
