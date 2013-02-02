;;(defstruct foo bar baaz  )

;;(aref (make-array 10 :element-type 'foo :initial-element NIL) 1)


(defconstant board-size 7)
(defconstant white 0)
(defconstant black 1)

(defstruct stone-string (stones (make-array 4 :fill-pointer 0 :element-type 'cons :adjustable t ) ) (liberties (make-array 4 :fill-pointer 0 :element-type 'cons :adjustable t )))
(defstruct board (stone-strings (make-array (list board-size board-size) :element-type 'stone-string)) (stones (make-array (list board-size board-size) :element-type 'cons)))

(defvar neighbor-map (make-array (list board-size board-size 4)  ))

(defun init-neighbor-map () 
			     (dotimes (i board-size) (
						      dotimes (j board-size) 
						       (setf (aref neighbor-map i j 0) (if (> i 1) (cons (- i 1) j) (cons -1 -1) ) ) 
						       (setf (aref neighbor-map i j 1) (if (> j 1) (cons i (- j 1)) (cons -1 -1) ) ) 
						       (setf (aref neighbor-map i j 2) (if (< i (- board-size 1)) (cons (+ i 1) j) (cons -1 -1) ))
						       (setf (aref neighbor-map i j 3) (if (< j (- board-size 1)) (cons i (+ j 1)) (cons -1 -1) )))))
						      
			     


;;(defun new-board () (make-array board-size :element-type 'node :initial-element NIL ))

(defun add-node-to-board (board position) (
					   
))

;;(defun neighbor-positions (position) (  ))
