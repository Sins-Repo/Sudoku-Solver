(defun neighbors-in-row (row column grid)
	(let* ((neighbors '()))								;neighbors is an empty list
		(dotimes (i 9 neighbors)						;repeat 0-8 times, return neighbors
			(let 										;grid is the 2D array while row and i are dimensions
				((x (aref grid row i)))					;retrieve element from array grid and assign it to x	
				(unless (or (eql '_ x) (= i column)))	;only evaluate its body if the condition is false
														;when x not equal to _ or i not equal to column
				(push x neighbors)						;push x into list neighbors
			)
		)
	)
)
 
(defun neighbors-in-column (row column grid)
	(let* ((neighbors '()))
		(dotimes (i 9 neighbors)
			(let 
				((x (aref grid i column)))				
				(unless (or (eql x '_) (= i row))
				(push x neighbors))
			)
		)
	)
)

(defun neighbors-in-square (row column grid &aux (neighbors '()))
	(let* ((rmin (* 3 (floor row 3)))  				;row is divided by 3 and go through floor function  
													;multiply the outcome by 3 then assign it to rmin (lower bound)
			(rmax (+ rmin 3))						;rmin plus 3 then assign it to rmax (upper bound)
			(cmin (* 3 (floor column 3))) 			;column divided by 3 and go through floor function
													;multiply it by 3 then assign it to cmin (lower bound)
			(cmax (+ cmin 3))						;cmin plus 3 then assign it to cmax (upper bound)
		  )

		(do ((r rmin (1+ r))) 						;variable r, having rmin value, updated with the increment of r 
													; 1+ function means increment r by 1
			((= r rmax) neighbors)					;conditional checking :return neighbors when r equal to rmax

				(do 
					((c cmin (1+ c))) 				;variable c, having cmin value, updated with the increment of c
													; 1+ function means increment c by 1
					((= c cmax))					;conditional checking: once c=cmax, statements below (s-expression) won't be exceuted
						(let ((x (aref grid r c)))  ;set x to the retrieved element of array grid
													;we have 2 param r c bcoz grid is a 2-dimensional arary
							(unless (or (eq x '_) (= r row) (= c column))
								(push x neighbors)
							)
						)
				)
		)
	)
)
;example: locate [4][6], once u reach column 9 which is cmax, means you've done

; we call the previous functions to check existing number in row, column and square
; and exclude those existing numbers
; then we use nset-difference to return a list of choices
(defun choices (row column grid)							;provide choices
	(nset-difference
	;return a list of elements of list 1 that do not appear in list 2
	;means throw away repeating figures
    ;then destroy list 1
		(list 1 2 3 4 5 6 7 8 9)						;list 1
		; don't use quote to create a list here, else it won't be evaluated
	;nconc is like append
    ;difference between append and nconc:
    ;- append will not affect the original copy of input lists
    ;the forth list is created where its elements are the same as those of the input lists
    ;- nconc will affect the original copy of the first input list
		(nconc (neighbors-in-row row column grid)		;function call, passing 3 arguments
				(neighbors-in-column row column grid)	;function call, passing 3 arguments
				 (neighbors-in-square row column grid)	;function call, passing 3 arguments
		)
	)
	;after this, we know which figures are possible for that particualr position
)
 
(defun solve (grid &optional (row 0) (column 0))	;array grid is a must-have argument here
													;while arguments row and column are optional, both having default values of 0
	(cond
		((= row 9)									;base case 1: if row equal to 9, means all row are solved, return grid
			grid
		)											;if base case 1 is false, proceed to base case 2
		((= column 9)								;base case 2: if column equal to 9
			(solve grid (1+ row) 0)					;recursion: proceed to next row
		)											;passing 3 args: grid, ++row, 0 and thus proceed to next row												
		((not (eq '_ (aref grid row column)))		;base case 3: if this particular position in grid is a digit
			;check if the retrieved value of grid is equal to _ ,then perform negation
			;AKA if it's not equal to _
			;means this is not an empty cell and we wouldn't do anything 
			(solve grid row (1+ column))			;recursive
			;proceed to new column of the same row
		)		
		;t is the second clause
		;last action to be performed if none of the above base cases is true
		(t 
			;dolist iterate over elements of a list
			;function choices is called
			
			;choice will act like a 'pointer' that iterate through returned content of choices
			;function (choices row column grid) will return a list of choices
			;choice can be modified to any name u like
			;it represents each element inside the list
			(dolist (choice (choices row column grid) (setf (aref grid row column) '_))
				;set the particular element of array grid to choice
				;(choice (choices ...) (<resultform>)), if resultform is omitted, result will be nil
				;this is the backtrack step
				;if the choice is not good for that particular position, go back & set it to _ again
				(setf (aref grid row column) choice)
				;when grid equal to the recursive call of ++column
				(when (eq grid (solve grid row (1+ column)))	;recursion
					(return grid)								; return the grid
					;else proceed to next iteration
				)
			)
		)
	)
)
