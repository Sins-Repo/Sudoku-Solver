# Sudoku Solver with Backtracking
* Board size: *9 x 9* grid 
* Language used: CLISP
* Platform: Allegro CL 10.1 Free Express Edition

<br/>
<br/>

# How to run
Define a board that you wish to solve. <br/>
Easy board
```
(defparameter *puzzle*
  #2A((3 9 4	_ _ 2	6 7 _)
  	(_ _ _	3 _ _	4 _ _)
  	(5 _ _	6 9 _	_ 2 _)
    
  	(_ 4 5	_ _ _	9 _ _)
  	(6 _ _	_ _ _	_ _ 7)
  	(_ _ 7	_ _ _	5 8 _)
    
  	(_ 1 _	_ 6 7	_ _ 8)
  	(_ _ 9	_ _ 8	_ _ _)
  	(_ 2 6	4 _ _	7 3 5)))
```

<br/>

Difficult board
```
(defparameter *puzzle*
  #2A((_ _ _	6 _ _	4 _ _)
  	(7 _ _	_ _ 3	6 _ _)
  	(_ _ _	_ 9 1	_ 8 _)
    
  	(_ _ _	_ _ _	_ _ _)
  	(_ 5 _	1 8 _	_ _ 3)
  	(_ _ _	3 _ 6	_ 4 5)
    
  	(_ 4 _	2 _ _	_ 6 _)
  	(9 _ 3	_ _ _	_ _ _)
  	(_ 2 _	_ _ _	1 _ _)))
```

<br/>

Call the `solve` function by passing the defined puzzle.

```
(pprint (solve *puzzle*))
```

<br/>
<br/>

# Notes
* Steps in the `.pdf` file 
* Comments in the `.lisp` file
