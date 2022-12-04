;; ------------------------------------------------------------------
;; Task 1: Random Tour
;; ------------------------------------------------------------------
( setf *cities* '( Kathmandu Pokhara Chitwan Janakpur Lalitpur ) )
( setf cities '() )

( setf ( symbol-plist 'Delhi ) '( Pokhara 282 Chitwan 1417 Janakpur 2210 Lalitpur 1491 ) )
( setf ( symbol-plist 'Jaipur ) '( Kathmandu 282 Chitwan 1148 Janakpur 2093 Lalitpur 1511 ) )
( setf ( symbol-plist 'Mumbai ) '( Kathmandu 1417 Pokhara 1148 Janakpur 1337 Lalitpur 2057 ) )
( setf ( symbol-plist 'Chennai ) '( Kathmandu 2210 Pokhara 2093 Chitwan 1337 Lalitpur 1667 ) )
( setf ( symbol-plist 'Kolkata ) '( Kathmandu 1491 Pokhara 1511 Chitwan 2057 Janakpur 1667 ) )
;;; tsp.l
( defmethod random-tour ( &aux l )
  ( setf cities ( copy-list *cities* ) )
  ( setf l ( nshuffle cities ) )
  l
)

( defun nshuffle ( lst )
  ( loop for i from ( length lst ) downto 2
    do ( rotatef ( elt lst ( random i ) )
         ( elt lst ( 1- i ) ) 
       ) 
  )
  lst
)

;; ------------------------------------------------------------------
;; Task 2: Mutation
;; ------------------------------------------------------------------
( defmethod mutation ( ( tour list ) &aux p q ) 
  ( setf p ( random ( length tour ) ) )
  ( setf q ( random ( length tour ) ) )
  ( rotatef ( nth p tour ) ( nth q tour ) )
  tour
)

;; ------------------------------------------------------------------
;; Task 3: Crossover
;; ------------------------------------------------------------------
( defmethod crossover ( ( m list ) ( f list ) )
  ( setf r ( from-m m ) )
  ( setf c ( from-f r f ) )
  ( append r c )
)

( defun from-m ( ( lst list ) &aux c1 c2 span )
  ( setf c1 ( pick lst ) )
  ( setf lst ( remove c1 lst ) )
  ( setf c2 ( pick lst ) )
  ( setf lst ( remove c2 lst ) )
  ( setf span ( list c1 c2 ) )
  span
)

( defun from-f ( ( rm list ) ( lst list ) )
  ( remove ( cadr rm ) ( remove ( car rm ) lst ) )
)

( defun pick ( ( lst list ) )
  ( nth ( random ( length lst ) ) lst )
)

;; ------------------------------------------------------------------
;; Task 4: Demo Programs for Mutation and Crossover
;; ------------------------------------------------------------------
( defmethod mutation-demo ( &aux s m ) 
  ( setf s ( random-tour ) )
  ( dotimes ( i 10 )
    ( format t "s = ~A~%" s )
    ( setf m ( mutation s ) )
    ( format t "m = ~A~%~%" m )
  )
)

( defmethod crossover-demo ( &aux m f x ) 
  ( setf m ( random-tour ) )
  ( setf f ( random-tour ) )
  ( dotimes ( i 10 )
    ( format t "m = ~A~%" m )
    ( setf x ( crossover m f ) )
    ( format t "x = ~A~%" x )
    ( format t "f = ~A~%~%" f )
  )
)

;; ------------------------------------------------------------------
;; Task 5: The Fitness Metric
;; ------------------------------------------------------------------
( defun singleton-p ( lst )
 ( cond
  ( ( null lst ) nil )
  ( ( null ( cdr lst ) ) t )
  ( t nil )
 )
)

( defun rac ( lst )
 ( cond 
  ( ( singleton-p lst ) ( car lst ) )
  ( t ( rac ( cdr lst ) ) )
 )
)

( defun distance ( tour )
  ( cond
    ( ( null ( cdr tour ) ) 0 )
    ( t ( + ( get ( car tour ) ( cadr tour ) ) ( distance ( cdr tour ) ) ) )
  )
)

( defmethod fitness ( tour )
  ( + ( get ( car tour ) ( rac tour ) ) ( distance tour ) )
)

( defmethod fitness-demo ( &aux x fitness )
  ( setf x ( random-tour ) )
  ( format t "x = ~A~%" x )
  ( format t "Directly applying the fitness metrics... ~%" )
  ( format t "fitness = ~A~%" ( fitness x ) )
  ( format t "Indirectly applying the fitness metrics... ~%" )
  ( setf fitness #'fitness )
  ( format t "fitness = ~A~%" ( funcall fitness x ) )
)

;; ------------------------------------------------------------------
;; Task 6: The Individual Class
;; ------------------------------------------------------------------
( defclass individual ()
  (
   ( tour :accessor individual-tour :initarg :tour )
   ( fitness :accessor individual-fitness :initarg :fitness )
   ( number :accessor individual-number :initarg :number )
  )
)

( defmethod random-individual ( ) 
  ( new-individual 0 ( random-tour ) )
)

( defmethod new-individual ( ( nr number ) ( notes list ) )
  ( make-instance 'individual
    :tour notes
	  :fitness ( fitness notes )
	  :number nr
  )	
)

( defmethod display ( ( i individual ) )
  ( display-nnl i ) ( terpri )
)

( defmethod display-nnl ( ( i individual ) )
  ( prin1 ( individual-number i ) )
  ( princ ( filler ( individual-number i ) ) )
  ( prin1 ( individual-tour i ) )
  ( princ "  " )
  ( prin1 ( individual-fitness i ) )
  ( princ ( filler ( individual-fitness i ) ) )
)

( defmethod filler ( ( n number ) )
  ( cond
	( ( < n 10 ) "     " )
	( ( < n 100 ) "    " )
	( ( < n 1000 ) "   " )
	( ( < n 10000 ) "  " )
	( ( < n 100000 ) " " )
  )
)

( defmethod fitness ( ( i individual ) )
  ( individual-fitness i ) 
)

( defmethod individual-demo ( &aux i0 i1 i2 i3 one two three )
  ( setf *fitness* #'fitness )
  ( setf i0 ( random-individual ) )
  ( display i0 )
  ( setf one ( random-tour ) )
  ( setf i1 ( new-individual 1 one ) )
  ( display i1 )
  ( setf two ( random-tour ) )
  ( setf i2 ( new-individual 2 two ) )
  ( display i2 )
  ( setf three (random-tour ) )
  ( setf i3 ( new-individual 3 three ) )
  ( display i3 )
  ( format t "Fitness of i0 = ~A~%" ( funcall *fitness* i0 ) )
  ( format t "Fitness of i1 = ~A~%" ( funcall *fitness* i1 ) )
  ( format t "Fitness of i2 = ~A~%" ( funcall *fitness* i2 ) )
  ( format t "Fitness of i3 = ~A~%" ( funcall *fitness* i3 ) )
  nil
)

;; ------------------------------------------------------------------
;; Task 7: The Population Class
;; ------------------------------------------------------------------
( defconstant *population-size* 100 )
( defconstant *selection-size* 8 )
( setf *fitness* #'fitness )

( defclass population ()
  (
	( individuals :accessor population-individuals :initarg :individuals )
	( generation :accessor population-generation :initform 0 )
  )
)

( defmethod size ( ( p population ) )
  ( length ( population-individuals p ) )
)

( defmethod display ( ( p population ) )
  ( terpri ) ( terpri )
  ( princ "Generation " )
  ( prin1 ( population-generation p ) )
  ( princ " population..." )
  ( terpri ) ( terpri )
  ( dolist ( i ( population-individuals p ) )
	( display i )
  )
  ( terpri )
)

( defmethod initial-population ( &aux individuals )
  ( setf individuals () )
  ( dotimes ( i *population-size* )
	( push ( new-individual ( + i 1 ) ( random-tour ) ) individuals )
  )
  ( make-instance 'population :individuals ( reverse individuals ) )
)	

( defmethod average ( ( p population ) &aux (sum 0) )
  ( loop for i in ( population-individuals p ) 
    do ( setf sum ( + sum ( individual-fitness i ) ) )
  )
  ( float ( / sum ( size p ) ) )
)

( setf *select-demo* nil )

( defmethod select-individual ( ( p population ) &aux i candidates rn )
  ( setf candidates ( select-individuals p ) )
  ( setf mfi ( most-fit-individual candidates ) )
  ( if *select-demo* ( select-demo-helper candidates mfi ) )
  mfi
)

( defmethod select-individuals ( ( p population ) &aux individuals candidates rn )
  ( setf individuals ( population-individuals p ) )
  ( setf candidates () )
  ( dotimes ( i *selection-size* )
    ( setf rn ( random *population-size* ) )
    ( push ( nth rn individuals ) candidates )
  )
  candidates 
)

( defmethod most-fit-individual ( ( lst list ) )
  ( reduce 
    ( lambda ( a b )
      ( if ( < ( individual-fitness a ) ( individual-fitness b ) ) a b )
    )
    lst
  )
)

( defmethod select-demo-helper ( ( l list ) ( i individual ) )
  ( princ "the sample of individuals..." ) ( terpri )
  ( mapcar #'display l )
  ( terpri )
  ( princ "the most fit of the sample..." ) ( terpri )
  ( display i )
  ( terpri )
  nil
)

( defmethod population-demo ( &aux p )
  ( setf p ( initial-population ) )
  ( display p )
  ( format t "Average fitness = ~A~%~%" ( average p ) )
  ( setf *select-demo* t )
  ( format t "Sampling...~%~%" )
  ( select-individual p ) ( terpri )
  ( format t "Sampling...~%~%" )
  ( select-individual p ) ( terpri )
  ( format t "Sampling...~%~%" )
  ( select-individual p ) ( terpri )
)

;; ------------------------------------------------------------------
;; Task 8: Incorporating Mutation
;; ------------------------------------------------------------------
( defmethod mutate ( ( i individual ) &aux mutation )
  ( setf mutation ( mutation ( individual-tour i ) ) )
  ( make-instance 'individual
    :number ( individual-number i )
    :tour mutation
    :fitness ( funcall *fitness* mutation )
  )
)

( defconstant *pc-m* 50 )

( defmethod maybe-mutate ( ( i individual ) )
  ( if ( <= ( + 1 ( random 100 ) ) *pc-m* )
    ( mutate i )
    i
  )
)

( defmethod mutate-demo ()
  ( setf i ( random-individual ) )
  ( display i )
  ( dotimes ( x 20 )
	( setf i ( mutate i ) )
	( display i )
  )	
)

( defmethod maybe-mutate-demo ()
  ( setf i ( random-individual ) )
  ( display i )
  ( dotimes ( x 20 )
	( setf n ( maybe-mutate i ) )
	( display-nnl n )
	( if ( not ( equal n i ) ) ( princ " *" ) )
	( terpri )
	( setf i n )
  )	
)

;; ------------------------------------------------------------------
;; Task 9: Copy!
;; ------------------------------------------------------------------
( setf *copy-demo* nil )

( defconstant *pc-c* 40 )

( defmethod perform-copies ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-copies ) )
    ( perform-one-copy cp np )
  )
)

( defmethod nr-copies ()
  ( * ( / *pc-c* 100 ) *population-size* )
)

( defmethod perform-one-copy ( ( cp population ) ( np population ) &aux x m mm new-i )
  ( setf m ( select-individual cp ) )
  ( if *copy-demo* ( format t "Selected individual = ~%" ) )
  ( if *copy-demo* ( display m ) )
  ( setf mm ( maybe-mutate m ) )
  ( if *copy-demo* ( format t "Possibly muted individual = ~&" ) ) 
  ( if *copy-demo* ( display mm ) )
  ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
  ( if *copy-demo* ( format t "Renumbered individual = ~&" ) ) 
  ( if *copy-demo* ( display mm ) )
  ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-tour mm ) ) )
  ( setf
    ( population-individuals np )
    ( append ( population-individuals np ) ( list new-i ) )
  )
  nil
)

( defmethod empty-population ( ( cp population ) &aux np )
  ( setf np ( make-instance 'population ) )
  ( setf ( population-individuals np ) () )
  ( setf ( population-generation np ) ( + 1 ( population-generation cp ) ) )
  np
)

( defmethod perform-copies-demo ( &aux cp np )
  ( setf cp ( initial-population ) )
  ( setf np ( empty-population cp ) )
  ( terpri ) ( display np ) ( terpri ) ( terpri )
  ( setf *select-demo* t )
  ( setf *copy-demo* t )
  ( dotimes ( i 10 )
	( perform-one-copy cp np )
	( terpri ) ( display np ) ( terpri ) ( terpri )
  )
  ( setf *select-demo* nil )
  ( setf *copy-demo nil )
  nil
)

;; ------------------------------------------------------------------
;; Task 10: Crossover!
;; ------------------------------------------------------------------
( setf *crossover-demo* nil )

( defconstant *pc-x* 60 )
 
( defmethod perform-crossovers ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-crossovers ) )
    ( perform-one-crossover cp np )
  )
)

( defmethod nr-crossovers ()
  ( * ( / *pc-x* 100 ) *population-size* ) 
)

( defmethod perform-one-crossover ( ( cp population ) ( np population ) )
  ( let ( x m mm mother father new-i )
    ( setf mother ( select-individual cp ) )
    ( setf father ( select-individual cp ) )
    ( if *crossover-demo* ( format t "Selected mother = ~%" ) )
    ( if *crossover-demo* ( display mother ) )
    ( if *crossover-demo* ( format t "Selected father = ~%" ) )
    ( if *crossover-demo* ( display father ) )
    ( setf m ( crossover mother father ) )
    ( if *crossover-demo* ( format t "the possibly mutated individual = ~&" ) )
    ( if *crossover-demo* ( display m ) )
    ( setf mm ( maybe-mutate m ) )
    ( if *crossover-demo* ( format t "the possibly mutated individual = ~&" ) )
    ( if *crossover-demo* ( display mm ) )
    ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
    ( if *crossover-demo* ( format t "the renumbered individual = &" ) )
    ( if *crossover-demo* ( display mm ) )
    ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-tour mm ) ) )
    ( setf
      ( population-individuals np )
      ( append ( population-individuals np ) ( list new-i ) )
    )
  )
  nil
)

( defmethod crossover ( ( mother individual ) ( father individual ) &aux mi fi x i )
  ( setf mi ( individual-tour mother ) )
  ( setf fi ( individual-tour father ) )
  ( setf x ( crossover mi fi ) )
  ( setf i ( new-individual 0 x ) )
  i
)

( defmethod perform-crossovers-demo ( &aux cp np )
  ( setf cp ( initial-population ) )
  ( setf np ( empty-population cp ) )
  ( terpri ) ( display np ) ( terpri ) ( terpri )
  ( setf *select-demo* t )
  ( setf *crossover-demo* t )
  ( dotimes ( i 10 )
	( perform-one-crossover cp np )
	( terpri ) ( display np ) ( terpri ) ( terpri )
  )
  ( setf *select-demo* nil )
  ( setf *crossover-demo* nil )
  nil
)

;; ------------------------------------------------------------------
;; Task 11: The GA
;; ------------------------------------------------------------------
;; THE NEXT GENERATION METHOD FOR THE GA

( defconstant *nr-generations* 25 )

( defmethod next-generation ( ( cp population ) &aux np )
  ( setf np ( empty-population cp ) )
  ( perform-copies cp np )
  ( perform-crossovers cp np )
  np
)

;; THE GA!

;;OUTPUT IS JUST TEXT -- LOTS OF IT!
( defmethod ga-text-demo ( &aux p )
  ( setf *fitness* #'fitness ) 
  ( setf p ( initial-population ) ) 
  ( terpri )
  ( summarize p )
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
    ( check-average p )
  )
  ( terpri )
  ( summarize p )
)

;; METHODS TO PROVIDE INFORMATION ON "PROGRESS"

( defmethod summarize ( ( p population ) )
  ( display p )
  ( check-average p )
  ( terpri )
)

( defmethod check-average ( ( p population ) )
  ( format t "average fitness of population ~A = ~A~%" 
    ( population-generation p )
    ( average p )
  )
)
