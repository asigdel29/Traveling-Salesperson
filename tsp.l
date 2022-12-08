
;; PRECONDITION: MAXIMUM CITIES IS 11

( setf *a-b* (random 1001) )( setf *a-c* (random 1001) )( setf *a-d* (random 1001) )
( setf *a-e* (random 1001) )( setf *a-f* (random 1001) )( setf *a-g* (random 1001) )
( setf *a-h* (random 1001) )( setf *a-i* (random 1001) )( setf *a-j* (random 1001) )
( setf *a-k* (random 1001) )
( setf *b-c* (random 1001) )( setf *b-d* (random 1001) )( setf *b-e* (random 1001) )
( setf *b-f* (random 1001) )( setf *b-g* (random 1001) )( setf *b-h* (random 1001) )
( setf *b-i* (random 1001) )( setf *b-j* (random 1001) )( setf *b-k* (random 1001) )
( setf *c-d* (random 1001) )( setf *c-e* (random 1001) )( setf *c-f* (random 1001) )
( setf *c-g* (random 1001) )( setf *c-h* (random 1001) )( setf *c-i* (random 1001) )
( setf *c-j* (random 1001) )( setf *c-k* (random 1001) )
( setf *d-e* (random 1001) )( setf *d-f* (random 1001) )( setf *d-g* (random 1001) )
( setf *d-h* (random 1001) )( setf *d-i* (random 1001) )( setf *d-j* (random 1001) )
( setf *d-k* (random 1001) )
( setf *e-f* (random 1001) )( setf *e-g* (random 1001) )( setf *e-h* (random 1001) )
( setf *e-i* (random 1001) )( setf *e-j* (random 1001) )( setf *e-k* (random 1001) )
( setf *f-g* (random 1001) )( setf *f-h* (random 1001) )( setf *f-i* (random 1001) )
( setf *f-j* (random 1001) )( setf *f-k* (random 1001) )
( setf *g-h* (random 1001) )( setf *g-i* (random 1001) )( setf *g-j* (random 1001) )
( setf *g-k* (random 1001) )
( setf *h-i* (random 1001) )( setf *h-j* (random 1001) )( setf *h-k* (random 1001) )
( setf *i-j* (random 1001) )( setf *i-k* (random 1001) )
( setf *j-k* (random 1001) )

; Default list of cities, labeled A->Z
( setf *cities* 
   '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
)

; Establish number of cities to generate
( setf *limit* 11 )

; Return ordered list of cities w/ length = *limit*
( defmethod city-list (n)
   ( cond
      ( (= n 0) nil )
      ( t (subseq *cities* 0 n) )
   )
)

; Establish random city list by shuffling a city-list
( defmethod city-string ()
   ( setf cities (nshuffle (city-list *limit*)) )
   ( cond
      ( (> (position 'a cities) (position 'e cities))
         (city-string)
      )
      ( t
         cities
      )
   )
)

( setf *map* nil )

( defmethod map-string ()
   ( setf (symbol-plist 'a) (list 'b *a-b* 'c *a-c* 'd *a-d* 'e *a-e* 'f *a-f* 'g *a-g* 'h *a-h* 'i *a-i* 'j *a-j* 'k *a-k*) )
   ( setf (symbol-plist 'b) (list 'a *a-b* 'c *b-c* 'd *b-d* 'e *b-e* 'f *b-f* 'g *b-g* 'h *b-h* 'i *b-i* 'j *b-j* 'k *b-k*) )
   ( setf (symbol-plist 'c) (list 'a *a-c* 'b *b-c* 'd *c-d* 'e *c-e* 'f *c-f* 'g *c-g* 'h *c-h* 'i *c-i* 'j *c-j* 'k *c-k*) )
   ( setf (symbol-plist 'd) (list 'a *a-d* 'b *b-d* 'c *c-d* 'e *d-e* 'f *d-f* 'g *d-g* 'h *d-h* 'i *d-i* 'j *d-j* 'k *d-k*) )
   ( setf (symbol-plist 'e) (list 'a *a-e* 'b *b-e* 'c *c-e* 'd *d-e* 'f *e-f* 'g *e-g* 'h *e-h* 'i *e-i* 'j *e-j* 'k *e-k*) )
   ( setf (symbol-plist 'f) (list 'a *a-f* 'b *b-f* 'c *c-f* 'd *d-f* 'e *e-f* 'g *f-g* 'h *f-h* 'i *f-i* 'j *f-j* 'k *f-k*) )
   ( setf (symbol-plist 'g) (list 'a *a-g* 'b *b-g* 'c *c-g* 'd *d-g* 'e *e-g* 'f *f-g* 'h *g-h* 'i *g-i* 'j *g-j* 'k *g-k*) )
   ( setf (symbol-plist 'h) (list 'a *a-h* 'b *b-h* 'c *c-h* 'd *d-h* 'e *e-h* 'f *f-h* 'g *g-h* 'i *h-i* 'j *h-j* 'k *h-k*) )
   ( setf (symbol-plist 'i) (list 'a *a-i* 'b *b-i* 'c *c-i* 'd *d-i* 'e *e-i* 'f *f-i* 'g *g-i* 'h *h-i* 'j *i-j* 'k *i-k*) )
   ( setf (symbol-plist 'j) (list 'a *a-j* 'b *b-j* 'c *c-j* 'd *d-j* 'e *e-j* 'f *f-j* 'g *g-j* 'h *h-j* 'i *i-j* 'k *j-k*) )
   ( setf (symbol-plist 'k) (list 'a *a-k* 'b *b-k* 'c *c-k* 'd *d-k* 'e *e-k* 'f *f-k* 'g *g-k* 'h *h-k* 'i *i-k* 'j *j-k*) )
   ( setf *map* 
      ( list
         (symbol-plist 'a) 
         (symbol-plist 'b)
         (symbol-plist 'c)
         (symbol-plist 'd) 
         (symbol-plist 'e)
         (symbol-plist 'f) 
         (symbol-plist 'g) 
         (symbol-plist 'h) 
         (symbol-plist 'i) 
         (symbol-plist 'j) 
         (symbol-plist 'k) 
      )
   )
)
( map-string )

; To be used with (city-list) and *map* as inputs:
( defmethod print-map (cities map)
   ( cond
      ( ( null cities )
         nil
      )
      ( t
         ( format t "CITY ~A: ~A~%" (car cities) (car map) )
         ( print-map (cdr cities) (cdr map) )
      )
   )
)

; Quick run of the print-map method
( defmethod run-print-map ()
   ( format t "Printing map...~%" )
   ( print-map (city-list *limit*) *map* )
)

;; Knuth Shuffle Algorithm
;; Source: https://rosettacode.org/wiki/Knuth_shuffle#Common_Lisp
( defmethod nshuffle (sequence)
   ( loop for i from (length sequence) downto 2
      do ( rotatef (elt sequence (random i)) (elt sequence (1- i)) )
   )
   sequence
)


; ------------------------------------------
; TASK 2: Mutation

; Mutate by swapping the positions of two cities
( defmethod mutation ((city-string list) &aux n1 n2)
   ( setf n1 (random (list-length city-string)) )
   ( setf n2 (random-2 city-string n1) )
   ( rotatef (nth n1 city-string) (nth n2 city-string) )
   ( cond
      ( (> (position 'a city-string) (position 'e city-string))
         ( rotatef (nth n1 city-string) (nth n2 city-string) )
         ( mutation city-string )
      )
      ( t
         city-string
      )
   )
)

( defmethod random-2 (lst n)
   ( setf nr (random (list-length lst)) )
   ( if (= nr n)
      (random-2 lst n)
      nr
   )
)


; ------------------------------------------
; TASK 3: Crossover

; Crossover a mother and father based on a common span
( defmethod crossover ( (m list) (f list) )
   ( setf span-5 (common-span-n m f 5) )
   ( setf span-4 (common-span-n m f 4) )
   ( setf span-3 (common-span-n m f 3) )
   ( cond
      ( (not (null span-5))
         ( setf span-m (nth 0 span-5) )
         ( setf span-f (nth 1 span-5) )
         ( swap-lst m span-m span-f )
      )
      ( (not (null span-4))
         ( setf span-m (nth 0 span-4) )
         ( setf span-f (nth 1 span-4) )
         ( swap-lst m span-m span-f )
      )
      ( (not (null span-3))
         ( setf span-m (nth 0 span-3) )
         ( setf span-f (nth 1 span-3) )
         ( swap-lst m span-m span-f )
      )
      ( t
         nil
      )
   )
)

( setf *index* 0 )
 
( defmethod swap-lst (m span-m span-f)
   ( setf m (remove-span span-m m 0) )
   ( add-span span-f m 0 )
)

( defmethod add-span (span lst n)
   ( cond
      ( (= n (+ 1 (length lst)))
         nil
      )
      ( (= n *index* )
         ( setf *index* 0 )
         ( append (subseq lst 0 n) span (subseq lst n (length lst)) )
      )
      ( t
         ( add-span span lst (+ 1 n) )
      )
   )
)

( defmethod remove-span (span lst n)
   ( cond
      ( (null span)
         ( setf *index* n )
         lst
      )
      ( (eq (car span) (nth n lst))
         ( remove-span (cdr span) (remove (nth n lst) lst) n )
      )
      ( t
         ( remove-span span lst (+ 1 n) )
      )
   )
)

( defmethod common-span-n ( mother father n )
   ( equal-set-1 mother father 0 n 0 n )
)

( defmethod equal-set-1 ( m f mn1 mn2 fn1 fn2 &aux set-m set-f )
   ( cond
      ( (= mn2 (+ 1 (length m)))
         nil
      )
      ( t
         ( setf set-m (subseq m mn1 mn2) )
         ( setf set-f (subseq f fn1 fn2) )
         ( cond
            ( (eq ( equal-set-2 m f mn1 mn2 fn1 fn2 ) nil)
               ( equal-set-1 m f (+ mn1 1) (+ mn2 1) fn1 fn2 )
            )
            ( t
               ( equal-set-2 m f mn1 mn2 fn1 fn2 )
            )
         )
      )
   )
)

( defmethod equal-set-2 ( m f mn1 mn2 fn1 fn2 &aux set-m set-f )
   ( cond
      ( (= fn2 (+ 1 (length f)))
         nil
      )
      ( t
         ( setf set-m (subseq m mn1 mn2) )
         ( setf set-f (subseq f fn1 fn2) )
         ( cond
            ( (not (eq (set-difference set-m set-f) nil))
               ( equal-set-2 m f mn1 mn2 (+ fn1 1) (+ fn2 1) )
            )
            ( t
               ( list set-m set-f ) 
            )
         )
      )
   )
)

; Based on code provided by Craig Graci:
( defmethod create-examples ((cities list) &aux mother father span)
   ( setf c1 (copy-list cities) )
   ( setf mother (nshuffle c1) )
   ( setf c2 (copy-list cities) )
   ( setf father (nshuffle c2) )
   ( setf span (common-span-n mother father 5) )
   ( cond
      ( (not (null span))
         ( format t "~A ~A ~A ~%" mother father span )
         ( create-examples cities )
      )
   )
   ( setf span (common-span-n mother father 4) )
   ( cond
      ( (not (null span))
         ( format t "~A ~A ~A ~%" mother father span )
         ( create-examples cities )
      )
   )
   ( setf span (common-span-n mother father 3) )
   ( cond
      ( (not (null span))
         ( format t "~A ~A ~A ~%" mother father span )
         ( create-examples cities )
      )
   )
)

; Create a fixed number of examples that doesn't loop forever
( defmethod create-examples-2 ()
   ( dotimes (i 20)
      ( create-examples (city-string) )
   )
)

; ------------------------------------------
; TASK 4: Demo for Mutation and Crossover

( defmethod mutation-demo ()
   ( setf s (city-string) )
   ( dotimes (i 10)
      ( format t "s = ~A~%" s )
      ( setf m ( mutation s ) )
      ( format t "m = ~A~%~%" m )
   )
)

; If crossover isn't applicable, run the method again 
; until the mother and father have a common span.
( defmethod crossover-demo ()
   ( setf m (city-string) )
   ( setf f (city-string) )
   ( setf x (crossover m f) )
   ( cond
      ( (null x)
         ( format t "m = ~A~%" m )
         ( format t "x = ~A~%" x )
         ( format t "f = ~A~%" f )
         ( format t "Crossover not applicable. Reshuffling... ~%~%" )
         ( crossover-demo )
      )
      ( t 
         ( format t "m = ~A~%" m )
         ( format t "x = ~A~%" x )
         ( format t "f = ~A~%~%" f )
      )
   )
)


; ------------------------------------------
; TASK 5: Fitness Metrics

( setf *started* nil )

; Sum of the distances across each city to get from A to E defines the fitness
( defmethod fitness ((cities list) &aux city (symbol-plist 'x))
   ( setf city (car cities) )
   ( setf (symbol-plist 'x) (symbol-plist city) )
   ( cond
      ( (eq 'a city)
         ( setf *started* t )
         ( + (get 'x (car (cdr cities))) (fitness (cdr cities)) )
      )
      ( (eq *started* nil)
         ( fitness (cdr cities) )
      )
      ( (eq 'e city)
         ( setf *started* nil )
         0
      )
      ( (null cities)
         ( setf *started* nil )
         0
      )
      ( t
         ( + (get 'x (car (cdr cities))) (fitness (cdr cities)) )
      )
   )
)

( defmethod fitness-demo (&aux x fitness)
   ( setf str (city-string) )
   ( setf x str )
   ( format t "MAP: ~A~%" *map* )
   ( format t "x = ~A~%" x )
   ( format t "Directly applying the fitness metric ...~%" )
   ( format t "fitness = ~A~%" (fitness str) )
   ( format t "Indirectly applying the fitness metric ...~%" )
   ( setf fitness #'fitness )
   ( format t "fitness = ~A~%" (funcall fitness str) )
)


; ------------------------------------------
; TASK 6: Individual Class

( defclass individual ()
   (  
      ( city-string :accessor individual-city-string :initarg :city-string )
      ( fitness :accessor individual-fitness :initarg :fitness )
      ( number :accessor individual-number :initarg :number )
   )
)  

( defmethod random-individual (&aux city)
   ( setf city (city-string) )
   ( make-instance 'individual
      :city-string city
      :fitness (funcall *fitness* city)
      :number 0
   )
)

( defmethod new-individual ((nr number) (notes list))
   ( make-instance 'individual
      :city-string notes
      :fitness (funcall *fitness* notes)
      :number nr
   )
)

( defmethod display ((i individual))
   (display-nnl i) (terpri)
)

( defmethod display-nnl ((i individual))
   ( prin1 (individual-number i) )
   ( princ (filler (individual-number i)) )
   ( prin1 (individual-city-string i) )
   ( format t " " )
   ( prin1 (individual-fitness i) )
   ( princ (filler (individual-fitness i)) )
)

( defmethod filler ( (n number) )
   ( cond
      ( ( < n 10 ) "     " )
      ( ( < n 100 ) "    " )
      ( ( < n 1000 ) "   " )
      ( ( < n 10000 ) "  " )
      ( ( < n 100000 ) " " )
   )
)

( defmethod fitness ((i individual))
   ( fitness (individual-city-string i) )
)

( defmethod individual-demo (&aux i0 i1 i2 i3 one two three)
   ( setf *fitness* #'fitness )
   ( setf i0 (random-individual) )
   ( display i0 )
   ( setf one (city-string) )
   ( setf i1 (new-individual 1 one) )
   ( display i1 )
   ( setf two (city-string) )
   ( setf i2 (new-individual 2 two) )
   ( display i2 )
   ( setf three (city-string) )
   ( setf i3 (new-individual 3 three) )
   ( display i3 )
   ( format t "Fitness of i0 = ~A~%" (funcall *fitness* i0) )
   ( format t "Fitness of i1 = ~A~%" (funcall *fitness* i1) )
   ( format t "Fitness of i2 = ~A~%" (funcall *fitness* i2) )
   ( format t "Fitness of i3 = ~A~%" (funcall *fitness* i3) )
   nil
)


; ------------------------------------------
; TASK 7: Population Class

( defconstant *population-size* 100 )
( defconstant *selection-size* 8 )
( setf *fitness* #'fitness )

( defclass population ()
   (
      ( individuals :accessor population-individuals :initarg :individuals )
      ( generation :accessor population-generation :initform 0 )
   )
)

( defmethod size ( (p population) )
   ( length (population-individuals p) )
)

( defmethod display ( (p population) )
   (terpri) (terpri)
   ( princ "Generation " )
   ( prin1 (population-generation p) )
   ( princ " population..." )
   ( terpri ) (terpri)
   ( dolist (i (population-individuals p))
      ( display i )
   )
   ( terpri )
)

( defmethod initial-population (&aux individuals)
   ( setf individuals () )
   ( dotimes (i *population-size*)
      ( push (new-individual (+ i 1) (city-string)) individuals )
   )
   ( make-instance 'population :individuals (reverse individuals) )
)

( defmethod average ((p population) &aux (sum 0))
   ( dolist (i (population-individuals p))
      ( setf sum ( + (individual-fitness i) sum ) )
   )
   ( / (float sum) (float *population-size*) )
)

( setf *select-demo* nil )

( defmethod select-individual ((p population) &aux i candidates rn)
   ( setf candidates (select-individuals p) )
   ( setf mfi (most-fit-individual candidates) )
   ( if *select-demo* (select-demo-helper candidates mfi) )
   mfi
)

( defmethod select-individuals ((p population) &aux individuals candidates rn)
   ( setf individuals (population-individuals p) )
   ( setf candidates () )
   ( dotimes ( i *selection-size* )
      ( setf rn (random *population-size*) )
      ( push (nth rn individuals) candidates )
   )
   candidates
)

( setf *infinity* most-positive-fixnum )

;; Unlike the traditional fitness metric, this metric is represented as
;; "lower = more fit" -- less distance is the goal.
( defmethod most-fit-individual ((l list) &aux max-value max-individual)
   ( setf max-value *infinity* )
   ( dolist ( i l )
      ( if ( > max-value (individual-fitness i))
         ( let ()
            ( setf max-value (individual-fitness i) )
            ( setf max-individual i )
         )
      )
   )
   max-individual
)

( defmethod select-demo-helper ((l list) (i individual))
   ( princ "the sample of individuals ..." ) (terpri)
   ( mapcar #'display l )
   ( terpri )
   ( princ "the most fit of the sample ... " ) (terpri)
   ( display i )
   ( terpri )
   nil
)

( defmethod population-demo (&aux p)
   ( setf p (initial-population) )
   ( display p )
   ( format t "Average fitness = ~A~%~%" (average p) )
   ( setf *select-demo* t )
   ( format t "Sampling ...~%~%" )
   ( select-individual p ) (terpri) 
   ( format t "Sampling ...~%~%" )
   ( select-individual p ) (terpri) 
   ( format t "Sampling ...~%~%" )
   ( select-individual p ) (terpri) 
)


; ------------------------------------------
; TASK 8: Incorporate Mutation

( defmethod mutate ((i individual) &aux mutation)
   ( setf mutation (mutation (individual-city-string i)) )
   ( make-instance 'individual
      :number (individual-number i)
      :city-string mutation
      :fitness (funcall *fitness* mutation)
   )
)

( defconstant *pc-m* 50 )

( defmethod maybe-mutate ( (i individual) )
   ( if ( <= ( + 1 (random 100) ) *pc-m* )
      (mutate i)
      i
   )
)

( defmethod mutate-demo ()
   ( setf i (random-individual) )
   ( display i )
   ( dotimes (x 20)
      ( setf i (mutate i) )
      ( display i )
   )
)

( defmethod maybe-mutate-demo ()
   ( setf i (random-individual) )
   ( display i )
   ( dotimes (x 20)
      ( setf n (maybe-mutate i) )
      ( display-nnl n )
      ( if (not (equal n i)) (princ " *") )
      ( terpri )
      ( setf i n )
   )
)


; ------------------------------------------
; TASK 9: Copy

( setf *copy-demo* nil )

( defconstant *pc-c* 40 )

( defmethod perform-copies ( (cp population) (np population) )
   ( dotimes (i (nr-copies))
      ( perform-one-copy cp np )
   )
)

( defmethod nr-copies ()
   ( * ( / *pc-c* 100 ) *population-size* )
)

( defmethod perform-one-copy ( (cp population) (np population) 
   &aux x m mm new-i )
   ( setf m (select-individual cp) )
   ( if *copy-demo* (format t "Selected individual = ~%") )
   ( if *copy-demo* (display m) )
   ( setf mm (maybe-mutate m) )
   ( if *copy-demo* (format t "Possibly muted individual = ~&") )
   ( if *copy-demo* (display mm) )
   ( setf (individual-number mm) (+ 1 (size np)) )
   ( if *copy-demo* (format t "Renumbered individual = ~&") )
   ( if *copy-demo* (display mm) )
   ( setf new-i (new-individual (+ 1 (size np)) (individual-city-string mm)) )
   ( setf 
      ( population-individuals np )
      ( append (population-individuals np) (list new-i) )
   )
   nil
)

( defmethod empty-population ( (cp population) &aux np )
   ( setf np (make-instance 'population) )
   ( setf (population-individuals np) () )
   ( setf (population-generation np) (+ 1 (population-generation cp)) )
   np
)

( defmethod perform-copies-demo (&aux cp np)
   ( setf cp (initial-population) )
   ( setf np (empty-population cp) )
   ( terpri ) (display np) (terpri) (terpri)
   ( setf *select-demo* t )
   ( setf *copy-demo* t )
   ( dotimes (i 10)
      ( perform-one-copy cp np )
      ( terpri ) (display np) (terpri) (terpri)
   )
   ( setf *select-demo* nil )
   ( setf *copy-demo* nil )
   nil
)


; ------------------------------------------
; TASK 10: Crossover pt. 2

( setf *crossover-demo* nil )

( defconstant *pc-x* 60 )

( defmethod perform-crossovers ( (cp population) (np population) )
   ( dotimes (i (nr-crossovers)) 
      ( perform-one-crossover cp np )
   )
)

( defmethod nr-crossovers ()
   ( * (/ *pc-x* 100) *population-size* )
)

( defmethod perform-one-crossover ( (cp population) (np population) )
   ( let (x m mm mother father new-i)
      ( setf mother (select-individual cp) )
      ( setf father (select-individual cp) )
      ( if *crossover-demo* (format t "Selected mother = ~%") )
      ( if *crossover-demo* (display mother) )
      ( if *crossover-demo* (format t "Selected father = ~&") )
      ( if *crossover-demo* (display father) )
      ( setf m (crossover mother father) )
      ( if *crossover-demo* (format t "the crossover = ~&") )
      ( if *crossover-demo* (display m) )
      ( setf mm (maybe-mutate m) )
      ( if *crossover-demo* (format t "the possibly mutated individual = ~&") )
      ( if *crossover-demo* (display mm) )
      ( setf (individual-number mm) (+ 1 (size np)) )
      ( if *crossover-demo* (format t "the renumbered individual = ~&") )
      ( if *crossover-demo* (display mm) )
      ( setf new-i (new-individual (+ 1 (size np)) (individual-city-string mm)) )
      ( setf
         (population-individuals np)
         (append (population-individuals np) (list new-i))
      )
   )
   nil
)

( defmethod crossover ( (mother individual) (father individual) 
   &aux mi fi x i )
   ( setf mi (individual-city-string mother) )
   ( setf fi (individual-city-string father) )
   ( setf x (crossover mi fi) )
   ( if (eq x nil)
      ( setf i (new-individual 0 mi) )
      ( setf i (new-individual 0 x) )
   )
   i
)

( defmethod perform-crossovers-demo (&aux cp np)
   ( setf cp (initial-population) )
   ( setf np (empty-population cp) )
   ( terpri ) (display np) (terpri) (terpri)
   ( setf *select-demo* t )
   ( setf *crossover-demo* t )
   ( dotimes (i 10)
      ( perform-one-crossover cp np )
      ( terpri ) (display np) (terpri) (terpri)
   )
   ( setf *select-demo* nil )
   ( setf *crossover-demo* nil )
   nil
)

; ------------------------------------------
; TASK 11: Next Generation

( defconstant *nr-generations* 25 )

( defmethod next-generation ( (cp population) &aux np )
   ( setf np (empty-population cp) )
   ( perform-copies cp np )
   ( perform-crossovers cp np )
   np
)

( defmethod ga-demo (&aux p)
   ( format t "OPTIMAL ROUTE FROM CITY A TO CITY E (lower fitness is better) ~%" )
   ( setf *fitness* #'fitness )
   ( setf p (initial-population) )
   ( terpri )
   ( summarize p )
   ( dotimes (i *nr-generations*)
      ( setf p (next-generation p) )
      ( check-average p )
   )
   ( terpri )
   ( summarize p )
)

( defmethod summarize ( (p population) )
   ( display p )
   ( check-average p )
   ( terpri )
)

( defmethod check-average ( (p population) )
   ( format t "average fitness of population ~A = ~A~%"
      ( population-generation p )
      ( average p )
   )
)
