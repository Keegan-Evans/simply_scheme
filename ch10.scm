; 10.1 It simply picks the first square that it can based on the functions
(define (already-won? triples me)
  (if (empty? (keep 
		     (lambda (triple) (equal? triple ((lambda (wd) 
							(word wd wd wd)) 
						      (opponent me)))) 
		     triples))
      #f
      (se '(Game already won by)  (opponent me))))

; 10.2 (and modified/added to for 10.3)
(define (tie-game? triples me)
  (if (or (empty? (keep number? triples))
	  (= (count (keep 
		      (lambda (triple) (split-pair? triple me)) 
		      triples))
	     8))
     '(The game is tied)
     #f))

; 10.3
(define (split-pair? triple me)
  (and (>= (appearances me triple) 1)
       (>= (appearances (opponent me) triple) 1)))

; 10.4
; a. What if you could win the game by having three squares forming an L shape in a corner (ex squares 1, 2, and 4)?


; 10.5
;Bwwahhaha, 'o stwongbad!
