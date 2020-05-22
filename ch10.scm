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

; new triples
(define (find-triples-corners position)
  (every (lambda (comb) (substitute-triple comb position))
	 '(123 456 789 147 258 369 159 357 412 236 698 874)))

(define (ttt-corners position me)
  (ttt-choose (find-triples-corners position) me))                         

(define (find-triples-no-diag position)
  (every (lambda (comb) (substitute-triple comb position))
	   '(123 456 789 147 258 369)))


; TESTS for all of the modifications
(define new-rules '(ttt-corners ttt-no-diagonals ttt-four-corners))

(define (use-new-rules rules position me)
  (ttt-choose (rules position) me))

(define (test-new-rules new-rules me)
  (every (lambda (tp) (use-new-rules new-rules tp me)) test-positions))

(define test-positions
  '(
    xxo_oo___
    xx_oo____
    ))

; predicted test results
(define expected-corners '(4 3))
(define expected-no-diag '(4 3))

; CONCLUSIONS
; 3 around the corners should still work
; Having one in each of the four corners would not work well, as procedures such as my-single? rely on the number of square in a triple being 3.

; 10.5
;Bwwahhaha, 'o stwongbad!               
