; ch10/ttt

(define (substitute-letter sqr position)
  (if (equal? '_ (item sqr position))
      sqr
      (item sqr position)))

(define (substitute-triple combination position)
  (accumulate word (every
		     (lambda (sqr) (substitute-letter sqr position))
		     combination)))

(define (find-triples position)
  (every (lambda (comb) (substitute-triple comb position))
	 '(123 456 789 147 258 369 159 357)))

(define (ttt position me)
  (ttt-choose (find-triples position) me))

;(define (ttt-choose triples me) ;; first version
;  (cond ((i-can-win? triples me)
;	 (choose-winning-move triples me))
;	((opponent-can-win? triples me)
;	 (block-opponent-win triples me))
;	...))

; function to determine if a pair is mine
(define (my-pair? triple me)
  (and (= (appearances me triple) 2)
       (= (appearances (opponent me) triple) 0)))

; now need function to determine opponents letter
(define (opponent letter)
  (if (equal? letter 'x) 'o 'x))

; Do any of the triples belong to the computer?
;(define (i-can-win? triples me)
;  (not (empty?
;	 (keep (lambda (triple) (my-pair? triple me))
;	       triples))))
; first version
; semipredicate version and helper choose-win function
(define (choose-win winning-triples)
  (if (empty? winning-triples)
      #f
      (keep number? (first winning-triples))))

(define (i-can-win? triples me)
  (choose-win
    (keep (lambda (triple) (my-pair? triple me))
	  triples)))
(define (ttt-choose triples me)
  (cond ((i-can-win? triples me)) ; kludge fix by using a semipredicate
	((opponent-can-win? triples me))
	))
