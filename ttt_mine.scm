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
; same approach for other possibilities
; starting with blocking opponent
(define (opponent-can-win? triples me)
  (i-can-win? triples (opponent me)))

; that was easy but onwards
; situation where we can find possibility of creating two winnning triples
(define (i-can-for? triples me)
  (first-if-any (pivot triples me)))

(define (first-if-any sent)
  (if (empty? sent)
      #f
      (first sent)))

(define (pivots triples me)
  (repeated-numbers (keep (lambda (triple) (my-single? triple me))
			  triples)))

(define (my-single? triple me)
  (and (= (appearances me triple) 1)
       (= (appearances (opponent me) triple) 0)))

(define (repeated-numbers sent)
  (every first
	 (keep (lambda (wd) (>= (count wd) 2))
	       (sort-digits (accumulate word sent)))))

; move choice using semipredicates
(define (ttt-choose triples me)
  (cond ((already-won? triples me))
	((tie-game? triples me))
	((i-can-win? triples me)) ; kludge fix by using a semipredicate
	((opponent-can-win? triples me))
	((i-can-fork? triples me))
	((i-can-advance? triples me))
	(else (best-free-square tripples))
	))

(define (i-can-fork? triples me)
  (first-if-any (pivots triples me)))
(define (first-if-any sent)
  (if (empty? sent)
      #f
      (first sent)))

(define (extract-digit desired-digit wd)
  (keep (lambda (wd-digit) (equal? wd-digit desired-digit)) wd))

(define (sort-digits number-word)
  (every (lambda (digit) (extract-digit digit number-word))
	 '(1 2 3 4 5 6 7 8 9)))

(define (i-can-advance? triples me)
  (best-move (keep (lambda (triple) (my-single? triple me)) triples)
	     triples
	     me))

(define (best-move my-triples all-triples me)
  (if (empty? my-triples)
      #f
      (best-square (first my-triples) all-triples me)))

(define (best-square my-triple triples me)
  (best-square-helper (pivots triples (opponent me))
		      (keep number? my-triple)))

(define (best-square-helper opponents-pivots pair)
  (if (member? (first pair) opponents-pivots)
      (first pair)
      (last pair)))

(define (best-free-square triples)
  (first-choice (accumulate word triples)
		'(5 1 3 7 9 2 4 6 8)))

(define (first-choice possibilities preferences)
  (first (keep (lambda (square) (member? square possibilities))
	       preferences)))
