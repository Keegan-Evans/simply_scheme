; Bridge Score

;	Ace | 4
;	King | 3
;	Queen | 2
;	Jack | 1
;	2-10 | 0 

; 	Disturbution points:
;	only 2 cards of suit | 1
;	only 1 card of suit | 2
;	only 0 card of suit | 3

; Card values

(define (card-val card)
  (cond ((equal? (bf card) 'a) 4)
	((equal? (bf card) 'k) 3)
	((equal? (bf card) 'q) 2) 
	((equal? (bf card) 'j) 1) 
	(else 0)))

(define (high-card-points hand)
  (accumulate + (every card-val hand)))

; did not read the instructions closely enough, this combines count-suit
; and suit counts, which made the process much more challenging.
; The seperate procedures shall follow.
;(define (count-suit hand)
;  (every 
;   (lambda (suit) (word suit (accumulate 
;			 +
;			 (every
;			  (lambda (card) 
;			    	(if (equal? (first card) suit) 1 0))
;			  hand)))) 
;     '(h d s c)))

(define (count-suit suit hand)
  (accumulate 
    +
    (every 
     (lambda (card) (if (equal? (first card) suit)
			1 
			0)) 
     hand)))

(define (suit-counts hand)
  (every (lambda (individual_suit) (count-suit individual_suit hand)) '(s h c d)))
