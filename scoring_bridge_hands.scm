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


