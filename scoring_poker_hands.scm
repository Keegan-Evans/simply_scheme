; Scoring Poker Hands

(define (poker-value hand)
  (define flush-val (flush? hand))
  (define rank-vals (card-sort (get_ranks hand)))
  (define straight-val (straight? rank-vals))
  (define rank-count-vals (compute-ranks rank-vals))
  (cond ((and flush-val
	      (equal? rank-vals '(a 10 j q k)))
	 'royal-flush)
	((and flush-val
	      straight-val)
	 'straigh-flush)
	((member? 'four rank-count-vals)
	 'four-of-a-kind)
	((and (member? 'three rank-count-vals)
	      (member? 'two rank-count-vals))
	 'full-house)
	((flush-val)
	 'flush)
	((straight-val)
	 'straight)
	((member? 'three rank-count-vals)
	 'three-of-a-kind)
	((equal? 2 (occurances 'two rank-count-vals))
	 'two-pairs)
	((member? 'two rank-count-vals)
	 'pair)
	(else 'nothing)))
	


; Is it a flush or not? AKA all the same suite?
(define (flush? hand)
  (cond ((equal? (count hand) 1) #t)
	((not (equal? (first (first hand)) (first (first (bf hand)))))
	 #f)
	(else (flush? (bf hand)))))

; How many are there of each value? Should look like:
; (compute_ranks '(q 3 4 3 4))
; (ONE q TWO 3 TWO 4)

(define (compute_ranks vals)
  (compute_ranks_helper vals '()))

(define (compute_ranks_helper in_vals out_vals)
  (cond ((empty? in_vals) out_vals)
	((member? (first in_vals) out_vals)
	 (compute_ranks_helper (bf in_vals) out_vals))
	(else (compute_ranks_helper 
		(bf in_vals) 
		(se out_vals (count_rank (first in_vals) in_vals))))))

(define (count_rank target vals)
  (count_rank_helper target vals 0))

(define (count_rank_helper target vals val_counter)
  (cond ((empty? vals) (se (spell_val val_counter) target))
	((equal? target (first vals))
	 (count_rank_helper
	   target
	   (bf vals)
	   (+ 1 val_counter)))
	(else
	  (count_rank_helper
	    target
	    (bf vals)
	    val_counter))))

(define (spell_val value)
  (cond ((= value 1) 'one)
	((= value 2) 'two)
	((= value 3) 'three)
	((= value 4) 'four)
	(else '(hmm something went wrong))))

; matching rank hands:
; in this section we will develop functions that detect hands that are
; scored by the basis of cards of matching rank such as four of a kind,
; full house, three of a kind, two pair, and pair.

(define (rank_hands hand)
  (rank_hands_helper (compute_ranks (get_ranks hand))))

(define (get_ranks hand)
  (if (empty? hand)
    '()
    (se (bf (first hand)) (get_ranks(bf hand)))))

(define (rank_hands_helper vals)
  (cond ((member? 'four vals) '(four of a kind))
	((and (member? 'three vals)
	      (member? 'two vals))
	 '(full house))
	((member? 'three vals) 
	 '(three of a kind))
	((= (occurances 'two vals) 2)
	 '(pair of pairs))
	((member? 'two vals)
	 'pair)
	(else 'nothing)))

(define (occurances target set)
  (occurance_helper target set 0))

(define (occurance_helper target set ct)
  (cond ((empty? set) ct)
	((equal? target (first set))
	 (occurance_helper target (bf set) (+ ct 1)))
	(else (occurance_helper target (bf set) ct))))


; Now lets check for straights or cards in a sequential order
; start by sorting:

(define (straight? ranks)
  (straight-helper (first ranks) (bf ranks) card-order))

(define (straight-helper first-card rest order)
  (cond ((empty? rest) #t)
	((and (equal? first-card (first order))
	      (not (equal? (first rest) (first (bf order)))))
	 #f)
	(else (straight-helper (first rest) (bf rest) (bf order)))))


(define (card-sort hand)
  (if (empty? hand)
    '()
    (se (first-card hand)
	(card-sort (remove-once (first-card hand) hand)))))

(define (first-card hand)
  (first-card-helper (first hand) (bf hand)))

(define (first-card-helper so-far rest)
  (cond ((empty? rest) so-far)
	((lower-card? so-far (first rest))
	 (first-card-helper so-far (bf rest)))
	(else (first-card-helper (first rest)(bf rest)))))

(define card-order '(a 2 3 4 5 6 7 8 9 10 j q k))

(define (lower-card? card1 card2)
  (lower-card-helper card1 card2 card-order))

(define (lower-card-helper card1 card2 order)
  (cond ((empty? order) '(that is not possible))
	((equal? card1 card2) #t)
	((equal? card1 (first order)) #t)
	((equal? card2 (first order)) #f)
	(else (lower-card-helper card1 card2 (bf order)))))
