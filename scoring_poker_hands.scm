; Scoring Poker Hands

(define (flush? hand)
  (cond ((equal? (count hand) 1) #t)
	((not (equal? (first (first hand)) (first (first (bf hand)))))
	 #f)
	(else (flush? (bf hand)))))

(define (compute_ranks vals)
  (compute_ranks_helper vals '()))

(define (compute_ranks_helper in_vals out_vals)
  (cond ((empty? in_vals) out_vals)
	((member? (first in_vals) out_vals)
	 (compute-ranks-helper (bf in_vals) out_vals))
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

(define (rank_order vals)
  )
