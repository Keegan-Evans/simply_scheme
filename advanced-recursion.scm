; Chapter 15: Advanced Recursion

; example: Sort

(define (sort sent)
  (if (empty? sent)
    '()
    (se (earliest-word sent)
	(sort (remove-once (earliest-word sent) sent)))))

(define (earliest-word sent)
  (earliest-helper (first sent) (bf sent)))

(define (earliest-helper so-far rest)
  (cond ((empty? rest) so-far)
	((before? so-far (first rest))
	 (earliest-helper so-far (bf rest)))
	(else (earliest-helper (first rest) (bf rest)))))


; example: From-Binary
