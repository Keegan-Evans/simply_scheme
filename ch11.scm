; stuff from the chapter

(define (explode wd)
  (if (empty? wd)
      '()
      (se (first wd) (explode (bf wd)))))

(define (first-two wd)
  (word (first wd) (first (bf wd))))

(define (letter-pairs wd)
  (if (<= (count wd) 1)
      '()
      (se (first-two wd) (letter-pairs (bf wd)))))

; EXERCISES
; 11.1
; Write downup4 with only sentence and word primitive procedures
(define (downup4 wd)
  (se wd
      (bl wd)
      (bl (bl wd))
      (first wd)
      (bl (bl wd))
      (bl wd)
      wd))

; 11.2
(define (count-ums sent)
  (cond ((empty? sent) 0)
	((equal? 'um (first sent))
	 (+ 1 (count-ums (bf sent))))
	(else (count-ums (bf sent)))))
