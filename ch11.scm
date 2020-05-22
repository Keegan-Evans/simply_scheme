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

; 11.3
(define (unspell-letter letter)
  (cond ((member? letter 'abc) 2)
        ((member? letter 'def) 3)
        ((member? letter 'ghi) 4)
	((member? letter 'jkl) 5)  
	((member? letter 'mno) 6) 
	((member? letter 'prs) 7) 
	((member? letter 'tuv) 8) 
	((member? letter 'wxy) 9) 
	(else 0)))

(define (phone-unspell wd)
  (if (= (count wd) 1)
      (unspell-letter wd)
      (word (unspell-letter (first wd)) 
	    (phone-unspell (bf wd)))))

; 11.5
(define (initials sent)
  (if (empty? sent)
      '()
      (se (first (first sent)) 
	  (initials (bf sent)))))

; 11.6
(define (countdown num)
  (if (= num 0)
      'BLASTOFF!
      (se num (countdown (- num 1)))))

; 11.7
(define (copies num wd)
  (if (= num 0)
      '()
      (se wd 
	  (copies (- num 1) wd))))
