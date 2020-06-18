; 12.1
; Original function
(define (addup-org nums)
  (if (empty? (bf nums))
      (first nums)
      (+ (first nums) (addup (bf nums)))))

; simpler base case
(define (addup nums)
  (if (empty? nums)
      0
      (+ (first nums) (addup (bf nums)))))

; 12.2
; Fix the bug in the following definition:
(define (acronym-wrong sent)
  (if (= (count sent) 1)
      (first sent)
      (word (first (first sent))
	    (acronym-wrong (bf sent)))))

; corrected, though could still be shorter:
(define (acronym-better sent)
  (if (= (count sent) 1)
      (first (first sent))
      (word (first (first sent))
	    (acronym-better (bf sent)))))

; Corrected, simpler base case
(define (acronym sent)
  (if (empty? sent)
      ""
      (word (first (first sent))
	    (acronym-better (bf sent)))))

; 12.3
; Can you reduce the base case of factorial to -1?
; I don't think this will work, as when n = 0, you will end up multiplying all of the values up to that point by 0, which will result in the output being 0 everytime.

(define (factorial-1 n)
  (if (= n -1)
      1
      (* n (factorial-1 (- n 1)))))

; That is what ended up happening.

; 12.4
; This should end up reversing the order of the sentence
(define (f sent)
  (if (empty? sent)
      sent
      (se (f (bf sent)) (first sent))))

; 12.5
(define (exaggerate sent)
  (cond ((empty? sent) '())
	((number? (first sent))
	 (se (* 2 (first sent)) (exaggerate (bf sent))))
	((equal? (first sent) 'good)
	 (se 'great (exaggerate (bf sent))))
	((equal? (first sent) 'bad)
	 (se 'terrible (exaggerate (bf sent))))
	(else (se (first sent) (exaggerate (bf sent))))))

; 12.6
(define (base-grade grade)
  (define grade-letter (first grade))
  (cond ((equal? grade-letter 'a) 4)
	((equal? grade-letter 'b) 3)
	((equal? grade-letter 'c) 2)
	((equal? grade-letter 'd) 1)
	(else 0)))

(define (grade-modifier grade)
  (cond ((equal? (last grade) '+) 0.33)
	((equal? (last grade) '-) (- 0.33))
	(else 0)))

(define (gpa grades)
  (if (= (count grades) 1)
    (+ (base-grade (first grades))
       (grade-modifier (first grades)))
    (/ (+ 
	 (+ (base-grade (first grades))
	    (grade-modifier (first grades)))
	 (gpa (bf grades)))
       2)))

; 12.7
; helper procedure
(define (spell-digit digit)
  (item (+ 1 digit)
	'(zero one two three four five six seven eight nine)))

(define (spell-number num)
  (if (empty? num)
    '()
    (se (spell-digit (first num)) (spell-number (bf num)))))
