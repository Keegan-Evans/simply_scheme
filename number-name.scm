; number-name

; (define (number-name num)
;   (number-name-helper 
;     num
;     (remainder (count num) 3)
;     (floor (/ (count num) 3))))
; 
; (define (number-name-helper num leading group)
;   (cond ((empty? num) '())
; 
; 	; Deal with the leading digits 
; 	((> leading 0) 
; 	 (se (write-group (make-group num leading))
; 	     (place-name (+ group 1))
; 	     (number-name-helper ((repeated leading bf) num)
; 				 0
; 				 group)))
; 
; 	; deal with cases where a group is all zeros
; 	((and (= (item 1 num) 0)
; 	      (= (item 2 num) 0)
; 	      (= (item 3 num) 0))
; 	 (number-name-helper (bf (bf (bf num))) 0 (- group 1)))
; 
; 	; case greater than 1000
; 	((> group 1)
; 	 (se (write-group (first-n-digits num 3))
; 	     (place-name group)
; 	     (number-name-helper (bf (bf (bf num))) 0 (- group 1))))
; 
; 	(else (write-group (first-n-digits num 3)))))
; 
; (define (write-group num)
;   (if (= (item 2 num) 1)
;     (se (hundreds (first num))
; 	(name-single-teen (bf num)))
;     (se (hundreds (first num))
; 	(tens (first (bf num)))
; 	(name-single-teen (bf (bf num))))))
; 
; (define (place-name group)
;   (item (- group 1)
; 	’(thousand million billion trillion quadrillion quintillion
; sextillion septillion octillion nonillion decillion)))
; 
; (define (first-n-digits num n)
;   (if (= n 0)
;     ""
;     (word (first num) 
; 	  (first-n-digits (bf num) (- n 1)))))
; 
; (define (make-group num leading)
;   (if (= leading 1)
;     (se 0 0 (first-n-digits num leading))
;     (word 0 (first-n-digits num leading))))
; 
; (define (name-single-teen num)
;   (cond ((= num 1) 'one)
; 	((= num 2) 'two)
; 	((= num 3) 'three)
; 	((= num 4) 'four)
; 	((= num 5) 'five)
; 	((= num 6) 'six)
; 	((= num 7) 'seven)
; 	((= num 8) 'eight)
; 	((= num 9) 'nine)
; 	((= num 10) 'ten)
; 	((= num 11) 'eleven)
; 	((= num 12) 'twelve)
; 	((= num 13) 'thirteen)
; 	((= num 14) 'fourteen)
; 	((= num 15) 'fifteen)
; 	((= num 16) 'sixteen)
; 	((= num 17) 'seventeen)
; 	((= num 18) 'eighteen)
; 	((= num 19) 'nineteen)
; 	(else "")))
; 
; (define (hundreds digit)
;   (if (= digit 0)
;     ""
;     (se (name-single-teen digit) 'hundred)))
; 
; (define (tens digit)
;   (cond ((= digit 2) 'twenty)
; 	((= digit 3) 'thirty)
; 	((= digit 4) 'fourty)
; 	((= digit 5) 'fifty)
; 	((= digit 6) 'sixyty)
; 	((= digit 7) 'seventy)
; 	((= digit 8) 'eighty)
; 	((= digit 9) 'ninety)
; 	(else "")))

; TAKE TWO

(define (number-name-helper num group) 
  (cond ((empty? num) '())
	;check for all zeros
	((and (> (count num) 2)
	      (all-three-zeros? num))
	 (se (number-name-helper ((repeated bl 3) num ) (+ group 1))))
	;normal groups of three
	((> (count num) 2) 
	 (se (number-name-helper ((repeated bl 3) num) (+ group 1))
	     (write-group (last-n-digits num 3))
	     (place-name group)))
	      
	(else (se (write-group num) (place-name group)))))

(define (last-n-digits num n)
  (if (= n 0)
    ""
    (word (last-n-digits (bl num) (- n 1))
	  (last num))))

(define (write-group num)
  (cond ((and 
	   (= (count num) 3)
	   (= (item 2 num) 1) 
	   )
	 (se (hundreds (first num))
	     (name-single-teen (bf num))))
	((= (count num) 3)
	 (se (hundreds (first num)) 
	     (tens (first (bf num)))
	     (name-single-teen (bf (bf num)))))
	((and 
	   (not (= (first num) 1))
	   (= (count num) 2))
	 (se (tens (first num))
	     (name-single-teen (bf num))))
	(else (name-single-teen num))))
 
 (define (place-name group) 
   (item group  
	 '( ()
	      thousand 
	      million
	      billion
	      trillion
	      quadrillion
	      quintillion
	      sextillion
	      septillion
	      octillion
	      nonillion
	      decillion)))

(define (hundreds digit)
  (if (= digit 0)
    '()
    (se (name-single-teen digit) 'hundred)))

(define (tens digit)
  (cond ((= digit 2) 'twenty)
      ((= digit 3) 'thirty)
      ((= digit 4) 'fourty)
      ((= digit 5) 'fifty)
      ((= digit 6) 'sixyty)
      ((= digit 7) 'seventy)
      ((= digit 8) 'eighty)
      ((= digit 9) 'ninety)
      (else '())))

(define (name-single-teen num)

  (cond ((= num 1) 'one)
      ((= num 2) 'two)
      ((= num 3) 'three)
      ((= num 4) 'four)
      ((= num 5) 'five)
      ((= num 6) 'six)
      ((= num 7) 'seven)
      ((= num 8) 'eight)
      ((= num 9) 'nine)
      ((= num 10) 'ten)
      ((= num 11) 'eleven)
      ((= num 12) 'twelve)
      ((= num 13) 'thirteen)
      ((= num 14) 'fourteen)
      ((= num 15) 'fifteen)
      ((= num 16) 'sixteen)
      ((= num 17) 'seventeen)
      ((= num 18) 'eighteen)
      ((= num 19) 'nineteen)
      (else '())))
(define (roots a b c)
  ((lambda (discriminant)
     (se (/ (+ (- b) discriminant) (* 2 a))
	 (/ (- (- b) discriminant) (* 2 a))))
   (sqrt (- ( * b b) ( * 4 a c)))))

(define (all-three-zeros? num)
  ((lambda (seq)
     (and 
       (= (first seq) 0)
       (= (first (bf seq)) 0)
       (= (first (bf (bf seq))) 0)))
   (last-n-digits num 3)))
