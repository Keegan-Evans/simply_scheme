; 9.1
; it will state it is a procedure
; 34
; '(yan etim ta lal)
; an error stating the incorrect number of arguements

; 9.2
(define second (lambda (stuff) (first (bf stuff))))

(define make-adder (lambda (num) (lambda (x) (+ num x))))

; 9.3
; it returns last of a sentence by by using accumulate to look at the first word and the second word in a sentence then keeping only the second, it continues doing this will all of the other words in the sentence until only the last word is left.

; 9.4
(define (who sent)
  (every 
    (lambda (person) (se person sent))
    '(pete roger john keith)))

; 9.5 
(define (prepend-every prefix sent)
  (every
    (lambda (wd) (word prefix wd))
    sent))

; 9.6
(define (sentence-version fun)
  (lambda (sent) (every fun sent)))

; 9.7
(define (letterwords letter sent)
  (keep (lambda (wd) (member? letter wd)) sent))

; 9.8
;(define (hang-letter letter guesses)
;  (if (member? letter guesses)
;      letter
;      '_))

(define (hang secret_word guesses)
  (every (lambda (letter) (if (member? letter guesses) letter '_)) 
	 secret_word))


; 9.9
(define (common-words sent1 sent2)
  (keep (lambda (wd) (member? wd sent2)) sent1))

; 9.10
(define (my_appearances arg1 arg2)
  (count
    (keep (lambda (letter) (equal? letter arg1)) arg2)))

; 9.11
(define (unabbrev sent1 sent2)
  (every (lambda (wd)
		(if (number? wd)
		    (item wd sent2)
		    wd))
	 sent1))

; 9.12
(define (first-last sent)
  (keep (lambda (wd) (if (equal? (first wd) (last wd))
			 #t
			 #f))
	sent))

; 9.13
(define (compose f g)
  (lambda (arg) (f (g arg))))

; 9.14
(define (substitute target new sent)
  (every (lambda (wd)
	   (if (equal? wd target)
	       new
	       wd))
	 sent))

; 9.15
(define (type-check fun pred)
  (lambda (arg) 
    (if (pred arg)
	(fun arg)
	#f)))

; 9.16
(define (aplize fun)
  (lambda (args) 
    (if (sentence? args)
	(every fun args)
	(fun args))))

; 9.17
(define (keep-2 pred sent)
  (every (lambda (wd) 
	   (if (pred wd)
	       wd
	       '()))
	 sent))
