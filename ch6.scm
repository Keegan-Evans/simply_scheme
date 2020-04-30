; 6.1
(define (third-person-singular verb)
  (cond ((equal? verb 'be) 'is)
	((equal? (last verb) 'o)(word verb 'es))
	(else (word verb 's))))

; 6.3
(define (sign number)
  (cond ((< number 0) 'negative)
	((= number 0) 'zero)
	(else 'positive)))

; 6.4
(define (utensil meal)
  (if (equal? meal 'chinese)
      'chopsticks
      'fork))

; 6.5
; Write a procedure, called european-time to convert from American AM/PM notation into 24hr notation.
(define (european-time time_of_day period)
  (if (= time_of_day 12)
      (if (equal? period 'AM)
	    24
	    12)
      (cond
	  ((equal? period 'AM) 
	   time_of_day)
	  ((equal? period 'PM) 
	   (+ time_of_day 12))
	  (else "I a not clear on what part of the day you are talking about, please clarify AM or PM."))))

(define (american-time euro-time)
  (cond ((equal? euro-time 12) (se 12 'PM))
	((equal? euro-time 24) (se 12 'AM))
	((> euro-time 12) (se (- euro-time 12)'PM))
	(else (se euro-time 'AM))))

; 6.6
(define (teen? age)
  (if (and (<= age 19)
	   (>= age 13))
      #t
      #f))

; 6.7

; helper function to determine if sentence
(define (determine-type arg)
  (cond ((number? arg) 
	 (cond ((and (integer? arg) (> arg 0)) '"positive integer")
	       ((and (integer? arg) (< arg 0)) '(negative integer))
	       (else 'number)))
	((word? arg) 'word)
	((sentence? arg) 'sentence)
	((boolean? arg) 'bool)
	(else '(not sure what type this is))))

; 6.8

(define (vowel? letter)
  (if (member letter '(a e i o u))
      #t
      #f))

(define (indef-article noun)
  (if (vowel? (first noun))
      (se 'an noun)
      (se 'a noun)))

; 6.9
; (define (plural wd)
;   (if (equal? (last wd) 'y)
;       (word (bl wd) 'ies)
;       (word wd 's)))

(define (this-many num wd)
  (if (= num 1) 
      (se num wd)
      (se num (plural wd))))

;6.10
(define (sort2 sent)
  (if (= (count sent) 2)
      (if (< (first sent) (last sent))
	  sent
	  (se (last sent) (first sent)))
      '(Incorrect number of arguments)))

; 6.11
(define (proper-date day month year)
  (cond ((and (member month '(4 6 9 11))
	    (<= day 30))
       #t)
      ((and (member month '(1 3 5 7 8 10 12))
	    (<= day 31))
       #t)
      ((or (and (equal? month 2) (equal? (leap-year year) #t) (<= day 29))
	   (and (equal? month 2) (equal? (leap-year year) #f) (<= day 28)))
       #t)
      (else #f)))

(define (leap-year year)
  (cond ((and (equal? (remainder year 400) 0)
	       (equal? (remainder year 100) 0)) #t)
	((equal? (remainder year 100) 0) #f)
	((equal? (remainder year 4) 0) #t)
	(else #f)))


; 6.12
(define (plural wd)
  (cond ((and (vowel? (second_last wd)) (equal? (last wd) 'y))
	 (word wd 's))
	((equal? (last wd) 'y)
	 (word (bl wd) 'ies))
	((equal? (last wd) 'x)
	 (word wd 'es))
	(else
	  (word wd 's))))

(define (second_last wd)
  (last (bl wd)))

; 6.13
(define (greet name)
  (cond ((and (equal? (first name) 'dr) (equal? (last name) 'jr))
	 (se 'hello (first name) (second_last name)))
	((equal? (first name) 'dr)
	 (se 'hello 'dr (last name)))
	((or (equal? (first name) 'king) (equal? (first name) 'queen))
	 '(hello your majesty))
	((equal? name '(david livingstone))
	 '(dr livingstone I presume?))
	((equal? (last name) 'jr)
	 '(sup junior?))
	(else (se 'hello (first name)))))

; 6.14
(define (describe-time sec)
  (cond ((< sec 60)
	 (se sec 'seconds))
	((< sec 3600)
	 (se (/ sec 60.0) 'minutes))
	((< sec 86400)
	 (se (/ sec 3600.0) 'hours))
	((< sec 31536000)
	 (se (/ sec 86400.0) 'days))
	((< sec 315360000)
	 (se (/ sec 31536000.0) 'years))
	(else (se (/ sec 3153600000.0) 'centuries))))
