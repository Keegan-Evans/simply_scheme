(load "simply")

(define (test fun)
  (fun '(a b c) '(d e f)))

; 5.2 
(define (f1 se1 se2)
  (se (bf se1) (bl se2)))

(define (f2 se1 se2)
  (se (f1 se1 se2) 
      (word (first se1) (last se2))))

(define (f3 se1 se2)
  (se se1 se1))

(define (f4 se1 se2)
  (define (mid_of_three sent)
    (bf (bl sent)))
  
  (se (mid_of_three se1) (mid_of_three se2)))

; 5.7
; (bf 'x) will return an empty world while (bf '(x)) will return an empty sentence

;5.8
;(you know my name (look up the number))

; 5.9
; matt
; (brian harvey)

(define (question_9a)
  (se (word (bl (bl (first '(make a))))
	    (bf (bf (last '(baseball mitt)))))
      (word (first 'with) (bl (bl (bl (bl 'rigidly))))
	    (first 'held) (first (bf 'stiches)))))

; 5.14
(define (third text)
  (first (bf (bf text))))

; 5.15
(define (first-two wd)
  (word (first wd)
	(first (bf wd))))

; 5.16
(define (two-firsts wd1 wd2)
  (word (first wd1)
	(first wd2)))

(define (knight wd)
  (se 'sir
      wd))

; 5.19
(define (insert-and sent)
  (se (bl sent)
      'and
      (last sent)))

; 5.20
(define (middle-names sent)
  (bf (bl sent)))

; 5.21
(define (query sent)
  (se (first (bf sent))
      (first sent)
      (bf (bf sent))
      '?))
