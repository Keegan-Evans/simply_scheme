; 7.1
(define (gertrude wd)
  (let ((article (if (vowel? (first wd)) 'an 'a)))
    (se article wd 'is article wd 'is article wd)))

; 7.2
(let ((pi 3.14159)
      (pie '(lemon meringue)))
  (se 'pi 'is pi 'but 'pie 'is pie))

; 7.3
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

; 7.4 
; This exercise defines new procedures for the symbols + and *, to multiplication and addition respectively. This works because all of the expressions giving the values are computed before the variables are actually created, preventing you from ending up with two procedures for addtion.
