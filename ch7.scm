; 7.1
(define (gertrude wd)
  (let ((article (if (vowel? (first wd)) 'an 'a)))
    (se article wd 'is article wd 'is article wd)))

