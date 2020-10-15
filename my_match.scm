; my_match.scm

; making a function that tests whether 2 sentences are exactly equal.

(define (sent-equal? sent1 sent2)
    (cond ((empty? sent1)
           (empty? sent2))
          ((empty? sent2) #f)
          ((equal? (first sent1) (first sent2))
           (sent-equal? (bf sent1) (bf sent2)))
          (else #f)))

(define (match-second? pattern sent)
    (cond ((empty? pattern)
          (empty? sent))
          ((equal? (first pattern) '?)
           (if (empty? sent)
               (match-second? (bf pattern) '())
               (or (match-second? (bf pattern) (bf sent))
                   (match-second? (bf pattern) sent))))
          ((empty? sent) #f)
          ((equal? (first pattern) '!)
           (match-second? (bf pattern) (bf sent)))
          ((equal? (first pattern) (first sent))
           (match-second? (bf pattern) (bf sent)))
          (else #f)))

; Third Iteration

(define (match? pattern sent)
    (cond ((empty? pattern)
          (empty? sent))
          ((equal? (first pattern) '?)
           (if (empty? sent)
               (match? (bf pattern) '())
               (or (match? (bf pattern) (bf sent))
                   (match? (bf pattern) sent))))
          ((equal? (first pattern) '*)
           (*-longest-match (bf pattern) sent))
          ((empty? sent) #f)
          ((equal? (first pattern) '!)
           (match? (bf pattern) (bf sent)))
          ((equal? (first pattern) (first sent))
           (match? (bf pattern) (bf sent)))
          (else #f)))

(define (*-longest-match pattern-rest sent)
    (*-lm-helper pattern-rest sent '()))

(define (*-lm-helper pattern-rest sent-matched sent-unmatched)
    (cond ((match? pattern-rest sent-unmatched) #t)
          ((empty? sent-matched) #f)
          (else (*-lm-helper pattern-rest
                             (bl sent-matched)
                             (se (last sent-matched) sent-unmatched)))))