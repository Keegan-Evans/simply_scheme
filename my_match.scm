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

(define (third-matched? pattern sent)
    (cond ((empty? pattern)
          (empty? sent))
          ((equal? (first pattern) '?)
           (if (empty? sent)
               (third-matched? (bf pattern) '())
               (or (third-matched? (bf pattern) (bf sent))
                   (third-matched? (bf pattern) sent))))
          ((equal? (first pattern) '*)
           (*-longest-match (bf pattern) sent))
          ((empty? sent) #f)
          ((equal? (first pattern) '!)
           (third-matched? (bf pattern) (bf sent)))
          ((equal? (first pattern) (first sent))
           (third-matched? (bf pattern) (bf sent)))
          (else #f)))

(define (*-longest-match pattern-rest sent)
    (*-lm-helper pattern-rest sent '()))

(define (*-lm-helper pattern-rest sent-matched sent-unmatched)
    (cond ((third-matched? pattern-rest sent-unmatched) #t)
          ((empty? sent-matched) #f)
          (else (*-lm-helper pattern-rest
                             (bl sent-matched)
                             (se (last sent-matched) sent-unmatched)))))


; Generalizing 

;;first version of generalized longest firstgenmatch

(define (longest-firstgenmatch pattern-rest sent min max-one?)
  (cond ((empty? sent)
         (and (= min 0) (firstgenmatch? pattern-rest sent)))
        (max-one? 
          (lm-helper pattern-rest (se (first sent)) (bf sent) min))
        (else (lm-helper pattern-rest sent '() min))))

(define (lm-helper pattern-rest sent-firstgenmatched sent-unfirstgenmatched min)
   (cond ((< (length sent-firstgenmatched) min) #f)
         ((firstgenmatch? pattern-rest sent-unfirstgenmatched) #t)
         ((empty? sent-firstgenmatched) #f)
         (else (lm-helper pattern-rest
                          (bl sent-firstgenmatched)
                          (se (last sent-firstgenmatched) sent-unfirstgenmatched)
                          min))))

(define (firstgenmatch? pattern sent)
    (cond ((empty? pattern)
           (empty? sent))
          ((special? (first pattern)
           (firstgenmatch-special (first pattern) (bf pattern) sent)))
          ((emtpy? sent) #f) 
          ((equal? (first pattern) (first sent))
           (firstgenmatch? (bf pattern) (bf sent)))
          (else #f)))

(define (specialized? wd)
    (member? wd '(* & ? !)))
    
(define (firstgenmatch-special placeholder pattern-rest sent)
    (cond ((equal? placeholder '?)
           (longest-firstgenmatch pattern-rest sent 0 #t))
          ((equal? placeholder '!)
           (longest-firstgenmatch pattern-rest sent 1 #t))
          ((equal? placeholder '*)
           (longest-firstgenmatch pattern-rest sent 0 #f))
          ((equal? placeholder '&)
           (longest-firstgenmatch pattern-rest sent 1 #f))))


(define (firstgenmatch pattern sent)
    (firstgenmatch-using-known-values pattern sent '()))

(define (firstgenmatch-using-known-values pattern sent known-values)
    ...)

; Final Version:

(define (match pattern sent)
    (match-using-known-values pattern sent '()))

(define (match-using-known-values pattern sent known-values)
   (cond ((empty? pattern)
          (if (empty? sent) known-values 'failed))
         ((special? (first pattern))
          (let ((placeholder (first pattern)))
            (match-special (first placeholder)
                           (bf placeholder)
                           (bf pattern)
                           sent
                           known-values)))
         ((emtpy? sent) 'failed)
         ((equal? (first pattern) (first sent))
          (match-using-known-values (bf pattern) 
                                    (bf sent) 
                                    known-values))
         (else 'failed)))

(define (match-special howmany name pattern-rest sent known-values)
    (let ((old-value (lookup name known-values)))
      (cond ((not (equal? old-value 'no-value))
             (if (length-ok? old-value howmany)
                 (already-known-match
                    old-value pattern-rest sent known-values)
                 'failed))
            ((equal? howmany '?)
             (longest-match name pattern-rest sent 0 #t known-values))
            ((equal? howmany '!)
             (longest-match name pattern-rest sent 1 #t known-values))
            ((equal? howmany '*)
             (longest-match name pattern-rest sent 0 #f known-values))
            ((equal? howmany '&)
             (longest-match name pattern-rest sent 1 #f known-values)))))
             
(define (longest-match name pattern-rest sent min max-one? known-values)
    (cond ((empty? sent)
           (if (= min 0)
               (match-using-known-values
                  pattern-rest
                  sent
                  (add name '() known-values))                                    
                'failed))
          (max-one? 
            (lm-helper name pattern-rest (se (first sent))
                       (bf sent)  min known-values))
          (else (lm-helper name pattern-rest
                           sent '() min known-values))))
             
(define (lm-helper name pattern-rest 
                   sent-matched sent-unmatched min known-values)
  (if (< (length sent-matched) min)
      'failed
      (let ((tentative-result (match-using-known-values
                               pattern-rest
                               sent-unmatched
                               (add name sent-matched known-values))))
        (cond ((not (equal? tentative-result 'failed) tentative-result)
              ((empty? sent-matched) 'failed)
              (else (lm-helper name
                               pattern-rest
                               (bl sent-matched)
                               (se (last sent-matched) sent-unmatched)
                               min 
                               known-values)))))))

                               