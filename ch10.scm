; 10.1 It simply picks the first square that it can based on the functions
(define (already-won? triples me)
  (if (empty? (keep 
		     (lambda (triple) (equal? triple (word me me me))) 
		     triples))
      #f
      (se "Game already won by " me)))

