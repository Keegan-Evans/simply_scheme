; `every` pattern

; two examples:

(define (square-sent sent)
  (if (empty? sent)
    '()
    (se (square (first sent))
	(square-sent (bf sent)))))

(define (pigl-sent sent)
  (if (empty? sent)
    '()
    (se (pigl (first sent))
	(pigl-sent (bf sent)))))

; a pretty clear pattern, that is our function will do something fairly
; straight forward with the first element of a sentence and then combine
; that with a recursive call the rest of the sentence. 

; letter-pairs is similar, though it looks two words at a time:

(define (letter-pairs wd)
  (if (= (count wd) 1)
  '()
  (se (word (first wd) (first (bf wd)))
      (letter-pairs (bf wd)))))

(define (disjoint-pairs wd)
  (cond ((empty? wd) '())
	((= (count wd) 1) (se wd))
	(else (se (word (first wd) (first (bf wd)))
		  (disjoint-pairs (bf (bf wd)))))))


; the `keep` pattern

; every pattern collect results of transforming every element of word or
; sentence. The keep pattern chooses some elements of a set and discards
; others. First example, select three-letter-words from a sentence:

(define (keep-three-letter-words sent)
  (cond ((empty? sent) '())
	((= (count (first sent)) 3)
	 (se (first sent) (keep-three-letter-words (bf sent))))
	(else (keep-three-letter-words (bf sent)))))

; a procedure for selecting vowels form a word:

(define (keep-vowels wd)
  (cond ((empty? wd) "")
	((vowel? (first wd))
	 (word (first wd) (keep-vowels (bf wd))))
	(else (keep-vowels (bf wd)))))

; differences between keep and every patterns. keep has 3 possible
; outcomes, while every *usualy* has 2. every: only distiguish between base
; case and the recursive case. keep: base case and 2 different recursive
; cases, where we use these to two to determine whether to keep the first
; element to the procedure return, keeping the element itself rather than
; the output of a function of the element.
; 
; there are also situations that follow the keep pattern only approximately,
; such as a function that looks for doubled letters in a word:

(define (doubles wd)
  (cond ((= (count wd) 1) "")
	((equal? (first wd) (first (bf wd)))
	 (word (first wd) (first (bf wd)) (doubles (bf (bf wd)))))
	(else (doubles (bf wd)))))

; The Accumulate Pattern

; combines that all elements of the argument into a single result.
; The pattern, use some combiner (+ or word) to the connect the
; portion that we are up to with the result of the recursive call. The
; base case tests for an empty argument,  but the base case return value
; must be the the identity element of the combiner function, for cases
; where there is no identity element for the combiner, the pattern is
; modified.
; Examples:

(define (addup nums)
  (if (empty? nums)
    0
    (+ (first nums) (addup (bf nums)))))

(define (scrunch-words sent)
  (if (empty? sent)
    ""
    (word (first sent) (scrunch-words (bf sent)))))

; no identity element example

(define (sent-max sent)
  (if (= (count sent) 1)
    (first sent)
    (max (first sent)
	 (sent-max (bf sent)))))
