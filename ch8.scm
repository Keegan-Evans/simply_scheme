; 8.1
;'(a e i o u)
;(every last '(algebra purple spaghetti tomato gnu))
;
;'()
;(keep number? '(one two three four))
;
;0
;(accumulate * '(6 7 13 0 9 42 17))
;
;#f
;(member? 'h (keep vowel? '(t h r o a t)))
;
;(16 144 0)
;(every square (keep even? '(87 4 7 12 0 5)))
;
;ai
;(accumulate word (keep vowel? (every first '(and i love her))))
;0
;((repeated square 0) 25)
;
;'(go d sunshi)
;(every (repeated bl 2) '(good day sunshine))
;
;Got all correct.

; 8.2
; keep
; every
; first
; every last
; accumulate word every last
; every
; accumulate

; 8.3
(define (f a)
  (keep even? a))
; A function that takes sentences of integers are returns a sentence of all of the even integers in the original sentence.

(define (g b)
  (every b '(blue jay way)))
; takes selector functions and out puts a sentence containing various subsets of the sentence (blue jay way)

(define (h c d)
  (c (c d)))
; inputs are first a selector function and then either a word or a sentence. It then it applies the selector function to it twice. This can result in either a blank word, a single letter word, a word, or a blank sentence depending on the selector function and the word or sentence provided to h.

(define (i e)
  (/ (accumulate + e) (count e)))
; the function i finds the average of all of the numbers in i. i must be a sentence or word of numbers. it can contain any kind of real numbers. it outputs a single number.

; accumulate takes a procedure(the first argument) and applies it to a sentence(the second argument) two words at a time. It continues to apply the procedure to the result and the next element in the sentence. It finishes when it has combined all of the words into a single result. That is the real power of this function, in that it takes all of the words of a sentence and combines them into a single meaningful(hopefully) result.

; repeated is a procedure that takes a procedure and a (positive?) integer as inputs. It outputs a procure that is the application of the input proceducre the input number of times.

;(repeated sqrt 3) This is a function that takes any number as an input and outputs a another number that is the 3rd square root of it. That is, it takes the square root of the input number, then takes the square root of that, then finally returns the square root of that.

; (repeated even? 2) This procedure will not function correctly, as it is trying to apply the even? procedure twice. However the output of the even? procedure is a boolean value and it expects an integer as the input.

;(repeated first 2) This procedure will take a word or sentence and output a word. If you give it a word it will return the first letter of the word, as the first letter of one letter is that letter. If the input is a sentence it will output the first letter of the first word, as it first applies the the first procedure to the sentence returning the first word. Then it applies the first procedure to that word. 

; (repeated (repeated bf 3) 2) This procedure will return all but the first six words of sentence and or all but the first six letters of word. The input can be either a word or sentence, and the output will be respective to the input.

;define beatles for thenext few exercises
(define beatles '(john ringo paul george))
; 8.4
(define (choose-beatles fun)
  (keep fun beatles))
; 8,5
(define (amazify name)
  (word 'the-amazing- name))

(define (transform-beatles fun)
  (every fun beatles))

; 8.6
(define (name-letter letter)
  (cond ((equal? letter 'a) 'alpha)
	((equal? letter 'b) 'bravo)
	((equal? letter 'c) 'charlie)
	((equal? letter 'd) 'delta)
	((equal? letter 'e) 'echo)
	((equal? letter 'f) 'foxtrot)
	((equal? letter 'g) 'golf)
	((equal? letter 'h) 'hotel)
	((equal? letter 'i) 'india)
	((equal? letter 'j) 'juliett)
	((equal? letter 'k) 'kilo)
	((equal? letter 'l) 'lima)
	((equal? letter 'm) 'mike)
	((equal? letter 'n) 'november)
	((equal? letter 'o) 'oscar)
	((equal? letter 'p) 'papa)
	((equal? letter 'q) 'quebec)
	((equal? letter 'r) 'romeo)
	((equal? letter 's) 'sierra)
	((equal? letter 't) 'tango)
	((equal? letter 'u) 'uniform)
	((equal? letter 'x) 'x-ray)
	((equal? letter 'y) 'yankee)
	((equal? letter 'z) 'zulu)
	(else '(unknown letter))))

(define (words wd)
  (every name-letter wd))

; 8.7
(define (letter-count sent)
  (accumulate + (every count sent)))

; 8.8
(define (exaggerate sent)
  (accumulate se (every exag sent)))

(define (exag wd)
  (cond ((number? wd) (* 2 wd))
	((equal? wd 'good) 'great)
	((equal? wd 'bad) 'terrible)
	(else wd)))

; 8.9
; Provide the function for the first argument of each higher order procedure that will return a complete sentence given to the procedure as the second argument.
; every: word
; keep: (define (always-one arg) 1)
; accumulate: sentence

; 8.10
(define (true-for-all? pred sent)
  (if (equal? (count sent) (count (keep pred sent)))
      #t
      #f))

; 8.11
(define (base-grade grade)
  (cond ((equal? (first grade) 'a) 4.0)
	((equal? (first grade) 'b) 3.0)
	((equal? (first grade) 'c) 2.0)
	((equal? (first grade) 'd) 1.0)
	(else 0.0)))
(define (grade-mod grade)
  (cond ((equal? (bf grade) '+) 0.33)
	((equal? (bf grade) '-) (- 0.33))
	(else 0)))

(define (grade-num grade)
  (+ (base-grade grade) (grade-mod grade)))

(define (gpa sent)
  (/ (accumulate + (every grade-num sent)) 
     (count sent)))

; 8.12
(define (um? wd)
  (if (equal? wd 'um)
      #t
      #f))
(define (count-ums sent)
  (count (keep um? sent)))

; 8.13
(define (phone-l2n letter)
  (cond ((or (equal? letter 'a)
	     (equal? letter 'b)
	     (equal? letter 'c))
	 2)
	((or (equal? letter 'd)
	     (equal? letter 'e)
	     (equal? letter 'f))
	 3)
	((or (equal? letter 'g)
	     (equal? letter 'h)
	     (equal? letter 'i))
	 4)
	((or (equal? letter 'j)
	     (equal? letter 'k)
	     (equal? letter 'l))
	 5)
	((or (equal? letter 'm)
	     (equal? letter 'n)
	     (equal? letter 'o))
	 6)
	((or (equal? letter 'p)
	     (equal? letter 'q)
	     (equal? letter 'r)
	     (equal? letter 's))
	 7)
	((or (equal? letter 't)
	     (equal? letter 'u)
	     (equal? letter 'v))
	 8)
	((or (equal? letter 'w)
	     (equal? letter 'x)
	     (equal? letter 'y)
	     (equal? letter 'z))
	 9)
	(else '(unknown))))

(define (unspell phone_word)
  (accumulate word (every phone-l2n phone_word)))

; 8.14
(define (subword wd start end)
  ((repeated bl (- (count wd) end))
   ((repeated bf (- start 1)) wd)))
