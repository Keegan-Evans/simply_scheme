; 4.4
; a. tries to return two different functions/does not multiply all together,
; instead it simply returns r^3 instead of multiply the first two terms by r^3.

(define (sphere-volume r)
  (* (/ 4 3)
     3.141592654
     (* r r r)))

; b. incorrect attempt to invoke the procedure +
(define (next x)
  (+ x 1))

; c. No formal parameter defined.
(define (square x)
  (* x x))

; d. base and height which are used as formal parameters are not defined as such
; in the definition of the function. If you wanted to call triangle instead of
; the actual base and heights, you would need another function(or atleast
; procedure used within the function to get the base and height out of triangle,
; given that the way triangle was given allowed you to determine this.
(define (area-triangle base height)
  (* 0.5 base height))

; e. Trying to use compound expressions as formal parameters
(define (sum-of-squares x y)
  (+ (square x) (square y)))

; 4.5
(define (f2c f)
  (* (/ 5.0 9)
     (- f 32)))

(define (c2f c)
  (+ (* (/ 9 5.0)
	c)
     32))

(define (convert-temperature temp original-temp)
  (cond ((equal? original-temp "c") (c2f temp))
    	((equal? original-temp "f") (f2c temp))
	(else '(I don't know that scale))))

; 4.6a
(define (fourth x)
  (* x x x x))

; 4.6b 
(define (fourth x)
  (square (square x)))

; 4.7 
(define (abs-val x)
  (sqrt (square x)))

; 4.8
(define (scientific num exponent)
  (* num (expt 10 exponent)))

; 4.8-challenge
(define (sci-exponent num)
  (floor (/ (log num)
	    (log 10))))

(define (sci-coefficient num)
  (/ num 
     (expt 10 (sci-expt num))))

; 4.9
(define (discount price percentage)
  (* price
     (- 1 (* 0.01 percentage))))

; 4.10
(define (tip bill)
  (ceiling (* bill 1.15)))
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
; 10.1 It simply picks the first square that it can based on the functions
(define (already-won? triples me)
  (if (empty? (keep 
		     (lambda (triple) (equal? triple ((lambda (wd) 
							(word wd wd wd)) 
						      (opponent me)))) 
		     triples))
      #f
      (se '(Game already won by)  (opponent me))))

; 10.2 (and modified/added to for 10.3)
(define (old-new-tie-game? triples me)
  (if (or (empty? (keep number? triples))
	  (= (count (keep 
		      (lambda (triple) (split-pair? triple me)) 
		      triples))
	     8))
     '(The game is tied)
     #f))

(define (tie-game? position me)
  (let ((triples (find-triples position)))
  (if (or (empty? (keep number? triples))
	  (= (count (keep 
		      (lambda (triple) (split-pair? triple me)) 
		      triples))
	     8))
     '(The game is tied)
     #f)))

; 10.3
(define (split-pair? triple me)
  (and (>= (appearances me triple) 1)
       (>= (appearances (opponent me) triple) 1)))

; 10.4
; a. What if you could win the game by having three squares forming an L shape in a corner (ex squares 1, 2, and 4)?

; new triples
(define (find-triples-corners position)
  (every (lambda (comb) (substitute-triple comb position))
	 '(123 456 789 147 258 369 159 357 412 236 698 874)))

(define (ttt-corners position me)
  (ttt-choose (find-triples-corners position) me))                         

(define (find-triples-no-diag position)
  (every (lambda (comb) (substitute-triple comb position))
	   '(123 456 789 147 258 369)))


; TESTS for all of the modifications
(define new-rules '(ttt-corners ttt-no-diagonals ttt-four-corners))

(define (use-new-rules rules position me)
  (ttt-choose (rules position) me))

(define (test-new-rules new-rules me)
  (every (lambda (tp) (use-new-rules new-rules tp me)) test-positions))

(define test-positions
  '(
    xxo_oo___
    xx_oo____
    ))

; predicted test results
(define expected-corners '(4 3))
(define expected-no-diag '(4 3))

; CONCLUSIONS
; 3 around the corners should still work
; Having one in each of the four corners would not work well, as procedures such as my-single? rely on the number of square in a triple being 3.

; 10.5
;Bwwahhaha, 'o stwongbad!               
; stuff from the chapter

(define (explode wd)
  (if (empty? wd)
      '()
      (se (first wd) (explode (bf wd)))))

(define (first-two wd)
  (word (first wd) (first (bf wd))))

(define (letter-pairs wd)
  (if (<= (count wd) 1)
      '()
      (se (first-two wd) (letter-pairs (bf wd)))))

; EXERCISES
; 11.1
; Write downup4 with only sentence and word primitive procedures
(define (downup4 wd)
  (se wd
      (bl wd)
      (bl (bl wd))
      (first wd)
      (bl (bl wd))
      (bl wd)
      wd))

; 11.2
(define (count-ums sent)
  (cond ((empty? sent) 0)
	((equal? 'um (first sent))
	 (+ 1 (count-ums (bf sent))))
	(else (count-ums (bf sent)))))

; 11.3
(define (unspell-letter letter)
  (cond ((member? letter 'abc) 2)
        ((member? letter 'def) 3)
        ((member? letter 'ghi) 4)
	((member? letter 'jkl) 5)  
	((member? letter 'mno) 6) 
	((member? letter 'prs) 7) 
	((member? letter 'tuv) 8) 
	((member? letter 'wxy) 9) 
	(else 0)))

(define (phone-unspell wd)
  (if (= (count wd) 1)
      (unspell-letter wd)
      (word (unspell-letter (first wd)) 
	    (phone-unspell (bf wd)))))

; 11.5
(define (initials sent)
  (if (empty? sent)
      '()
      (se (first (first sent)) 
	  (initials (bf sent)))))

; 11.6
(define (countdown num)
  (if (= num 0)
      'BLASTOFF!
      (se num (countdown (- num 1)))))

; 11.7
(define (copies num wd)
  (if (= num 0)
      '()
      (se wd 
	  (copies (- num 1) wd))))
; 12.1
; Original function
(define (addup-org nums)
  (if (empty? (bf nums))
      (first nums)
      (+ (first nums) (addup (bf nums)))))

; simpler base case
(define (addup nums)
  (if (empty? nums)
      0
      (+ (first nums) (addup (bf nums)))))

; 12.2
; Fix the bug in the following definition:
(define (acronym-wrong sent)
  (if (= (count sent) 1)
      (first sent)
      (word (first (first sent))
	    (acronym-wrong (bf sent)))))

; corrected, though could still be shorter:
(define (acronym-better sent)
  (if (= (count sent) 1)
      (first (first sent))
      (word (first (first sent))
	    (acronym-better (bf sent)))))

; Corrected, simpler base case
(define (acronym sent)
  (if (empty? sent)
      ""
      (word (first (first sent))
	    (acronym-better (bf sent)))))

; 12.3
; Can you reduce the base case of factorial to -1?
; I don't think this will work, as when n = 0, you will end up multiplying all of the values up to that point by 0, which will result in the output being 0 everytime.

(define (factorial-1 n)
  (if (= n -1)
      1
      (* n (factorial-1 (- n 1)))))

; That is what ended up happening.

; 12.4
; This should end up reversing the order of the sentence
(define (f sent)
  (if (empty? sent)
      sent
      (se (f (bf sent)) (first sent))))

; 12.5
(define (exaggerate sent)
  (cond ((empty? sent) '())
	((number? (first sent))
	 (se (* 2 (first sent)) (exaggerate (bf sent))))
	((equal? (first sent) 'good)
	 (se 'great (exaggerate (bf sent))))
	((equal? (first sent) 'bad)
	 (se 'terrible (exaggerate (bf sent))))
	(else (se (first sent) (exaggerate (bf sent))))))

; 12.6
(define (base-grade grade)
  (define grade-letter (first grade))
  (cond ((equal? grade-letter 'a) 4)
	((equal? grade-letter 'b) 3)
	((equal? grade-letter 'c) 2)
	((equal? grade-letter 'd) 1)
	(else 0)))

(define (grade-modifier grade)
  (cond ((equal? (last grade) '+) 0.33)
	((equal? (last grade) '-) (- 0.33))
	(else 0)))

(define (gpa grades)
  (if (= (count grades) 1)
    (+ (base-grade (first grades))
       (grade-modifier (first grades)))
    (/ (+ 
	 (+ (base-grade (first grades))
	    (grade-modifier (first grades)))
	 (gpa (bf grades)))
       2)))

; 12.7
; helper procedure
(define (spell-digit digit)
  (item (+ 1 digit)
	'(zero one two three four five six seven eight nine)))

(define (spell-number num)
  (if (empty? num)
    '()
    (se (spell-digit (first num)) (spell-number (bf num)))))

; 12.8
(define (numbers sent)
  (cond ((empty? sent) '())
	((number? (first sent))
	 (se (first sent) (numbers (bf sent))))
	(else 
	  (se (numbers (bf sent))))))

; 12.9
; real-word? helper procedure
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (real-words sent)
  (cond ((empty? sent) '())
	((member? (first sent) '(a the an in of and for to with))
	 (real-words (bf sent)))
	(else (se (first sent) (real-words (bf sent))))))

; 12.10
(define (remove wd sent)
  (cond ((empty? sent) '())
	((equal? (first sent) wd) (remove wd (bf sent)))
	(else (se (first sent) (remove wd (bf sent))))))

; 12.11
(define (count sent)
  (if (empty? sent)
    0
    (+ 1 (count (bf sent)))))

; 12.12

; helper function, romnan-value
(define (roman-value letter)
  (cond ((equal? letter 'i) 1)
	((equal? letter 'v) 5)
	((equal? letter 'x) 10)
	((equal? letter 'l) 50)
	((equal? letter 'c) 100)
	((equal? letter 'd) 500)
	((equal? letter 'm) 1000)
	(else 'huh?)))

(define (roman_less primary subsequent)
  (if (< (roman-value primary) (roman-value subsequent))
    1
    0))

(define (arabic num)
  (cond ((= (count num) 0) 0)
	((and
	  (> (count num) 1)
	  (<= (roman-value (first num))
	     (roman-value (first (bf num)))))
	 (- (arabic (bf num))
	    (roman-value (first num))))
	(else (+ (roman-value (first num))
		  (arabic (bf num))))))

; 12.13
(define (describe-time secs)
  (cond
    ((> (floor (/ secs 3153600000)) 1)
	 (se (floor (/ secs 3153600000)) 
	     'centuries
	     (describe-time (remainder secs 3153600000))))
	((= (floor (/ secs 3153600000)) 1)
	 (se (floor (/ secs 3153600000)) 
	     'century
	     (describe-time (remainder secs 3153600000))))
	((> (floor (/ secs 31536000)) 0)
	 (se (floor (/ secs 31536000)) 
             'years
             (describe-time (remainder secs 31536000))))
	((> (floor (/ secs 86400)) 0)
         (se (floor (/ secs 86400)) 
             'days
             (describe-time (remainder secs 86400))))
	((> (floor (/ secs 3600)) 0)                 
         (se (floor (/ secs 3600)) 
             'hours
             (describe-time (remainder secs 3600))))
	((> (floor (/ secs 60)) 0)                
         (se (floor (/ secs 60)) 
             'minutes
             (describe-time (remainder secs 60))))
	(else (se secs 'seconds))))
; 13.1

(define (trace-exploded-ape)
  (trace explode)
  (explode 'ape))

; 3 recursive calls, "pe", "e", " "
; > (explode 'ape)
; | > (explode 'ape)
; | | > (explode 'pe)
; | | | > (explode 'e)
; | | | | > (explode "")
; | | | | ()
; | | | (e)
; | | (p e)
; | (a p e)
; (a p e)

; 13.2

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
    (word wd 'ay)
    (pigl (word (bf wd) (first wd)))))

; 4:
; first one takes 'throughout and hands out (pigl 'hroughoutt)
; second takes 'hroughoutt and outputs (pigl 'roughoutth)
; third takes 'roughoutth and outputs (pigl 'oughoutthr)
; fourth takes 'oughoutthr and returns 'oughoutthray

; 13.3
; Each time downup is recursively called, one letter is removed from the
; input word. When the word only consists of the one letter, the recursive
; tries to call downup with the word less one letter, which is an empty
; which is not an allowed input to `but-last`, leading to an error being
; thrown.

; 13.4

; Because you are not changing n, therefore it cannot move any closer to
; or reach the base case, so the situation applies even to negative
; numbers.

; 13.5

(define (downup wd)
  (if (= (count wd) 1)
    (se wd)
    (se wd (downup (bl wd)) wd)))

; This wouldn't work, because information (the original word) is lost when
; the procedure is invoked again. 

; 13.6

; The "little person" evaluating (factorial 2) has determined this value
; and now hands that value to the "little person evaluating (factorial 3)"
; who asked for this value, who can no determine the value of (factorial 3)
; and so on up the line.

; 14.0a
(define (letter-pair letter wd)
  (if (empty? wd)
    '()
    (se (word letter (first wd)) (letter-pair letter (bf wd)))))

(define (pair-every-letter-helper let-num wd)
  (if (= let-num 0)
    '()
    (se (letter-pair (item let-num wd) wd) 
	(pair-every-letter-helper (- let-num 1) wd))))

(define (pair-every-letter wd)
  (pair-every-letter-helper (count wd) wd))

; 14.1-keep
(define (remove-once wd sent)
  (cond ((empty? sent) '())
	((equal? wd (first sent))(bf sent))
	(else (se (first sent) 
		  (remove-once wd (bf sent))))))

; 14.2 every
(define (up wd)
  (up-helper 1 wd))

(define (up-helper num wd)
  (if (= num (count wd)) 
    wd
    (se ((repeated bl (- (count wd) num)) wd)
	(up-helper (+ 1 num) wd))))

; 14.3 accumulate

(define (remdup sent)
  (remdup-helper '()  sent))

(define (remdup-helper in-words sent)
  (cond ((empty? sent) in-words)
	((member? (first sent) in-words)
	 (remdup-helper in-words (bf sent)))
	(else (remdup-helper 
		(se in-words (first sent)) 
		(bf sent)))))

; 14.4 keep
(define (odds sent)
  (odds-helper 1 sent))

(define (odds-helper bool sent)
  (cond ((empty? sent) '())
	((= bool 1) 
	 (se (first sent) (odds-helper 0 (bf sent))))
	(else (odds-helper 1 (bf sent)))))

; 14.5 accumulate
(define (letter-count sent)
  (if (empty? sent)
    0
    (+ (count (first sent)) (letter-count (bf sent)))))

; 14.6 every

(define (member-rec? wd group)
  (cond ((empty? group) #f)
	((equal? wd (first group)) #t)
	(else (member-rec? wd (bf group)))))

; 14.7 every

(define (differences num-sent)
  (if (= (count num-sent) 1) 
    '()
    (se (- (first (bf num-sent)) (first num-sent))
	(differences (bf num-sent)))))

; 14.8 every

; helper procedure to manage number of times word is repeated
(define (repeat-word num wd)
  (if (= num 0) 
    '()
    (se wd (repeat-word (- num 1) wd))))

(define (expand sent)
  (cond ((empty? sent) '())
	((and (number? (first sent))
	      (= (count sent) 1))
	 (se (first sent)))
	((number? (first sent))
	 (se (repeat-word (first sent) (first (bf sent)))
	     (expand (bf (bf sent)))))
	(else (se (first sent)
		  (expand (bf sent))))))

; 14.9

(define (location wd sent)
  (location-helper 1 wd sent))

(define (location-helper ct wd sent)
  (cond ((empty? sent) #f)
	((equal? (first sent) wd) ct)
	(else (location-helper (+ ct 1) wd (bf sent)))))

; 14.10 accumulate

(define (count-adjacent-duplicates sent)
  (cad-helper 0 sent))

(define (cad-helper ct sent)
  (cond ((= (count sent) 1) ct)
	((equal? (first sent) (first (bf sent)))
	 (cad-helper (+ ct 1) (bf sent)))
	(else (cad-helper ct (bf sent)))))

; 14.11 every

(define (remove-adjacent-duplicates sent)
  (cond ((empty? sent) '())
	((= (count sent) 1) (first sent))
	((equal? (first sent) (first (bf sent)))
	 (se (first sent) 
	     (remove-adjacent-duplicates (bf (bf sent)))))
	(else (se (first sent) 
		  (remove-adjacent-duplicates (bf sent))))))

; 14.12 every

(define (progressive-squares? num-sent)
  (cond ((= (count num-sent) 1) #t)
	((= (square (first num-sent)) (first (bf num-sent)))
	 (progressive-squares? (bf num-sent)))
	(else #f)))

; 14.13 every

; (define (pigl wd)
;   (if (member? (first wd) 'aeiou)
;     (word wd 'ay)
;     (pigl (word (bf wd) (first wd)))))

(define (robust-pigl wd)
  (pigl-helper (count wd) wd))

(define (pigl-helper ct wd)
  (cond ((= ct 0) (word wd 'ay))
	((member? (first wd) 'aeiou)
	 (word wd 'ay))
	(else (pigl-helper 
		(- ct 1) 
		(word (bf wd) (first wd))))))

; 14.14 

(define (same-shape? sent1 sent2)
  (cond ((and (empty? sent1) (empty? sent2))
	 #t)

    	((not (= (count sent1) (count sent2)))
	 #f)
	
	((not (= (count (first sent1))
		 (count (first sent2))))
	 #f)
		
	(else (same-shape? (bf sent1) (bf sent2)))))
	
; 14.15

(define (merge sent1 sent2)
  (cond ((empty? sent1) sent2)
	((empty? sent2) sent1)
	((before? (first sent1) (first sent2))
	 (se (first sent1) (merge (bf sent1) sent2)))
	(else (se (first sent2) (merge sent1 (bf sent2))))))

; 14.16 

(define (syllables wd)
  (syllable-helper 0 wd))

(define (syllable-helper ct wd)
  (cond ((empty? wd) ct)
	
	; single syllable for silent e
	((silent-e? wd)
	 (syllable-helper (+ ct 1) ((repeated bf 3) wd)))
	
	; 3 adjancent vowels syllable
	((three-con-vowels? wd)
	 (syllable-helper (+ ct 1) ((repeated bf 3) wd)))

	; 2 adjacent vowels syllable
	((and (> (count wd) 1)
	      (vowel? (first wd)) 
	      (vowel? (first (bf wd))))
	 (syllable-helper (+ ct 1) (bf (bf wd))))

	; single syllable per vowel
	((vowel? (first wd))
	 (syllable-helper (+ ct 1) (bf wd)))
	
	(else (syllable-helper ct (bf wd)))))

(define (silent-e? wd)
  (if (and (>= (count wd) 3)
	   (vowel? (first wd))
	   (not (vowel? (first (bf wd))))
	   (equal? (first (bf (bf wd))) 'e))
    #t
    #f))

(define (three-con-vowels? wd)
  (if (and (>= (count wd) 3)
	   (vowel? (first wd))
	   (vowel? (first (bf wd)))
	   (vowel? (first (bf (bf wd)))))
    #t
    #f))

; PROJECT: SPELLING NAMES OF HUGE NUMBERS
; See separate file

; 15.1

(define (to-binary num)
  (cond ((= num 0) "")
	((odd? num) (word (to-binary (/ (- num 1) 2)) 1))
	(else (word (to-binary (/ num 2)) 0))))
; 15.2

(define (sent-to-word sent)
  (if (empty? sent)
    ""
    (word (first sent) 
	  (sent-to-word (bf sent)))))

(define (palindrome?-helper wd)
  (cond ((<= (count wd) 1) #t)
	((not (equal? (first wd) (last wd)))
	 #f)
	(else (palindrome?-helper (bf (bl wd))))))

(define (palindrome? sent)
  (palindrome?-helper (sent-to-word sent)))

; 15.3 substring

; First try:

(define (substrings-first-try wd)
  (if (empty? wd)
    ""
    (se (substrings-first-try-helper 
	  (first wd)
	  (bf wd))
	(substrings-first-try (bf wd)))))

(define (substrings-first-try-helper beginning end)
  (if (= (count end) 0)
    '()
    (let ((so-far (word beginning (first end))))
      (se so-far (substrings-first-try-helper so-far (bf end))))))

; second
(define (diminish wd)
  (if (empty? wd)
    (se "")
    (se wd (diminish (bl wd)))))

(define (substrings wd)
  (single-instance
    (if (empty? wd)
      (se "")
      (se (diminish wd)
          (substrings (bf wd))))))

(define (single-instance-helper elements-so-far sent)
  (cond ((empty? sent)
	 elements-so-far)
	((member? (first sent) elements-so-far)
	 (single-instance-helper elements-so-far (bf sent)))
	(else (single-instance-helper
		(se elements-so-far (first sent))
		(bf sent)))))

(define (single-instance sent)
  (single-instance-helper '() sent))


; 15.4

(define (substring? is-a-substring-of-wd wd)
  (member? is-a-substring-of-wd (substrings wd)))

; 15.5 

;(define (phone-spell phone-num)
;  (if (empty? phone-num)
;    '()
;    (let ((after (phone-spell (bf phone-num))))
;      (se 
(define (letters num)
  (cond ((= num 2) '(a b c))
	((= num 3) '(d e f))
	((= num 4) '(g h i))
	((= num 5) '(j k l))
	((= num 6) '(m n o))
	((= num 7) '(p q r s))
	((= num 8) '(t u v))
	((= num 9) '(w x y z))
	(else num)))

(define (prepend-each a b)
  (if (empty? a)
    '()
    (se (prepend-every (first a) b)
	(prepend-each (bf a) b))))

(define (phone-spell phone-num)
  (if (empty? phone-num)
    (se "")
    (se
      (prepend-each (letters (first phone-num))
		    (phone-spell (bf phone-num))))))


; 15.6

(define (unscramble sent) 
  (cond ((empty? sent) '())
	((= (count sent) 2)
	 sent)
	((equal? (first sent) 'the)
	 (se (unscramble (bf (bf (bl sent))))
		  (se 'that
		      (last sent) 
		      'the
		      (first (bf sent)))))
	(else (se (first sent)
		  (unscramble (bf sent))))))

; Chapter 16

; 16.1

(define (match_three sent)
    (if (match '(! ! !) sent)
	  #t
	  #f))

; 16.2

(define (two_sub_sent? sent)
     (if (equal? (match '(*sub *sub) sent) 'failed)
		 #f
		 #t))

; 16.3 Design and test a pattern that matches any sentence of no more
; than three words.

(define (at_least_three_words? sent)
    (if (equal? (match '(? ? ?) sent) '())
	  #t
	  #f))

; 16.4 Design and test a pattern that matches any sentence of at least
; three words.

(define (at_most_three? sent)
    (if (equal (match '(& & &) sent) '())
	#t
	#f))

; 16.5 Show sentences of length 2, 3, and 4 that match the pattern
; (*x *y *y *x):
; (a a)
; (a b b a)
; There are no sentences of length 3 that can match, because if there is
; a single match, then there must be two of that same sentence.

; 16.6 Show sentences of length 2, 3, and 4 that match the pattern:
; (*x *y &y &x)
; No sentences of length 2, as this call requires there to be at least
; two distinct senteneces of at least one unique word each, and since these must be paired this is not
; possible to have only a two word sentence that will match.
; 3x Words: The same reasoning as in 16.5 and also for the 2x word
; sentence in this example.
; 4x words: '(a b b a)

; 16.7 List the sentences of length 6 or less, starting with , that
; match the pattern (*x *y *y *x)
; '(a b b a), '(a b a b) '(a a a a) '(a a a a a a) '(a b a a b a) '(b a
; b b a b) '(a b c a b c) '(a b c b c a)

; CHAPTER 16 IMPLEMENTATION QUESTIONS

; 16.8 If the sentence is empty, it first looks to see what the minimum
; value is. If there is no minimum(that is, min = 0), then it adds the
; blank sentence, '(), to the known values and returns the results from
; giving the rest of the pattern to `match-using-known-values`. If the
; minimum is one, then it returns failed.

; 16.9 Pattern = '(evil *one dog) '(evil tubby dog ran)

; 16.10 If it is within the words that would be captured by a special
; character then it is retained. However, it if is elsewhere in the
; phrase it must have a character in the pattern if it is not to fail.

; 16.11: a) 4 b) 15 c) 4 d) 5 e) 13 f) 8
; More special wildcards and wildcards near the beginning of the pattern
; make things more difficult

; 16.12 '(* *blank-known)

; 16.13 '(i like to listen to *band when i ?action) 
;       '(i like to listen to the beatles when i drive)

; you would end up with a blank word ("") inserted as a name in
; known-values ,which could create problems if there was more than one
; unnamed placeholder in the pattern. The part that prevents this is
; directly in the (add) function. It checks to see if the name is empty
; and only if it is not does it add it to known values.

; 16.15 Because they are both called by (lookup), which will check to
; see if known-values is empty. If it is, then neither will be called.
; If they are called, then they will both return a value before
; known-values is empty.

; 16.16 Because we want it to return #f if it is empty and howmany is
; not ? or *.

; 16.17 The call to (match-using-known-values) by the initial (match)

; 16.18 

; 1) The first words of the sent argument musut match the old value in
;    the database. '(?place me ?place) '(give me fun)

; 2) The partial pattern that remains after the placeholder must match
;    the rest fo the sentence. '(?blank for ?blank s sake) '(love for
;    love is doomed)

; 3) The old value must be consistent with the number of words permitted
;    by the howmany part of the placeholder. '(!adj men see &adj things)
;   '(funny men see very funny things)

; 16.19 It will fail because get-value will read the ! in known-values
; and return '(). We need someway to differentiate between the symbol !
; as used by the program and the symbol ! read a word from the sentence
; known-values/the placeholder.

; 16.20 and 16.21 have functions added to the "my_match.scm" file to
; enact these additional functionality. I probably should have written
; what I could have as seperate files and then made sure to document the
; other changes, but I am fairly happy with how they have worked out.

; 16.22 Yep, VIM definitely does.

; 17.1 

; a) Rod
; b) Chris
; c) (Chris Colin Hugh Paul)
; d) Error as it is expecting a list
; e) ((Rod Argent) Chris White)
; f) ((rod argent) (chris white))
; g) (rod)
; h) (colin blunstone)
; i) #f

; 17.2

(define (f1 lst1 lst2)
    (list (append (cdr lst1) (list (car lst2)))))

(define (f2 lst1 lst2)
    (list (cdr lst1) (cadr lst2)))

(define (f3 lst1 lst2)
    (append lst1 lst1))

(define (f4 lst1 lst2)
    (list
	  (list (car lst1)
	        (car lst2))
	  (append (cdr lst1)
	          (cdr lst2))))

; 17.3 It returns a list of procedures, with each procedure being
; (lambda (y) (+ n y)), where in is one of the values taken from the
; mapped list of numeric values. You can access them using the car and
; cdr functions to select the correct one from the list and using that
; as the procedure you are calling, for example to get a return value of
; 3 from the second function (which is (lambda (y) (+ 2 y)):
;
; ((cadr (map (lambda (x) (lambda (y) (+ x y))) '(1 2 3 4))) 1)

; REAL EXERCISES
; 17.4 I think it will reverse the order of the list

(define (mystery 1st)
  (mystery-helper 1st '()))

(define (mystery-helper 1st other)
  (if (null? 1st)
      other
	  (mystery-helper (cdr 1st) (cons (car 1st) other))))

; Which it does.

; 17.5

(define (max2 a b)
    (if (> b a) b a))
(define (max . numbers)
    (cond ((equal? (count numbers) 1)
	       (car numbers))
		  ((> (car numbers) (cadr numbers))
		   (apply max (cons (car numbers) (cddr numbers))))
		  (else (apply max (cdr numbers)))))

; 17.6

; First the version with only 2 arguments
(define (mypend1 a b)
    (if (null? a)
	    b
		(cons (car a) (mypend1 (cdr a) b))))
; Version with any number of arguements. I think I will try to clean
; this up, as currently it is very mess, but it does work. 
(define (mypend fin . subin)
  (cond ((null? fin)
         (mypend (car subin) (cdr subin)))
        ((null? (cdr subin))
		 (mypend1 fin (car subin)))
		(else (apply mypend
		        (cons (mypend1 fin (car subin))
				      (cdr subin))))))

; 17.7					  

(define (myse . part)
   (cond ((empty? (car part))
          '())
		 ((word? (car part))
		  (append (list (car part))
		          (myse (cdr part))))
		 (else (append (myse (caar part) (cdar part))
		               (myse (cdr part))))))

; 17.8

(define (alt-member el lst)
    (cond ((null? lst)
	       #f)
		  ((equal? el (car lst))
	       (cdr lst))
		  (else (alt-member el (cdr lst)))))

; 17.9

(define (mlf lst num)
    (if (equal? num 0)
	    (car lst)
		(mlf (cdr lst) (- num 1))))

; Just a little something to find from the index of the reverse
(define (rev-list-ref lst num)
    (mlf lst (- (length lst) num)))

; 17.10

(define (mylength lst)
    (lh lst 0))
	
;(define (length-helper lst cnt)
;  (if (null? (car lst))
;      (+ cnt 1)
;      (length-helper (cdr lst) (+ cnt 1)))

(define (lh lst cnt)
  (cond ((null? lst)
         cnt)
	    (else (lh (cdr lst) (+ cnt 1)))))

; 17.11

(define (before-in-list? lst fw sw)
  (cond ((null? lst) #f)
        ((equal? (car lst) sw) #f)
		((equal? (car lst) fw) #t)
		(else (before-in-list? (cdr lst) fw sw))))
		
; 17.12

(define (flatten lst)
  (cond ((null? lst) '())
        ((word? (car lst))
		 (cons (car lst) (flatten (cdr lst))))
        (else (append (flatten (car lst))
		              (flatten (cdr lst))))))

; 17.13
(define (deep-count lst)
    (cond ((null? lst) 0)
	      ((word? lst) 1)
		  (else (+ (deep-count (car lst))
		           (deep-count (cdr lst))))))

; 17.14

(define (branch num-lst lst)
  (cond ((null? num-lst)
         lst)
		(else 
		  (branch 
		    (cdr num-lst) 
			(car ((repeated cdr (- (car num-lst) 1)) lst))))))

; 17.15

(define (lookup name known-values)
  (cond ((null? known-values) 'no-value)
	((equal? (caar known-values) name)
	 (cadar known-values))
	(else (lookup name (cdr known-values)))))

(define (add name value known-values)
  (if (empty? name)
      known-values
      (cons (cons name value) known-values)))

; 17.16

(define (valid-infix? ex)
  (cond ((and (equal? (length ex) 1)
             (or (number? (car ex))
			     (valid-infix? (car ex)))) #t)
        ((and (number? (car ex))
		      (member (cadr ex) '(+ - / *)))
	     (valid-infix? (cddr ex)))
		(else #f)))


; 18.1 The leaf node san francisco, the inner set of parentheses
; indicates the sublist that is the datum of the node. This is because
; San Francisco is two seperate words, which were it not returned as a
; list, would return a datum of San. It also would have worked to input
; "San-Francisco" or "SanFrancisco" to denote the same thing. 

; 18.2
; Calls to the remaining portion have to be called with cadr instead of
; cdr, as it is a sublist instead of seperate elements of the same list.


; define a test list to use

(define tree-test-list 
  (make-node
   'animals
   (list (make-node 
          'mammals 
           (list (make-node 'rodents 
		           (list (make-node 'rat '())
				         (make-node 'beaver '())))
			     (make-node 
				   'artiodactyla
				   (list 
				    (make-node 'whales 
					  (list (make-node 'blue-whale '())
					        (make-node 'killer-whale '())))
				    (make-node 
					  'pecora
				      (list 
					    (make-node 'cervids 
						  (list (make-node 'elk '())
						        (make-node 'deer '())
								(make-node 'antelope '())))
						(make-node 'bovids 
						  (list (make-node 'cows '())
						        (make-node 'goats '())))))))))
		 (make-node
		   'reptiles 
		   (list
			(make-node
			  'archosaurs
			  (list (make-node 'crocodilians 
			          (list (make-node 'crocodiles '())
					        (make-node 'alligators '())))
					(make-node 'birds 
					  (list (make-node 'ducks '())
					        (make-node 'finches '()))))))))))

; and a small test list

(define small-tree-test-list 
  (make-node 'alphabet
    (list (make-node 'consonants
	        (list (make-node 'b '())
			      (make-node 'c '())))
		  (make-node 'vowels
		    (list (make-node 'a '())
			      (make-node 'e '()))))))
; 18.3

(define (depth tree)
  (if (null? (children tree))
      1
	  (+ 1 (depth-across (children tree)))))


(define (depth-across forest)
  (cond ((null? forest) 0)
        (else (max (depth (car forest))
		           (depth-across (cdr forest))))))

; 18.4 

(define (count-nodes tree)
  (if (null? (children tree))
      1
	  (+ 1 (count-nodes-in-forest (children tree)))))

(define (count-nodes-in-forest forest)
  (if (null? forest)
      0
	  (+ (count-nodes (car forest))
	     (count-nodes-in-forest (cdr forest)))))

; 18.5 Write prune, a procedure that takes a tree as argument and returns a copy of the
; tree, but with all the leaf nodes of the original tree removed. (If
; the argument to prune
; is a one-node tree, in which the root node has no children, then should return #f
; because the result of removing the root node wouldn’t be a tree.)

(define (prune tree)
    (cond ((list? (datum (car (children tree))))
	   (datum tree))
	  (else (list (datum tree)
		      (prune-along (children tree))))))

(define (prune-along . branches)
    (cond ((null? (cdr branches))
	   (prune (caar branches)))
          (else (append (prune (car tree))
	                (prune-along (cdr tree))))))

;(define (prune tree)
;  (cond ((not (null? (children tree)))
;	 (cons (car tree)
;	       (prune-down (car (children tree))
;				(cadr (children tree)))))))
;
;(define (prune-down . branches)
;  (cond ((null? (cdr branches))
;	 (prune (car branches)))
;	(else (cons
;    (prune (car branches))
;    (prune (cadr branches))))))
;

;(define (prune tree)
;  (if (leaf? tree)
;      '()
;      (list (list (car tree)) (map prune (children tree)))))
;(define (leaf? node)
;  (null? (children node)))

(define (prune tree)
  (cond ((null? (children tree))
         #f)
		(else (cons (datum tree)
		            (prune-forest (children tree))))))

(define (prune-forest forest)
  (cond ((null? forest)
         '())
        ((null? (children (car forest)))
		 (prune-forest (cdr forest)))
		(else (cons (prune (car forest))
		            (prune-forest (cdr forest))))))

; 18.6 
; compute to call the parse function with
(define (compute tree)
  (if (number? (datum tree))
      (datum tree)
      ((function-named-by (datum tree))
         (compute (car (children tree)))
         (compute (cadr (children tree))))))

(define (function-named-by oper)
  (cond ((equal? oper '+) +)
	((equal? oper '-) -)
	((equal? oper '*) *)
	((equal? oper '/) /)
	(else (error "no such operator as" oper))))

; parse-scheme

(define (parse-scheme expr)
    (cond ((null? expr)
	       '()) 
		  ((list? (car expr))
	       (cons (parse-scheme (car expr))
		         (parse-scheme (cdr expr))))
          ((number? (car expr))
		   (cons (list (car expr))
			     (parse-scheme (cdr expr))))
          (else (cons (car expr)
		              (parse-scheme (cdr expr))))))

; 19.1

; every produces a list of words
; map produces a list of lists, each containing everything but the first
; list object in the original individual list. This is because every
; operates using sentences  and words where as map simply looks at
; lists and their objects

; 19.2

(define (keep_helper fn in_lst type base)
    (cond ((empty? in_lst) 
	       base)
		  ((fn (first in_lst))
		   (type (first in_lst) (keep_helper fn (bf in_lst) type base)))
		  (else (keep_helper fn (bf in_lst) type base))))

(define (my_keep fn lst)
    (cond ((word? lst)
	       (keep_helper fn lst word ""))
		  ((sentence? lst)
		   (keep_helper fn lst sentence '()))
		  (else '(incorrect input type))))

; 19.3

(define (three-arg-accumulate fn base lst)
  (if (empty? lst)
      base
	  (fn (first lst) (three-arg-accumulate fn base (bf lst)))))

; 19.4

(define (left-accumulate fn lst)
  (if (empty? (bf lst))
      (first lst)
	  (fn (left-accumulate fn (bl lst)) (last lst))))

; 19.5

(define (new-true-for-all? pred sent)
  (cond ((empty? sent)
         #t)
		((pred (first sent))
		 (new-true-for-all? pred (bf sent)))
		(else #f)))

; 19.6
(define (true-for-any-pair? pred sent)
  (cond ((null? (cdr sent))
         #f)
		((pred (car sent) (cadr sent))
		 #t)
		(else (true-for-any-pair? pred (cdr sent)))))

; 19.7

(define (true-for-all-pairs? pred sent)
  (cond ((equal? (count sent) 1)
         #t)
		((pred (first sent) (first (bf sent)))
		 (true-for-all-pairs? pred (bf sent)))
		(else #f)))

; 19.8

(define (true-for-all-pairs2? pred sent)
  (if 
    (not (null?	(reduce 
					(lambda (ls ls2)(if 
								(true-for-any-pair? pred (list ls ls2))
								ls
								'()))
					sent)))
	#t
	#f))

; 19.9

(define (hi-sort lst pred)
    (if (null? lst)
	    '()
		(cons (earliest-inst lst pred)
		      (hi-sort (remove-inst (earliest-inst lst pred) lst) pred))))		

(define (earliest-inst lst pred)
  (ei-helper (car lst) (cdr lst) pred))

(define (ei-helper so-far rest pred)
  (cond ((null? rest) so-far)
        ((pred so-far (car rest))
		 (ei-helper so-far (cdr rest) pred))
		(else (ei-helper (car rest) (cdr rest) pred))))

(define (remove-inst inst lst)
  (cond ((null? lst) '())
        ((equal? inst (car lst)) 
		 (cdr lst))
		(else (cons (car lst)
		            (remove-inst inst (cdr lst))))))

; 19.10

(define (deep-tree fn structure)
  (cond ((null? (children structure)) 
         (list (fn (datum structure))))
		(else (cons (fn (datum structure))
		            (deep-tree-across fn (children structure))))))

(define (deep-tree-across fn structure)
  (cond ((null? (cdr structure))
         (deep-tree fn (car structure)))
		(else (list (deep-tree fn (car structure))
		            (deep-tree-across fn (cdr structure))))))

; 19.11

(define (my-repeated fn n)
  (lambda (any-in)
    (let rg ((fn fn) (n n))
      (if (= n 0)
  		  any-in
  		  (fn (rg fn (- n 1)))))))

; 19.12

(define (tree-reduce fn tree)
    (cond ((null? (children tree))
	       (datum tree))
		  (else (fn (datum tree)
		            (tree-reduce-across fn (children tree))))))

(define (tree-reduce-across fn branches)
    (cond ((null? (cdr branches))
	       (tree-reduce fn (car branches)))
		  (else (fn (tree-reduce fn (car branches))
		            (tree-reduce-across fn (cdr branches))))))


; 19.13

(define (deep-map f structure)
  (cond ((word? structure) (f structure))
        ((null? structure) '())
		(else (cons (deep-map f (car structure))
		            (deep-map f (cdr structure))))))


(define (deep-reduce fn lst)
  (cond ((null? lst) (fn))
        ((not (list? lst))
		 lst)
		(else (fn (deep-reduce fn (car lst))
		          (deep-reduce fn (cdr lst))))))

; 20.1 
; (the night before)
; (hello little girl)

; 20.2
; #t

; 20.3

(define (show2 wd)
  (display wd)
  (newline))

; 20.4

(define (converse)
(begin 
  (read-line)
  (display "hello im the computer. who are you? ")
  (let ((name (read-line)))
  (display-sent (se 'hi (first name) "how are you? ")))
  (let ((doing (read-line)))
  (display "That is great"))
  (newline)))

(define (display-sent sent)
  (if (equal? (count sent) 1)
      (display (first sent))
	  (begin (display (first sent))
	         (display " ")
			 (display-sent (bf sent)))))

; 20.5 
(define (longest-last-draft names)
  (let ((nh (lambda (so-far rest)
              (cond ((empty? rest)
			         so-far)
				    ((> so-far (count (cdar rest)))
					 (nh (count cdar rest) (cdr rest)))
					(else (nh so-far (cdr rest)))))))
	   (nh (count (cdar names)) (cdr names))))

(define (longest-last names)
  (count
	(let nh ((so-far (cadar names)) (rest (cdr names)))
		(cond ((empty? rest)
			so-far)
			((< (count so-far) (count (cadar rest)))
			(nh (cadar rest) (cdr rest)))
			(else (nh so-far (cdr rest)))))))

(define russian-composers '(
	(piotr tchaikovsky) 
	(nicolay rimsky-korsakov)
	(sergei rachmaninov) 
	(modest musorgsky)))

(define (let-er lol)
  (let egg ((shell (car lol)) (yolk (cdr lol)))
    (if (empty? yolk)
	    '()
		(cons (word shell (car yolk)) (egg (car yolk) (cdr yolk))))))

(define (name-table names)
  (let name-table-var ((la-names names) 
                       (cnt (+ (longest-last names) 2)))
    (if (null? la-names)
	    'done
		(begin (display (align (cadar la-names) cnt))
		       (show (caar la-names))
			   (name-table-var (cdr la-names) cnt)))))

; 20.6

;(define (ask-user position letter)
;  (print-position position)
;  (display letter)
;  (display "'s move: ")
;  (read))
;
;(define (ask-user position letter)
;  (print-position position)
;  (display letter)G
;  (display "'s move: ")
;  (let ((user-input (read)))
;    (if (and (number? user-input)
;	         (<= user-input 9)
;			 (<= 1 user-input))
;		user-input
;		(begin
;		  (display
;		   "That is an invalid input, please enter a number between 1 and 9")
;		  (ask-user position letter)))))
;
; 20.7	

; if you select a position that you already hold, the program seems to
; skip your turn and if you

(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (let ((user-input (read)))
    (cond 
		((not (equal? (item user-input position) '_))
		(begin
			(display
			"That position has already been choosen, pick a different position")
			(newline)
			(ask-user position letter)))
	    ((and (number? user-input)
	         (<= user-input 9)
			 (<= 1 user-input))
		    user-input)
		(else (begin
		  (display
		   "That is an invalid input, please enter a number between 1 and 9")
		  (ask-user position letter))))))

; 20.8

; 20.9

(define (play-ttt-show-ties x-strat o-strat)
 (play-ttt-helper-show-tie x-strat o-strat '_________ 'x))

(define (play-ttt-helper-show-tie x-strat o-strat position whose-turn)
 (let ((square (if (equal? whose-turn 'x)
		                  (x-strat position 'x)
						  (o-strat position 'o))))
  (cond ((already-won? position (opponent whose-turn))
         (list (opponent whose-turn) 'wins!))
		((tie-game? position whose-turn) 
		 (begin
		   (show square)
		   (display "the game has been tied")))
        (else (play-ttt-helper-show-tie x-strat
                               o-strat
                               (add-move square whose-turn position)
                               (opponent whose-turn))))))

; Chapter 21

; 21.1 Order of Execution. We need to make get-arg is run before
; recursively calling get-args. This is because different versions of
; scheme have different OOE.

; 21.2 Because in-domain? applies the value returned by
; type-predicate to the arguments of the function, thus if we simply had
; #t as this list element, it would try to use #t as a function, which
; it is not.

; 21.3-5 Made changes directly in functions.scm

; 21.6 Because of error checking in get-fn

; 21.7 Because we have to allow any single input, so that scheme can
; evaluate whether the input is a word or not. 

; 21.8 The functions loop procedure/ultimately the string "Thanks for
; using FUNCTIONS", everything else is just displayed text/side effect

; 21.9 It "reduces" invalid inputs until we are left with a valid input
; and returns that.


; Chapter 22

; 22.1 

(define (concatenate file-names out-file)
  (let ((outp (open-output-file out-file)))
    (concatenate-helper file-names outp)
    (close-output-port outp)))

(define (concatenate-helper file-names outp)
  (if (empty? file-names)
      'done
	  (begin (write-ind-file (first file-names outp))
	         (concatenate-helper (bf filenames) outp))))


(define (write-ind-file ind-file outp)
  (let ))