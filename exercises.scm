;  4.4
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
(define (tie-game? triples me)
  (if (or (empty? (keep number? triples))
	  (= (count (keep 
		      (lambda (triple) (split-pair? triple me)) 
		      triples))
	     8))
     '(The game is tied)
     #f))

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

(define (less_than_three_words? sent)
    (if (equal? (match '(? ? ?) sent) '())
	  #t
	  #f))

; 16.4 Design and test a pattern that matches any sentence of at least
; three words.

; 16.5 Show sentences of length 2, 3, and 4 that match the pattern
; (*x *y *y *x)
; for each length, if no sentence can match the pattern, explain why not.

; 16.6 Show sentences of length 2, 3, and 4 that match the pattern:
; (*x *y &y &x)
; For each length, if no sentence can match the pattern, explain why not.

; 16.7 List the sentences of length 6 or less, starting with , that
; match the pattern

; CHAPTER 16 IMPLEMENTATION QUESTIONS
