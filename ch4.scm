(load "~/repo/simply_scheme/simply.scm")
(load "~/repo/simply_scheme/functions.scm")

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
