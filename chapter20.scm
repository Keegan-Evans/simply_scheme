(define (bottles n)
  (if (= n 0)
      'burp
      (begin (verse n)
              (bottles (- n 1)))))

(define (verse n)
  (show (cons n '(bottles of beer on the wall)))
  (show (cons n '(bottles of beer)))
  (show '(if one of those bottles should happen to fall))
  (show (cons (- n 1) '(bottles fo beer on the wall)))
  (show '()))

; side effects and sequencing

(define (effect x)
  (show x)
  'done)

(define (value x)
  x)

(define (lots-of-effect x)
  (effect x)
  (effect x)
  (effect x))

(define (lots-of-value x)
  (value x)
  (value x)
  (value x))

; The begin Special Form
; This turns a sequence of expressions into a single. This is what it
; would look like for our bottle procedure:

(define (bottles n)
  (if (= n 0)
      'burp
      (begin (verse n)
             (bottles (- n 1)))))

; This isn't Functional Programming
; A function computes and returns 1 value, with no side effects
; A procedure is the general term for the thing that lambda returns(the
; embodiement of an algorithm). 

; Not moving to the next line:
(define (show-addition x y)
  (display x)
  (display '+)
  (display y)
  (display '=)
  (show (+ x y)))

; show is written from  newline and display. Display shows the specified
; object on the current line and does not change to a new one by itself.

; we can replace the '() in our original with (newline) and the cons and
; lists with (display) and (show).

(define (verse n)
  (display n)
  (show " bottles of beer on the wall" )
  (display n)
  (show " bottles of beer" )
  (show "if one of those bottles should happen to fall" )
  (display (- n 1))
  (show " bottles fo beer on the wall" )
  (newline))

; A Higher-Order Procedure for Sequencing
(define (show-list lst)
  (if (null? lst)
      'done
      (begin (show (car lst))
             (show-list (cdr lst)))))

(define sh-lst '((dig a pony) (doctor robert) (for you blue)))

; this is not a higher order function as it is the computations that we are
; performing with it have side-effects as an integral part of the computing.
; Scheme has for-each which is kind of like map, as the standard implementation
; of this type of procedure, though it has an unspecified return value. You
; can't use map it returns a list with three unspecified values.

; tic-tac-toe-revisted

(define (stupid-ttt position letter)
  (location '_ position))

(define (location letter word)
  (if (equal? letter (first word))
      1
      (+ 1 (location letter (bf word)))))

; Including the already-won? and tie-game?
(define (already-won? position who)
  (member? (word who who who) (find-triples position)))

(define (tie-game? position)
  (not (member? '_ position)))

; a program that takes strategies as inputs and plays game


(define (play-ttt x-strat o-strat)
  (play-ttt-helper x-strat o-strat '_________ 'x))

(define (play-ttt-helper x-strat o-strat position whose-turn)
  (cond ((already-won? position (opponent whose-turn))
         (list (opponent whose-turn) 'wins!))
        ((tie-game? position) '(tie game))
        (else (let ((square (if (equal? whose-turn 'x)
                                (x-strat position 'x)
                                (o-strat position 'o))))
                (play-ttt-helper x-strat
                                 o-strat
                                 (add-move square whose-turn position)
                                 (opponent whose-turn))))))

(define (add-move square letter position)
  (if (= square 1)
      (word letter (bf position))
      (word (first position)
            (add-move (- square 1) letter (bf position)))))

(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (read))

(define (echo)
  (display "What? ")
  (let ((expr (read)))
    (if (equal? expr 'stop)
        'okay
        (begin
          (show expr)
          (echo)))))

; aesthetic board display

(define (print-position position)
  (print-row (subword position 1 3)) 
  (show "-+-+-")
  (print-row (subword position 4 6))
  (show "-+-+-")
  (print-row (subword position 7 9))
  (newline))

(define (print-row row)
  (maybe-display (first row))
  (display "|")
  (maybe-display (first (bf row)))
  (display "|")
  (maybe-display (last row))
  (newline))

(define (maybe-display letter)
  (if (not (equal? letter '_))
      (display letter)
      (display " ")))

(define (subword wd start end)
  ((repeated bf (- start 1))
   ((repeated bl (- (count wd) end))
  wd)))

; Reading and writing Normal Text

(define (music-critic)
  (show "whats your favorite beatles song?")
  (let ((song (read)))
    (show (se "i like" song "too. - one"))))

; this will only read the first scheme expression given

(define (music-critic2)
  (read-line)
  (show "Whats your favorite beatles song?")
  (let ((song (read-line)))
    (show (se "i like" song "too. - two"))))

(define (music-critic3)
  (read-line)
  (show "Whats your favorite Beatles song?")
  (let ((song (read-line)))
    (show-line (se "i like" song "too - three"))))

(define (square-root-table nums)
  (if (null? nums)
      'done
      (begin (display (align (car nums) 7 1))
             (show (align (sqrt (car nums)) 10 5))
             (square-root-table (cdr nums)))))
            
(define (name-table names)
  (if (null? names)
      (newline)
      (begin (display (align (cadar names) 11))
             (show (caar names))
             (name-table (cdr names)))))

            