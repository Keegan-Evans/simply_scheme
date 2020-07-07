; 13.1

(define (explode wd)
  (if (empty? wd)
    '()
    (se (first wd) (explode (bf wd)))))

(trace explode)

(explode 'ape)

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


