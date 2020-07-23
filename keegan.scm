(load "~/repo/simply_scheme/simply.scm")
(load "~/repo/simply_scheme/functions.scm")

;; function to find all of the functions in a list of functions that have a
;; particular number of arguments

;; look at first on list
;; keep if number-of-arguments returns equal to input 
;; recursively evaluate using (butfirst function_list) to evaluate remainder

