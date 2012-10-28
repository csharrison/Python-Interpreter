#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.


we need to define: 
___assertIn
___assertNotIn
___assertRaises
___assertIs
___fail

|#

(define-type-alias Lib (CExp -> CExp))

(define print-lambda
  (CFunc (list 'to-print) empty
    (CPrim1 'print (CId 'to-print))))

(define assert-true-lambda
  (CFunc (list 'check-true) empty
    (CIf (CId 'check-true) (CTrue) (CError (CStr "Assert failed")))))

(define assert-false-lambda
  (CFunc (list 'check-true) empty
    (CIf (CId 'check-true) (CError (CStr "Assert failed")) (CTrue))))

(define assert-equal-lambda
  (CFunc (list 'first-elt 'second-elt) empty
         (CIf (Compare '== (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert failed")))))
(define assert-not-equal-lambda
  (CFunc (list 'first-elt 'second-elt) empty
         (CIf (Compare '!= (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert failed")))))

(define exception-lambda
  (CFunc (list 'e) empty
         (CError (CId 'e))))
(define true-val
  (CTrue))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True true-val); we do this at parse time, which i think is better
        (bind 'Exception exception-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertNotEqual assert-not-equal-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)))


(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


