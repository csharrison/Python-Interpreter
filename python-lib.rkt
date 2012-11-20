#lang plai-typed

(require "core-syntax.rkt")

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

(define (make-prim op)
  (CFunc (list 'the-arg) (hash empty) (none) (none)
         (CReturn (CPrim1 op (CId 'the-arg)))))

(define (constructor type)
  (CFunc empty (hash empty) (some '-args) (none)
         (CIf (Compare '> (CApp (CId 'len) (list (CId '-args)) (hash empty) (none) (none)) (CNum 0))
              (CIf (Compare '== (CApp (CId 'len) (list (CId '-args)) (hash empty) (none) (none)) (CNum 1))
                   (CReturn (CPrim1 type (CIndex (CId '-args) (CNum 0))))
                   (CError (CStr "constructor takes no more than one arg!")))
              (CReturn (case type 
                         ['dict (CDict (make-hash empty))]
                         ['list (CList true empty)]
                         ['tuple (CList false empty)]
                         ['bool (CFalse)]
                         ['str (CStr "")])))))
              

(define assert-true-lambda
  (CFunc (list 'check-true) (hash empty) (none) (none)
    (CIf (CId 'check-true) (CTrue) (CError (CStr "Assert failed")))))

(define assert-false-lambda
  (CFunc (list 'check-true) (hash empty) (none) (none)
    (CIf (CId 'check-true) (CError (CStr "Assert failed")) (CTrue))))

(define assert-equal-lambda
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare '== (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert failed")))))
(define assert-not-equal-lambda
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare '!= (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert failed")))))
(define assert-is
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare 'is (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert failed")))))

(define (exception-lambda [s : string]) : CExp
  (CFunc (list 'e) (hash empty) (none) (none)
      (CObject (make-hash (list 
                           (values (CStr "__type__") (CStr "exception"))
                           (values (CStr "__class__") (CStr "class"))
                           (values (CStr "__exceptiontype__") (CStr s))
                           (values (CStr "__errexp__") (CId 'e)))))))


#|
(define assert-raises-lambda
  (CFunc (list 'exc-type 'func) empty
         (CLet 'fun-call 'local (CApp (CId 'func) empty)
               (CIf (Compare '== (CApp (CId 'tagof) (list (CId 'fun-call))) (CStr "object"))
                    (CIf (Compare '== (CGet (CId 'fun-call) (CStr "__type__")) (CStr "exception"))
                         (CIf (Compare '== (CGet (CId 'fun-call) (CStr "__exceptiontype__")) (CId 'exc-type))
                              (CTrue)
                              (CError (CStr "Assert failed")))
                         (CError (CStr "Assert failed")))
                    (CError (CStr "Assert failed"))))))

|#
(define true-val
  (CTrue))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print (make-prim 'print))
        (bind 'tagof (make-prim 'tag))
        (bind 'len (make-prim 'len))
        (bind 'list (constructor 'list))
        (bind 'tuple (constructor 'tuple))
        (bind 'str (constructor 'str))
        (bind 'bool (constructor 'bool))
        (bind 'True true-val); we do this at parse time, which i think is better
        (bind 'Exception (exception-lambda "Exception"))
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertIs assert-is)
        (bind '___assertNotEqual assert-not-equal-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        ;(bind '___assertRaises assert-raises-lambda)
        (bind 'TypeError (exception-lambda "TypeError"))
        (bind 'KeyError (exception-lambda "KeyError"))
        (bind 'RuntimeError (exception-lambda "RuntimeError"))
        (bind 'IndexError (exception-lambda "IndexError"))))


(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name 'global value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


