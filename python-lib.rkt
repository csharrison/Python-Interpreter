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
         (CIf (Compare '> (CApp (CId 'prim-len) (list (CId '-args)) (hash empty) (none) (none)) (CNum 0))
              (CIf (Compare '== (CApp (CId 'prim-len) (list (CId '-args)) (hash empty) (none) (none)) (CNum 1))
                   (CReturn (CPrim1 type (CIndex (CId '-args) (CNum 0))))
                   (CError (CStr "constructor takes no more than one arg!")))
              (CReturn (case type 
                         ['dict (CDict (make-hash empty))]
                         ['list (CList true empty)]
                         ['tuple (CList false empty)]
                         ['set (CSet (make-hash empty))]
                         ['bool (CFalse)]
                         [(int) (CNum 0)]
                         ['abs (CError (CStr "abs takes only one arg!"))]
                         ['float (CNum 0.0)]
                         ['prim-len (CNum 0)]
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
(define assert-isnot
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare 'isnot (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert failed")))))
(define assert-notin
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare 'notin (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-in
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare 'in (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert failed")))))
(define (exception-lambda [s : string]) : CExp
  (CFunc (list 'self 'e) (hash empty) (none) (none)
         (CReturn (CObject (make-hash (list 
                                       (values (CStr "__type__") (CStr "exception"))
                                       (values (CStr "__class__") (CStr "class"))
                                       (values (CStr "__exceptiontype__") (CStr s))
                                       (values (CStr "__errexp__") (CId 'e))))))))
(define (exn-class (s : string))
  (CObject (make-hash (list (values (CStr "__call__")
                                    (exception-lambda s))
                            (values (CStr "__exceptionclass__") (CStr s))))))


#|
(define assert-raises-lambda
  (CFunc (list 'exc-type 'func) (hash empty) (none) (none)
         (CLet 'fun-call 'local (CApp (CId 'func) empty)
               (CIf (Compare '== (CApp (CId 'tagof) (list (CId 'fun-call))) (CStr "object"))
                    (CIf (Compare '== (CGet (CId 'fun-call) (CStr "__type__")) (CStr "exception"))
                         (CIf (Compare '== 
                                       (CGet (CId 'fun-call) (CStr "__exceptiontype__")) 
                                       (CGet (CId 'exc-type) (CStr "__exceptiontype__")))
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
        (bind 'callable (make-prim 'callable))
        (bind 'min (make-prim 'min))
        (bind 'max (make-prim 'max))
        (bind 'prim-len (make-prim 'prim-len))
        (bind 'len (constructor 'prim-len))
        (bind 'list (constructor 'list))
        (bind 'tuple (constructor 'tuple))
        (bind 'set (constructor 'set))
        (bind 'str (constructor 'str))
        (bind 'bool (constructor 'bool))
        (bind 'int (constructor 'int))
        (bind 'abs (constructor 'abs))
        (bind 'float (constructor 'float))
        (bind 'True true-val); we do this at parse time, which i think is better
        (bind 'Exception (exception-lambda "Exception"))
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertIs assert-is)
        (bind '___assertIsNot assert-isnot)
        (bind '___assertNotEqual assert-not-equal-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertIn assert-in)
        (bind '___assertNotIn assert-notin)
        ;(bind '___assertRaises assert-raises-lambda)
        (bind 'TypeError (exn-class "TypeError"))
        (bind 'KeyError (exn-class "KeyError"))
        (bind 'RuntimeError (exn-class "RuntimeError"))
        (bind 'IndexError (exn-class "IndexError"))))


(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name 'global value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


