#lang plai-typed

(require "core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

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
              (CError (CStr "Assert equal failed")))))
(define assert-not-equal-lambda
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare '!= (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert not equal failed")))))
(define assert-is
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare 'is (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert is failed")))))
(define assert-isnot
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare 'isnot (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert is not failed")))))
(define assert-notin
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare 'notin (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert not in failed")))))

(define assert-in
  (CFunc (list 'first-elt 'second-elt) (hash empty) (none) (none)
         (CIf (Compare 'in (CId 'first-elt) (CId 'second-elt)) (CTrue)
              (CError (CStr "Assert in failed")))))
(define (exception-lambda [s : string]) : CExp
  (CFunc (list 'self 'e) (hash empty) (none) (none)
         (CReturn (CObject (make-hash (list 
                                       (values (CStr "__type__") (CStr "exception"))
                                       (values (CStr "__class__") (CStr "class"))
                                       (values (CStr "__exceptiontype__") (CStr s))
                                       (values (CStr "__errexp__") (CId 'e))))))))

(define (make-exn [type : string] [msg : string]) : CVal
  (VObject (make-hash (list 
                       (values (VStr "__type__") (VStr "exception"))
                       (values (VStr "__class__") (VStr "class"))
                       (values (VStr "__exceptiontype__") (VStr type))
                       (values (VStr "__errexp__") (VStr msg))))))

(define (Cmake-exn type msg)
    (CObject (make-hash (list 
                       (values (CStr "__type__") (CStr "exception"))
                       (values (CStr "__class__") (CStr "class"))
                       (values (CStr "__exceptiontype__") (CStr type))
                       (values (CStr "__errexp__") (CStr msg))))))
(define (exn-class (s : string))
  (CObject (make-hash (list (values (CStr "__call__")
                                    (exception-lambda s))
                            (values (CStr "__exceptionclass__") (CStr s))))))

(define fail-lambda
  (CFunc empty (hash empty) (none) (none)
         (CError (CStr "Assert (__fail) failed"))))


(define assert-raises-lambda
  (CFunc (list 'exc-class 'func) (hash empty) (some 'args) (none)
         (CSeq (CTryExcept (CApp (CId 'func) empty (hash empty) (some (CId 'args)) (none))
                           (list (CExceptHandler (CReturn (CNone));body
                                                 (some (CId 'exc-class));type
                                                 (some 'e)));name
                           (CError (CStr "Assert raises failed")))
               (CError (CStr "Assert raises after")))))


(define true-val
  (CTrue))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define our-map
  (CFunc (list 'func 'lst) (hash empty) (none) (none)
         (CIf (Compare '== (CApp (CId 'len) (list (CId 'lst)) (hash empty) (none) (none)) (CNum 0)) 
              (CReturn (CList #t empty)) 
              (CReturn (CBinOp '+ 
                               (CList #t (list (CApp (CId 'func) (list (CIndex (CId 'lst) (CNum 0))) (hash empty) (none) (none)))) 
                               (CApp (CId 'map) (list (CId 'func) (CSlice (CId 'lst) (CNum 1) (CNone) (CNone))) (hash empty) (none) (none)))))))

(define lib-functions
  (list (bind 'print (make-prim 'print))
        (bind 'any (make-prim 'any))
        (bind 'all (make-prim 'all))
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
        (bind 'map our-map)
        (bind 'True true-val); we do this at parse time, which i think is better
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertIs assert-is)
        (bind '___assertIsNot assert-isnot)
        (bind '___assertNotEqual assert-not-equal-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertIn assert-in)
        (bind '___assertNotIn assert-notin)
        (bind '___fail fail-lambda)
        (bind '___assertRaises assert-raises-lambda)
        (bind 'Exception (exn-class "Exception"))
        (bind 'TypeError (exn-class "TypeError"))
        (bind 'KeyError (exn-class "KeyError"))
        (bind 'ZeroDivisionError (exn-class "ZeroDivisionError"))
        (bind 'RuntimeError (exn-class "RuntimeError"))
        (bind 'AttributeError (exn-class "AttributeError"))
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


