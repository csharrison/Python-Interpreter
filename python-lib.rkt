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

(define print-lambda
  (CFunc (list 'to-print) empty
    (CPrim1 'print (CId 'to-print))))

(define tag-of
  (CFunc (list 'to-tag) empty
         (CReturn (CPrim1 'tag (CId 'to-tag)))))

(define len 
  (CFunc (list 'to-len) empty
         (CReturn (CPrim1 'len (CId 'to-len)))))

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

(define (exception-lambda [s : string]) : CExp
  (CFunc (list 'e) empty
      (CObject (make-hash (list 
                           (values '__type__ (CStr "exception"))
                           (values '__class__ (CStr "class"))
                           (values '__exceptiontype__ (CStr s))
                           (values '__errexp__ (CId 'e)))))))



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

(define true-val
  (CTrue))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'tagof tag-of)
        (bind 'len len)
        (bind 'True true-val); we do this at parse time, which i think is better
        (bind 'Exception (exception-lambda "Exception"))
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertIs assert-equal-lambda)
        (bind '___assertNotEqual assert-not-equal-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertRaises assert-raises-lambda)
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


