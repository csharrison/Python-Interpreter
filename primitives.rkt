#lang plai-typed

(require "core-syntax.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#



(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VTrue () "true"]
    [VFalse () "false"]
    [VNone () "None"]
    [VNotDefined () "Not Defined"]
    [VClosure (env args defs body) "(closure ...)"]
    [VObject (fields) (string-append "Object: " "")]
    [VReturn (val) (pretty val)]))

(define (tagof arg)
  (type-case CVal arg
    [VNum (n) "number"]
    [VStr (s) "string"]
    [VTrue () "boolean"]
    [VFalse () "boolean"]
    [VNone () "None"]
    [VNotDefined () "Not Defined"]
    [VClosure (env args defs body) "closure"]
    [VObject (fields) "object"]
    [VReturn (val) "return"]))
(define (get-tag arg)
  (begin ;(display "fuck-> ")(display (pretty arg)) (display "\n")
         (VStr (tagof arg))))

(define (print arg)
  (begin (display (pretty arg)) (display "\n")))


(define (python-prim1 (op : symbol) (arg : CVal)) : CVal
  (begin 
  (case op
    [(print) (begin (print arg) arg)]
    [(tag) (get-tag arg)])))

