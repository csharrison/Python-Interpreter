#lang plai-typed

(require "python-core-syntax.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))

(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VTrue () "true"]
    [VFalse () "false"]
    [VNone () "None"]
    [VNotDefined () "Undefined"]
    [VClosure (env args defs body) "(closure ...)"]
    [VReturn (val) (pretty val)]))

(define (print arg)
  (begin (display (pretty arg)) (display "\n")))


(define (python-prim1 (op : symbol) (arg : CVal)) : CVal
  (case op
    [(print) (begin (print arg) arg)]))

