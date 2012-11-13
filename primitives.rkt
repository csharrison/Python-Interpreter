#lang plai-typed

(require "core-syntax.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket
                   [display : (string -> void)]
                   [string-join : ((listof string) string -> string)]))
                   

(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VList (mutable elts) (string-join (list "[" (string-join (map pretty elts) ", ") "]") "")]
    [VTrue () "true"]
    [VFalse () "false"]
    [VNone () "None"]
    [VNotDefined () "Not Defined"]
    [VClosure (env args defs body) "(closure ...)"]
    [VObject (type base fields) (string-append "Object: "(symbol->string type))]
    [VReturn (val) (pretty val)]))

(define (print arg)
  (begin (display (pretty arg)) (display "\n")))


(define (python-prim1 (op : symbol) (arg : CVal)) : CVal
  (case op
    [(print) (begin (print arg) arg)]))

