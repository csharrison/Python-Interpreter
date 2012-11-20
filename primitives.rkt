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
                   [string-join : ((listof string) string -> string)]
                   [drop : ((listof 'a)  number -> (listof 'a))]
                   [take : ((listof 'a)  number -> (listof 'a))]
                   [string-length : (string -> number)]
                   [string-split : (string string -> (listof string))]
                   [range : (number number number -> (listof number))]))
(define (valid-index c)
  (or (VNone? c) (VNum? c)))

(define (my-drop lst n)
  (cond [(> n (length lst)) empty]
        [else (drop lst n)]))
(define (drop-n lst n)
  (cond [(empty? lst) empty]
        [(cons? lst) (cons (first lst)
                           (drop-n (my-drop lst n) n))]))
(define (between lst lower upper)
  (let ((len (length lst)))
    (drop (take lst (min upper len)) lower)))
(define (index (lst : CVal) (i : CVal) store)
  (local ((define (nth lst n)
                   (cond [(zero? n) (first lst)]
                         [else (nth (rest lst) (sub1 n))])))
           (type-case CVal lst
             [VList (m elts) (type-case CVal i
                               [VNum (n) (if (>= n (length elts))
                                             (ExnA (VStr "list index out of range") store)
                                             (ValA (nth elts n) store))]
                               [else (ExnA (VStr "indices must be integers!") store)])]
             [VDict (fields) (type-case (optionof CVal) (hash-ref fields i)
                               [some (v) (ValA v store)]
                               [none () (ExnA (VStr (string-append "lookup failed: " (pretty i))) store)])]
             [else (ExnA (VStr "cannot take the index of a non-list") store)])))

(define (slice (lst : CVal) (lower : CVal) (upper : CVal) (step : CVal) (sto : Store))
  (if (and (valid-index lower) (valid-index upper) (valid-index step))
      (type-case CVal lst
        [VList (m elts)
               (let ((l (if (VNone? lower) 0 (VNum-n lower)))
                     (u (if (VNone? upper) (length elts) (VNum-n upper)))
                     (s (if (VNone? step) 1 (VNum-n step))))
                   (ValA (VList m (drop-n (between elts l u) s)) sto))]
        [else (ExnA (VStr "nonlist not subscriptable!") sto)])
      (ExnA (VStr "list indices must be integers!") sto)))
           
(define (VBool v)
  (if v (VTrue) (VFalse)))
(define (BoolEval (val : CVal)) : CVal
  (type-case CVal val
    [VNum (n) (VBool (not (zero? n)))]
    [VList (m elts) (VBool (not (empty? elts)))]
    [VStr (s) (VBool (not (string=? "" s)))]
    [VTrue () (VTrue)]
    [VFalse () (VFalse)]
    [VNone () (VFalse)]
    [VClosure (e args defs s k b) (VTrue)]
    [VNotDefined () (VFalse)]
    [VObject (fields) (VTrue)]
    [VDict (fields) (VBool (not (empty? (hash-keys fields))))]
    [VReturn (val) (BoolEval val)]))

(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VList (mutable elts) (string-join (list (if mutable "[" "(") (string-join (map pretty elts) ", ") (if mutable "]" ")")) "")]
    [VTrue () "True"]
    [VFalse () "False"]
    [VNone () "None"]
    [VNotDefined () "Not Defined"]
    [VClosure (env args defs s k body) "(closure ...)"]
    [VObject (fields) (string-append "Object: " "")]
    [VDict (fields) (string-join  (list "{" (string-join (map (lambda (k)
                                                                (string-join (list (pretty k) (pretty (some-v (hash-ref fields k)))) ": ")) (hash-keys fields)) ", ") "}") "")]
    [VReturn (val) (pretty val)]))

(define (tagof arg)
  (type-case CVal arg
    [VNum (n) "number"]
    [VStr (s) "string"]
    [VList (m elt) "list"]
    [VTrue () "boolean"]
    [VFalse () "boolean"]
    [VNone () "None"]
    [VNotDefined () "Not Defined"]
    [VClosure (env args defs s k body) "closure"]
    [VObject (fields) "object"]
    [VDict (fields) "dict"]
    [VReturn (val) "return"]))
(define (print arg)
  (begin (display (pretty arg)) (display "\n")))

(define (len arg store)
  (type-case CVal arg
    [VList (m elts) (ValA (VNum (length elts)) store)]
    [VStr (s) (ValA (VNum (string-length s)) store)]
    [VDict (fields) (ValA (VNum (length (hash-keys fields))) store)]
    [else (ExnA (VStr (string-append (tagof arg) " has no len()")) store)]))

(define (to-list arg mutable store)
  (type-case CVal arg
    [VList (m elts) (ValA (VList mutable elts) store)]
    [VStr (s) (ValA (VList mutable (map VStr (filter (lambda (x) (not (string=? "" x))) (string-split s "")))) store)]
    [VDict (fields) (ValA (VList mutable (hash-keys fields)) store)]
    [else (ExnA (VStr "cannot call list() on non iterable") store)]))

(define (python-prim1 (op : symbol) (arg : CVal) store) : Ans
  (begin 
  (case op
    [(print) (begin (print arg) (ValA arg store))]
    [(tag) (ValA (VStr (tagof arg)) store)]
    [(list) (to-list arg true store)]
    [(bool) (ValA (BoolEval arg) store)]
    [(tuple) (to-list arg false store)]
    [(str) (ValA (VStr (pretty arg)) store)]
    [(len) (len arg store)])))

