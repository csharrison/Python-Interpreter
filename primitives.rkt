#lang plai-typed

(require "core-syntax.rkt"
         "methods.rkt")
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
                   (string<? : (string string -> boolean))
                   (string>? : (string string -> boolean))
                   (string-replace :(string string string -> string))
                   [string-split : (string string -> (listof string))]
                   [range : (number number number -> (listof number))]))
(define (valid-index c)
  (or (VNone? c) (VNum? c)))

(define (str-to-list str)
  (filter (lambda (x) (not (string=? x ""))) (string-split str "")))
(define (str-in str isin)
  (not (string=? str (string-replace str isin ""))))

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

(define (callable elt store)
  (type-case CVal elt
    [VClosure (e args defs star kwar b) (ValA (VTrue) store)]
    [VObject (fields) 
             (type-case (optionof CVal) (hash-ref fields (VStr "__call__"))
               [some (v) (ValA (VTrue) store)]
               [none () (ValA (VFalse) store)])]
    [else (ValA (VFalse) store)]))

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
                           (s (if (VNone? step) 1 (if (< (VNum-n step) 0) (- 0 (VNum-n step)) (VNum-n step))))
                           (elts (if (and (VNum? step) (< (VNum-n step) 0)) (reverse elts) elts)))
                       (ValA (VList m (drop-n (between elts l u) s)) sto))]
              [VStr (str) (type-case Ans (slice (VList false (map VStr (str-to-list str))) lower upper step sto)
                            [ValA (v s) (type-case CVal v
                                         [VList (m l) (ValA (VStr (string-join (map VStr-s l) "")) sto)]
                                         [else (error 'slice "slice returned non-lst")])]
                            [ExnA (v s) (ExnA v s)])]
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
    [VSet (elts) (VBool (not (empty? (hash-keys elts))))]
    [VReturn (val) (BoolEval val)]))

(define (num-of type (val : CVal) store)
  (type-case CVal val
    [VTrue () (ValA (VNum (case type [(int abs) 1] ['float 1.0])) store)]
    [VFalse () (ValA (VNum (case type [(int abs) 0] ['float 0.0])) store)]
    [else (ExnA (VStr "int() argument must be booleans") store)]))

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
    [VSet (elts) (string-join (list "{" (string-join (map pretty (hash-keys elts)) ", ") "}") "")]
    [VReturn (val) (pretty val)]))

(define (str-op op str store)
  (type-case CVal str
    [VStr (s) (local ((define lst (str-to-list s))
                      (define (find-extreme (l : (listof string)) comparator) : string 
                        (cond [(empty? (rest l)) (first l)]
                              [else (let ((max (find-extreme (rest l) comparator)))
                                      (if (comparator (first l) max)
                                          (first l) max))])))
                      (ValA (VStr (find-extreme lst (case op ['min string<?] ['max string>?]))) store))]
                
    [else (ExnA (VStr "min on non-string") store)]))

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
    [VSet (elts) "set"]
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
    [VList (m elts) 
           (if (eq? m mutable) (ValA arg store)
               (ValA (VList mutable elts) store))]
    [VStr (s) (ValA (VList mutable (map VStr (str-to-list s))) store)]
    [VDict (fields) (ValA (VList mutable (hash-keys fields)) store)]
    [VSet (elts) (ValA (VList mutable (hash-keys elts)) store)]
    [else (ExnA (VStr "cannot call list() on non iterable") store)]))
(define (to-set arg store)
  (let ((h (make-hash empty)))
    (type-case CVal arg
      [VList (m elts) (begin (map (lambda (x) (hash-set! h x (VNone))) elts) (ValA (VSet h) store))]
      [VDict (fields) (begin (map (lambda (x) (hash-set! h x (VNone))) (hash-keys fields)) (ValA (VSet h) store))]
      [VSet (elts) (ValA arg store)]
      [VStr (s) 
            (begin
              (map (lambda (x) (hash-set! h x (VNone))) (map VStr (filter (lambda (x) (not (string=? "" x))) (string-split s ""))))
              (ValA (VSet h) store))]
      [else (ExnA (VStr "cannot call set() on non iterable") store)])))

(define (python-prim1 (op : symbol) (arg : CVal) store) : Ans
  (begin 
  (case op
    [(print) (begin (print arg) (ValA arg store))]
    [(callable) (callable arg store)]
    [(tag) (ValA (VStr (tagof arg)) store)]
    [(list) (to-list arg true store)]
    [(set) (to-set arg store)]
    [(bool) (ValA (BoolEval arg) store)]
    [(int float abs) (num-of op arg store)]
    [(tuple) (to-list arg false store)]
    [(str) (ValA (VStr (pretty arg)) store)]
    [(prim-len) (len arg store)]
    [(min max) (str-op op arg store)]
    [(items clear values keys) (dict-method op arg store)])))

