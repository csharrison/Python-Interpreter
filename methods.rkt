#lang plai-typed

(require "core-syntax.rkt")

(define (dict-method m dict store)
  (type-case CVal dict
    [VDict (h) (case m
                 ['clear (begin (map (lambda (x) (hash-remove! h x)) (hash-keys h)) (ValA dict store))]
                 ['items (ValA (VSet (make-hash (map (lambda (x)
                                                       (values (VList false (list x (some-v (hash-ref h x)))) (VNone))) (hash-keys h)))) store)]
                 [(values keys) (ValA (VSet (make-hash (map (lambda (x) (values 
                                                                         (case m ['values (some-v (hash-ref h x))] ['keys x])
                                                                         (VNone))) (hash-keys h)))) store)])]
    [else (error '-methods "should check this is a dict before calling")]))

(define (and-lst l1 l2)
  (filter (lambda (x) (member x l2)) l1))
(define (xor-lst l1 l2)
  (append (filter (lambda (x) (not (member x l2))) l1)
          (filter (lambda (x) (not (member x l1))) l2)))

(define (set-op op d1 d2 store)
  (type-case CVal d1
    [VSet (h)
          (type-case CVal d2
            [VSet (h2)
                  (let ((new (make-hash empty)))
                    (case op
                      ['bitor (begin (map (lambda (x) (hash-set! new x (VNone))) (append (hash-keys h) (hash-keys h2))) (ValA (VSet new) store))]
                      ['- (ValA (VSet (make-hash (map (lambda (x) (values x (VNone))) (filter (lambda (x) (none? (hash-ref h2 x))) (hash-keys h))))) store)]
                      ['& (ValA (VSet (make-hash (map (lambda (x) (values x (VNone))) (and-lst (hash-keys h) (hash-keys h2)))))store )]
                      ['bitxor (ValA (VSet (make-hash (map (lambda (x) (values x (VNone))) (xor-lst (hash-keys h) (hash-keys h2))))) store)]
                      [else (ExnA (VStr (string-append "bad bin op for set: " (symbol->string op))) store)]))]
            [else (error 'set-op "should not call set-op with non-sets")])]
    [else (error 'set-op "should not call set-op with non-sets")]))
(define (dict-op op d1 d2 store) : Ans
  (type-case CVal d1
    [VDict (h)
           (type-case CVal d2
             [VDict (h2)
                   (case op
                     ['update (begin (map (lambda (x) (hash-set! h x (some-v (hash-ref h2 x)))) (hash-keys h2)) (ValA d1 store))]
                     [else (ExnA (VStr (string-append "bad bin op for dict " (symbol->string op))) store)])]
             [else (error 'set-op "should not call dict-op with non-dicts")])]
    [else (error 'set-op "should not call dict-op with non-dicts")]))
