#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define make-id
  (let ((count 0))
    (lambda () 
      (begin (set! count (add1 count))
             (string->symbol (string-append "--var--" (to-string count)))))))
(define (last lst)
  (cond [(empty? (rest lst)) (first lst)]
        [else (last (rest lst))]))
(define (take-last lst)
  (cond [(empty? (rest lst)) empty]
        [else (cons (first lst) (take-last (rest lst)))]))

(define (desug (expr : PyExpr)) : CExp
  (type-case PyExpr expr
    [PySeq (es) 
           (if (empty? es) 
               (CNone)
               (foldr (lambda (e1 e2) (CSeq e2 (desug e1))) (desug (first es)) (rest es)))]
    [PyNum (n) (CNum n)]
    [PyStr (s) (CStr s)]
    [PyTrue () (CTrue)]
    [PyFalse () (CFalse)]
    [PyApp (f args) (CApp (desug f) (map desug args))]
    [PyId (x) (CId x)]
    [PyPass () (CNone)]
    
    [PyOr (exprs)  (foldr (lambda (f rest) 
                            (let ((id (make-id)))
                              (CLet id (desug f) (CIf (CId id) (CId id) rest)))) (CFalse) exprs)]
    [PyAnd (exprs)
           (foldr (lambda (f rest) 
                            (let ((id (make-id)))
                              (CLet id (desug f) 
                                    (CIf (CId id) rest (CId id))))) (desug (last exprs)) (take-last exprs))]   

    [PyUnary (op expr) (CUnary op (desug expr))]
    [PyIf (c t e) (CIf (desug c) (desug t) (desug e))]
    
    [PyRaise (ex) (CError (desugar ex))]
    [PyNotImplemented () (CStr "not implemented")]
    [else (begin (display expr) (CError (CStr "desugar hasn't handled a case yet")))]))

(define (desugar expr)
  (begin ;(display expr)
  (desug expr)))
