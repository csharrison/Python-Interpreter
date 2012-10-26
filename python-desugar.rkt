#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desug expr)
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
    [PyIf (c t e) (CIf (desug c) (desug t) (desug e))]
    
    [PyRaise (ex) (CError (desugar ex))]
    [PyNotImplemented () (CStr "not implemented")]
    [else (CError (CStr "desugar hasn't handled a case yet"))]))

(define (desugar expr)
  (begin ;(display expr)
  (desug expr)))