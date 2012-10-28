#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (Err str)
  (CError (CStr str)))

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

(define (cascade-seq (exprs : (listof CExp))) : CExp
  (cond
    [(empty? exprs) (CNone)]
    [(cons? exprs) (CSeq (first exprs) (cascade-seq (rest exprs)))]))
  
(define (convert-defaults (defs : (listof PyDefault)))
  (map (lambda (pyd) (CD (PD-id pyd) (desug (PD-val pyd)))) defs))

(define (dummy-func) 
  (CFunc empty empty (CError (CStr "dummy func"))))

(define (desug (expr : PyExpr)) : CExp
  (type-case PyExpr expr
    [PySeq (es) (cascade-seq (map desug es))]
    [PyNum (n) (CNum n)]
    [PyStr (s) (CStr s)]
    [PyTrue () (CTrue)]
    [PyFalse () (CFalse)]
    [PyNone () (CNone)]
    [PyApp (f args) (CApp (desug f) (map desug args))]
    
    [PyId (x) (CId x)]
    [PyAssign (targets value)
              (cond [(empty? (rest targets))
                     (type-case PyExpr (first targets)
                       [PyId (id) (CSet! id (desug value))]
                       [else (Err "no assign case yet for ")])]
                    [(cons? (rest targets))
                     (Err "no assign for iterables")])]
    
    [PyPass () (CNone)]
    
    ;;use mutation to set
    [PyFunDef (name args defaults body) 
              (let ((thefun (make-id)))
                (CSet! name 
                       (CLet name (dummy-func)
                             (CLet thefun 
                                   (CFunc args 
                                          (convert-defaults defaults)
                                          (desugar body))
                                   (CSeq (CSet! name (CId thefun)) (CId name))))))]
    
    [PyReturn (val) (CReturn (desug val))]
    
    [PyFun (args defaults body) (CFunc args 
                                       (convert-defaults defaults) 
                                       (CReturn (desug body)))]
    
    [PyOr (exprs)  (foldr (lambda (f rest) 
                            (let ((id (make-id)))
                              (CLet id (desug f) (CIf (CId id) (CId id) rest)))) (CFalse) exprs)]
    [PyAnd (exprs)
           (foldr (lambda (f rest) 
                            (let ((id (make-id)))
                              (CLet id (desug f) 
                                    (CIf (CId id) rest (CId id))))) (desug (last exprs)) (take-last exprs))]   

    ;;basically fold And over all the compares
    [PyCompare (left ops rights)
               (local ((define (create-ops ops elts)
                         (cond [(empty? (rest ops)) (Compare (first ops) (first elts) (second elts))]
                               [else (let ((id (make-id)))
                                       (CLet id (Compare (first ops) (first elts) (second elts)) 
                                             (CIf (CId id) (create-ops (rest ops) (rest elts)) (CId id))))])))
                 (create-ops ops (cons (desug left) (map desug rights))))]
    
    [PyUnary (op expr) (CUnary op (desug expr))]
    [PyBinOp (op l r) (CBinOp op (desug l) (desug r))]
    [PyIf (c t e) (CIf (desug c) (desug t) (desug e))]
    
    [PyRaise (ex) (CError (desugar ex))]
    [PyNotImplemented () (CStr "not implemented")]
    [else (begin (display expr) (CError (CStr "desugar hasn't handled a case yet")))]))

(define (desugar expr)
  (begin ;(display expr)
  (desug expr)))
