#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")
(require (typed-in racket 
                   (flatten : ((listof (listof 'a)) -> (listof 'a)))))
;;err : calls an error with all input strings appended
(define-syntax Err
  (syntax-rules ()
    [(Err s) (CError (CStr s))]
    [(Err s ...) (CError (CStr (foldr string-append "" (list s ...))))]))

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

(define dummy-func (CFunc empty empty (CError (CStr "dummy func"))))

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (exprs : (listof CExp))
                      (body : CExp)) : CExp
  (cond [(empty? ids) body]
        [(cons? ids)
         (CLet (first ids) (first exprs) (cascade-lets (rest ids) (rest exprs) body))]))
;;js style static code reading for hoisting
(define (get-assigns (expr : PyExpr)) : (listof symbol)
  (type-case PyExpr expr
    [PySeq (es) (flatten (map get-assigns es))]
    [PyIf (c t e) (flatten (map get-assigns (list c t e)))]
    [PyAssign (targets value)
              (cond [(empty? (rest targets))
                     (type-case PyExpr (first targets)
                       [PyId (id) (list id)]
                       [else (begin (error 'desugar "no assign case for non ids") empty)])]
                    [else (begin (error 'desugar "no assign for iterables") empty)])]
    [PyFunDef (name args defaults body) (list name)]
    [else empty]))

(define (hoist/desug (body : PyExpr) (args : (listof symbol))) : CExp
  (let ((ids (filter (lambda (x) (not (member x args))) (get-assigns body))))
    (cascade-lets ids (map (lambda (x) (CNotDefined)) ids) (desug body))))
(define (get-args args defaults)
  (append args (map (lambda (x) (PD-id x)) defaults)))


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
    
    [PyFunDef (name args defaults body) 
              (let ((thefun (make-id)))
                (CSeq (CSet! name dummy-func)
                      (CLet thefun
                            (CFunc args 
                                   (convert-defaults defaults)
                                   (hoist/desug body (get-args args defaults)))
                            (CSet! name (CId thefun)))))]
    
    [PyReturn (val) (CReturn (desug val))]
    
    [PyFun (args defaults body) (CFunc args (convert-defaults defaults) (desug body))]
    
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
  (hoist/desug expr empty)))
