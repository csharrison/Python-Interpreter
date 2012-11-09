#lang plai-typed

(require "python-syntax.rkt"
         "core-syntax.rkt")
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
  
(define (convert-defaults (defs : (listof PyDefault)) scope)
  (map (lambda (pyd) (CD (PD-id pyd) (desug (PD-val pyd) scope))) defs))

(define dummy-func (CFunc empty empty (CError (CStr "dummy func"))))

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (type : symbol)
                      (exprs : (listof CExp))
                      (body : CExp)) : CExp
  (cond [(empty? ids) body]
        [(cons? ids)
         (CLet (first ids) type (first exprs) (cascade-lets (rest ids) type (rest exprs) body))]))

(define (modify-scope id (h : (hashof symbol symbol)) type)
  (type-case (optionof symbol) (hash-ref h id)
    [some (s) h];;this comes from the fact that doing nonlocal before a global makes it nonlocal (or any combination)
    [none () (hash-set h id type)]))
;;js style static code reading for hoisting
(define (get-assigns (expr : PyExpr) global?) : (hashof symbol symbol)
  (local ((define (get-scope expr scope) : (hashof symbol symbol)
           (type-case PyExpr expr
             ;;get rid of global and nonlocal identifiers
             [PySeq (es) 
                    (cond [(empty? es) scope]
                          [(cons? es)
                           (type-case PyExpr (first es)
                             [PyGlobal (x) (get-scope (PySeq (rest es)) (modify-scope x scope 'global))]
                             [PyNonLocal (x) (get-scope (PySeq (rest es)) (modify-scope x scope 'nonlocal))]
                             [else (get-scope (PySeq (rest es)) (get-scope (first es) scope))])])]
             ;;THIS MAY NEED REWORKING, EXAMINE IF STATEMENTS
             [PyIf (c t e) (get-scope (PySeq (list c t e)) scope)]
             [PyAssign (targets value)
                       (cond [(empty? (rest targets))
                              (type-case PyExpr (first targets)
                                [PyId (id) (modify-scope id scope (if global? 'global 'local))]
                                [else scope])]
                                ;[else (begin (error 'desugar "no assign case for non ids") scope)])]
                             [else (begin (error 'desugar "no assign for iterables") scope)])]
             [PyFunDef (name args defaults body) (modify-scope name scope (if global? 'global 'local))]
             [PyClassDef (name base body) (modify-scope name scope (if global? 'global 'local))]
             [else scope])))
    (get-scope expr (hash empty))))

(define (get-type scope type)
  (filter (lambda (x) (symbol=? type (some-v (hash-ref scope x)))) (hash-keys scope)))

;;puts locals, nonlocals, and globals on the let cascade
(define (hoist/desug (body : PyExpr) (args : (listof symbol)) global?) : CExp
  (local ((define scope (get-assigns body global?))
          (define locals (filter (lambda (x) (not (member x args))) (get-type scope 'local)))
          (define globals (get-type scope 'global))
          (define nonlocals (get-type scope 'nonlocal)))
    (if global? (begin ;(display scope)
        (cascade-lets (hash-keys scope) 'global (map (lambda (x) (CNotDefined)) (hash-keys scope)) (desug body scope)))
        (cascade-lets locals 'local (map (lambda (x) (CNotDefined)) locals)
                      (cascade-lets nonlocals 'nonlocal (map (lambda (x) (CNotDefined)) nonlocals)
                                    (cascade-lets globals 'global (map (lambda (x) (CNotDefined)) globals)
                                                  (desug body scope)))))))

(define (get-args args defaults)
  (append args (map (lambda (x) (PD-id x)) defaults)))

(define (get-scope-type id scope)
  (type-case (optionof symbol) (hash-ref scope id)
    [some (v) v]
    [none () 'local]))

;;ns = nonlocals
;;gs = globals
(define (desug (expr : PyExpr) (scope : (hashof symbol symbol))) : CExp
  (type-case PyExpr expr
    [PySeq (es) (cascade-seq (map (lambda (x) (desug x scope)) es))]
    [PyNum (n) (CNum n)]
    [PyStr (s) (CStr s)]
    [PyTrue () (CTrue)]
    [PyFalse () (CFalse)]
    [PyNone () (CNone)]
    ;[PyApp (f args) (CApp (CGet (desug f scope) (CStr "__call__") ) (map (lambda (x) (desug x scope)) args))]
    
    [PyApp (f args) (CApp  (desug f scope) (map (lambda (x) (desug x scope)) args))]
    [PyId (x) ((case (get-scope-type x scope) ('local CId) ('nonlocal CNonLocalId) ('global CGlobalId)) x)]
    [PyGlobal (x) (CGlobalId x)]
    [PyNonLocal (x) (CNonLocalId x)]
    
    [PyAssign (targets value)
              (cond [(empty? (rest targets))
                     (type-case PyExpr (first targets)
                       [PyId (id) (CSet! id (desug value scope) (get-scope-type id scope))]
                       [PyGetAttr (target field) (CSetAttr (desug target scope) (desug field scope ) (desug value scope))]
                       [else (Err "no assign case yet for ")])]
                    [(cons? (rest targets))
                     (Err "no assign for iterables")])]
    
    [PyPass () (CNone)]
    
    
    
    [PyFunDef (name args defaults body) 
              (let ((thefun (make-id)))
                (CSeq (CSet! name dummy-func (get-scope-type name scope))
                      (CLet thefun 'local
                            (CFunc args 
                                   (convert-defaults defaults scope)
                                   (hoist/desug body (get-args args defaults) false))
                            (CSet! name (CId thefun) (get-scope-type name scope)))))]
    
    [PyReturn (val) (CReturn (desug val scope))]
    
    [PyClassDef (name base body)
                (local ((define (get-fields (fs :(listof PyExpr))) : (listof (symbol * CExp))
                          (if (empty? fs) empty
                              (type-case PyExpr (first fs)
                                [PyFunDef (name args defaults body) (cons (values name (desug (PyFun args defaults body) scope)) (get-fields (rest fs)))]
                                [PyAssign (targets value) 
                                          (cond [(empty? (rest targets))
                                                 (type-case PyExpr (first targets)
                                                   [PyId (id) (cons (values id (desug value scope)) (get-fields (rest fs)))]
                                                   [else (get-fields (rest fs))])]
                                                [else (get-fields (rest fs))])]
                                [else (get-fields (rest fs))])))
                        (define fields (get-fields body))
                        (define hash-fields (make-hash fields))
                        (define obj (CObject hash-fields)))
                  
                  (CSeq (CSet! name (CObject (make-hash (append fields
                                                                       (list (values '__call__
                                                                                     (CFunc empty empty (CReturn obj)))))))
                               (get-scope-type name scope)) (CNone)))]
    ;;DO NOW: add __call__ method to the class ,which instantiates an objcect
                 
    
    [PyGetAttr (target attr) (CGet (desug target scope) (desug attr scope))]
    [PyFun (args defaults body) (CFunc args (convert-defaults defaults scope) (desug body scope))]
    
    [PyOr (exprs)  (foldr (lambda (f rest) 
                            (let ((id (make-id)))
                              (CLet id 'local (desug f scope) (CIf (CId id ) (CId id ) rest)))) (CFalse) exprs)]
    [PyAnd (exprs)
           (foldr (lambda (f rest) 
                            (let ((id (make-id)))
                              (CLet id 'local (desug f scope) 
                                    (CIf (CId id ) rest (CId id ))))) (desug (last exprs) scope) (take-last exprs))]   

    ;;basically fold And over all the compares
    [PyCompare (left ops rights)
               (local ((define (create-ops ops elts)
                         (cond [(empty? (rest ops)) (Compare (first ops) (first elts) (second elts))]
                               [else (let ((id (make-id)))
                                       (CLet id 'local (Compare (first ops) (first elts) (second elts)) 
                                             (CIf (CId id ) (create-ops (rest ops) (rest elts)) (CId id ))))])))
                 (create-ops ops (cons (desug left scope) (map (lambda (x) (desug x scope)) rights))))]
    
    [PyUnary (op expr) (CUnary op (desug expr scope))]
    [PyBinOp (op l r) (CBinOp op (desug l scope) (desug r scope))]
    [PyIf (c t e) (CIf (desug c scope) (desug t scope) (desug e scope))]
    
    [PyRaise (ex) (CError (desug ex scope))]
    [PyNotImplemented () (CStr "not implemented")]
    [else (begin (display expr) (CError (CStr "desugar hasn't handled a case yet")))]))

(define (desugar expr)
  (begin ;(display expr)
  (hoist/desug expr empty true)))
