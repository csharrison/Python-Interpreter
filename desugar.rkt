#lang plai-typed

(require "python-syntax.rkt"
         "core-syntax.rkt")
(require (typed-in racket 
                   (flatten : ((listof (listof 'a)) -> (listof 'a)))
                   ))
;;err : calls an error with all input strings appended
(define (map2 (f :  ('a 'b -> 'c)) (l1 : (listof 'a)) (l2 :(listof 'b))) : (listof 'c)
  (cond [(empty? l1) empty]
        [(cons? l1) (cons (f (first l1) (first l2)) (map2 f (rest l1) (rest l2)))]))

(define-syntax Err
  (syntax-rules ()
    [(Err s) (CError (CStr s))]
    [(Err s ...) (CError (CStr (foldr string-append "" (list s ...))))]))

(define (str (sym : symbol)) : CExp
  (CStr (symbol->string sym)))

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
  
(define (convert-defaults (defs : (hashof symbol PyExpr)) scope) : (hashof symbol CExp)
  (hash (map (lambda (pyd)
           (values pyd (desug (some-v (hash-ref defs pyd)) scope))) (hash-keys defs))))

(define dummy-func (CFunc empty (hash empty) (none) (none) (CError (CStr "dummy func"))))

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
  (begin 
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
             [PyFunDef (name args defaults s k body) (modify-scope name scope (if global? 'global 'local))]
             [PyClassDef (name base body) (modify-scope name scope (if global? 'global 'local))]
             [PyTryFinally (body finally)
                           (type-case PyExpr body
                             [PySeq (b-es) 
                                    (type-case PyExpr finally
                                      [PySeq (f-es) (get-scope (PySeq (append b-es f-es)) scope)]
                                      [else (begin (error 'desugar "finally not a seq") scope)])]
                             [else (begin (error 'desugar "body not a seq"))])]
             [PyTryExcept (body handlers elsebody)
                          (local ((define (get-all-bodies (hndls : (listof PyExpr)) (accum : (listof PyExpr)))
                                    (cond 
                                      [(empty? hndls) accum]
                                      [(cons? hndls) 
                                       (type-case PyExpr (first hndls)
                                         [PyExceptHandler (body t n)
                                                          (type-case PyExpr body
                                                            [PySeq (bodyseq)
                                                                   (get-all-bodies (rest hndls) (append accum bodyseq))]
                                                            [else (begin (error 'desugar "PyExceptHandler body not seq") accum)])]
                                         [else (begin (error 'desugar "expected PyExceptHandler, found something else"))])])))
                            (get-scope (PySeq (get-all-bodies handlers empty)) scope))]
                                                        
                                                          
             [else scope])))
    (get-scope expr (hash empty)))))

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

(define (get-args args defaults s k)
  (let ((all (append args (hash-keys defaults))))
    (cond [(and (some? s) (some? k)) (cons (some-v s) (cons (some-v k) all))]
          [(some? s) (cons (some-v s) all)]
          [(some? k) (cons (some-v k) all)]
          [else all])))

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
    
    [PyApp (f args keys star kwarg) (CApp (desug f scope) 
                                          (map (lambda (x) (desug x scope)) args)
                                          (hash (map (lambda (x) (values x (desug (some-v (hash-ref keys x)) scope))) (hash-keys keys)))
                                          (if (some? star) (some (desug (some-v star) scope)) (none))
                                          (if (some? kwarg) (some (desug (some-v kwarg) scope)) (none)))]
                                          
    [PyId (x) ((case (get-scope-type x scope) ('local CId) ('nonlocal CNonLocalId) ('global CGlobalId)) x)]
    [PyGlobal (x) (CGlobalId x)]
    [PyNonLocal (x) (CNonLocalId x)]
    
    ;;these assigns need work
    [PyAssign (targets value)
              (cond [(empty? (rest targets))
                     (type-case PyExpr (first targets)
                       [PyId (id) (CSet! id (desug value scope) (get-scope-type id scope))]
                       [PyGetAttr (target field) (CSetAttr (desug target scope) (desug field scope ) (desug value scope))]
                       [PyIndex (target i) (CSetAttr (desug target scope) (desug i scope) (desug value scope))]
                       [else (Err "no assign case yet for ")])]
                    [(cons? (rest targets))
                     (Err "no assign for iterables")])]
    [PyAugAssign (target op value)
                 (type-case PyExpr target
                   [PyId (id) (CSet! id (CBinOp op (CId id) (desug value scope)) (get-scope-type id scope))]
                   [PyGetAttr (target field)
                              (let ((t (make-id))
                                    (f (make-id)))
                                (CLet t 'local (desug target scope)
                                      (CLet f 'local (desug field scope)
                                            (CSetAttr (CId t) (CId f) (CBinOp op (desug value scope) (CGet (CId t) (CId f)))))))]
                   [else (Err "no assign case yet for ")])]
    
    [PyPass () (CNone)]
    [PyDelete (target) (CDelete (desug target scope))]
    [PyLocals ()
              (CLet '-locals 'local (CPrim1 'locals (CNone))
                    (CFunc empty (hash empty) (none) (none)
                           (CReturn (CId '-locals))))]

    
    [PyFunDef (name args defaults star kwarg body) 
              (let ((thefun (make-id)))
                (CSeq (CSet! name dummy-func (get-scope-type name scope))
                      (CLet thefun 'local
                            (CFunc args 
                                   (convert-defaults defaults scope)
                                   star kwarg
                                   (hoist/desug body (get-args args defaults star kwarg) false))
                            (CSet! name (CId thefun) (get-scope-type name scope)))))]
    
    [PyReturn (val) (CReturn (desug val scope))]
    
    [PyClassDef (name base body)
                (local ((define (get-fields (fs :(listof PyExpr))) : (listof (CExp * CExp))
                          (if (empty? fs) empty
                              (type-case PyExpr (first fs)
                                [PyFunDef (name args defaults s k body) (cons (values (str name) (desug (PyFun args defaults s k body) scope)) (get-fields (rest fs)))]
                                [PyAssign (targets value) 
                                          (cond [(empty? (rest targets))
                                                 (type-case PyExpr (first targets)
                                                   [PyId (id) (cons (values (str id) (desug value scope)) (get-fields (rest fs)))]
                                                   [else (get-fields (rest fs))])]
                                                [else (get-fields (rest fs))])]
                                [else (get-fields (rest fs))])))
                        (define fields (get-fields body))
                        (define hash-fields (make-hash fields))
                        (define obj (CObject hash-fields)))
                  
                  (CSeq (CSet! name (CObject 
                                     (make-hash 
                                      (append fields
                                              (list (values (CStr "__class__") (CStr "class"))
                                                    (values (CStr "__call__")
                                                            (type-case (optionof CExp) (hash-ref hash-fields (CStr "__init__"))
                                                              [some (v) (type-case CExp v
                                                                          [CFunc (args defaults s k body)
                                                                                 (let ((theinit (make-id)))
                                                                                   (CLet 'theobj 'local obj
                                                                                         (CFunc (rest args) defaults s k
                                                                                                (CSeq (CApp v (cons (CId 'theobj) (map CId (rest args))) (hash empty) (none) (none))
                                                                                                      (CReturn (CId 'theobj))))))]
                                                                          [else (Err "bad __init__ case")])]
                                                                                                        
                                                              [none () (CFunc empty (hash empty) (none) (none) (CReturn obj))]))))))
                               (get-scope-type name scope)) (CNone)))]
                 
    
    [PyGetAttr (target attr)
               (let ((t (make-id))
                     (a (make-id))
                     (tag  (make-id))
                     (result (make-id)))
                 (CLet t 'local (desug target scope)
                       (CLet a 'local (desug attr scope)
                             (CLet result 'local (CGet (CId t) (CId a))
                                   (CLet tag 'local (CApp (CId 'tagof) (list (CId result)) (hash empty) (none) (none))
                                         (CIf (Compare '== (CId tag) (CStr "closure"))
                                              (CPartialApply (CId result) (CId t));;give the target as first argument
                                              (CIf (Compare '== (CId tag) (CStr "dict"))
                                                   (CPartialApply (CId result) (CId t))
                                                   (CId result))))))))]
    [PyFun (args defaults star kwarg body) (CFunc args 
                                                  (convert-defaults defaults scope) 
                                                  star kwarg
                                                  (desug body scope))]
    
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
    [PyList (elts) (CList true (map (lambda (x) (desug x scope)) elts))]
    [PyTuple (elts) (CList false (map (lambda (x) (desug x scope)) elts))]
    [PySlice (lst lower upper step) (CSlice (desug lst scope) (desug lower scope) (desug upper scope) (desug step scope))]
    [PyIndex (lst i) (CIndex (desug lst scope) (desug i scope))]
    [PyDict (keys vals) (CDict (make-hash (map2 (lambda (x y) (values x y))
                                                          (map (lambda (x) (desug x scope)) keys)
                                                          (map (lambda (x) (desug x scope)) vals))))]
    [PyTryFinally (body finally) (CTryFinally (desug body scope) (desug finally scope))]
    [PyTryExcept (body handlers pelse)
                 (let ((c-body (desug body scope))
                       (c-else (desug pelse scope))
                       (c-handlers (map (lambda (h) (desug h scope)) handlers)))
                   (CTryExcept c-body c-handlers c-else))]
    [PyExceptHandler (body type name) 
                     (CExceptHandler (desug body scope)
                                     (type-case (optionof PyExpr) type
                                       [some (v) (some (desug v scope))]
                                       [none () (none)])
                                     name)]
    [PySet (elts) (CSet (make-hash (map (lambda (x) (values (desug x scope) (CNone))) elts)))]
    [PyGen (elt target iter)
           (type-case CExp (desug target scope)
             [CId (x) (CApp (CId 'map) (list (CFunc (list x) (hash empty) (none) (none)
                                                    (CReturn (desug elt scope))) (desug iter scope)) (hash empty) (none) (none))]
             [else (error 'desug "didnt get an id in PyGen")])]
    
                                                    
                                                          
    ;[else (begin (display expr) (CError (CStr "desugar hasn't handled a case yet")))]
    ))

(define (desugar expr)
  (begin ;(display expr)
  (hoist/desug expr empty true)))
