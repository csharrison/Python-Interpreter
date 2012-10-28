#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")
(require (typed-in racket 
                   (string<? : (string string -> boolean))
                   (string<=? : (string string -> boolean))
                   (string>? : (string string -> boolean))
                   (string>=? : (string string -> boolean))
                   (expt : (number number -> number))))
(define s+ string-append)

                   

;;err : calls an error with all input strings appended
(define-syntax err
  (syntax-rules ()
    [(err e store s) (ExnA (VStr s) e store)]
    [(err e store s ...) (ExnA (VStr (foldr string-append "" (list s ...))) e store)]))

(define-syntax interp-as
  (syntax-rules ()
    [(interp-as env0 store0 () body) body]
    [(interp-as env0 store0 ([(v0 e0 s0) x0] [(v-rest e-rest s-rest) x-rest]...) body)
     (type-case Ans (interp-full x0 env0 store0)
       [ValA (v0 e0 s0) (interp-as e0 s0 ([(v-rest e-rest s-rest) x-rest] ...) body)]
       [ExnA (exn-val exn-env exn-sto) (ExnA exn-val exn-env exn-sto)])]))

(define update-env-store
  (let ((count 0))
    (lambda (id (val : CVal) env store) : (Env * Store)
      (begin (set! count (add1 count))
             (values (hash-set env id count)
                     (hash-set store count val))))))

;;the bulk of the AppC case
;;recursively builds up the arguments (binds their values to ids)
;;and updates the closures environment / callers store
(define (Apply (ids : (listof symbol)) (vals : (listof CExp)) (defs : (listof VDefault)) (caller-env : Env) (sto : Store) (clo-env : Env) (clo-body : CExp)) : Ans
  (cond
    [(and (empty? ids) (empty? vals) (empty? defs)) 
     (type-case Ans (interp-full clo-body clo-env sto)
       [ValA (v e s) 
             (type-case CVal v
               [VReturn (value) (ValA value caller-env s)]
               [else (ValA (VNone) caller-env s)])]
       [ExnA (v e s) (ExnA v e s)])]
    [(and (empty? ids) (cons? vals) (empty? defs))
     (err caller-env sto "Application failed with an arity mismatch")]
    [(and (empty? ids) (cons? vals) (cons? defs)) ; use val instead of def
     (interp-as caller-env sto ([(v e s) (first vals)])
                (local ((define-values (newenv newsto) (update-env-store (VD-id (first defs)) v clo-env s)))
                  (Apply ids (rest vals) (rest defs) e newsto newenv clo-body)))]
    [(and (empty? ids) (empty? vals) (cons? defs))
     (local ((define-values (newenv newsto) (update-env-store (VD-id (first defs)) (VD-val (first defs)) clo-env sto)))
       (Apply ids vals (rest defs) caller-env newsto newenv clo-body))]
    [(and (cons? ids) (cons? vals))
     (interp-as caller-env sto ([(v e s) (first vals)])
                (local ((define-values  (newenv newsto) (update-env-store (first ids) v clo-env s)))
                  (Apply (rest ids) (rest vals) defs e newsto newenv clo-body)))]
    
    [else (err caller-env sto "Application failed with arity mismatch")]))

;; RANDOM ASS COMMENT LINE!

(define (VBool v)
  (if v (VTrue) (VFalse)))
(define (BoolEval (val : CVal)) : CVal
  (type-case CVal val
    [VNum (n) (VBool (not (zero? n)))]
    [VStr (s) (VBool (not (string=? "" s)))]
    [VTrue () (VTrue)]
    [VFalse () (VFalse)]
    [VNone () (VFalse)]
    [VClosure (e args defs b) (VTrue)]
    [VReturn (val) (BoolEval val)]))

(define (interp-full (expr : CExp)  (env : Env)  (store : Store)) : Ans
  (type-case CExp expr
    [CNum (n) (ValA (VNum n) env store)]
    [CStr (s) (ValA (VStr s) env store)]
    [CTrue () (ValA (VTrue) env store)]
    [CFalse () (ValA (VFalse) env store)]
    [CNone () (ValA (VNone) env store)]

    [CUnary (op expr)
            (interp-as env store ([(v e s) expr])
                       (case op
                         ['not (ValA (if (VTrue? (BoolEval v)) (VFalse) (VTrue)) e s)]
                         [else (type-case CVal v
                                 [VNum (n) (case op
                                             ['+ (ValA v e s)] ['- (ValA (VNum (- 0 n)) e s)] 
                                             [else (err e s "not defined on numbers: " (symbol->string op))])]
                                 [else (err e s "bad operand for unary operation" (symbol->string op))])]))]
    
    [CError (ex) (type-case Ans (interp-full ex env store)
                  [ValA (v e s) (ExnA v e s)]
                  [ExnA (v e s) (ExnA v e s)])]
    [CIf (i then_block else_block)
         (interp-as env store ([(v e s) i])
                    (interp-full (if (VTrue? (BoolEval v)) then_block else_block) e s))]
    
    [CId (x) (type-case (optionof Location) (hash-ref env x)
               [some (loc) (ValA (some-v (hash-ref store loc)) env store)]
               [none () (err env store "Unbound identifier: " (symbol->string x))])]
    
    [CSet! (id val) (interp-as env store ([(v e s) val])
                               (type-case (optionof Location) (hash-ref e id)
                                 [some (loc) (ValA v e (hash-set s loc v))]
                                 [none () (local ((define-values (ne ns) (update-env-store id v e s)))
                                            (ValA v ne ns))]))]
    
    [CLet (x bind body)
          (interp-as env store([(v e s) bind])
                     (local ((define-values (ne ns) (update-env-store x v e s)))
                       (interp-full body ne ns)))]
    
    [CSeq (ex1 ex2) (interp-as env store ([(v1 e1 s1) ex1])
                               (begin
                                      (type-case CVal v1
                                        [VReturn (val) (ValA v1 e1 s1)]
                                        [else (interp-full ex2 e1 s1)])))]
    
    [CReturn (val) (interp-as env store ([(v e s) val])
                              (ValA (VReturn v) e s))]
    
    [CApp (fun args)
          (interp-as env store ([(clos e s) fun])
                     (type-case CVal clos
                       [VClosure (clo-env ids defaults body)
                                 (Apply ids args defaults e s clo-env body)]
                       [else (err e s "Not a closure at application")]))]
    
    ;;iterate through default arguments
    [CFunc (args defaults body)
           (local ((define (iter cdefs vdefs environ sto)
                     (cond [(empty? cdefs) (ValA (VClosure environ args vdefs body) environ sto)]
                           [else (interp-as env sto ([(v e s) (CD-expr (first cdefs))])
                                            (iter (rest cdefs) (cons (VD (CD-id (first cdefs)) v) vdefs) e s))])))
             (iter defaults empty env store))]
          
    [CBinOp (op left right) 
            (interp-as env store ([(l e s) left] [(r e2 s2) right])
                       (cond [(and (VNum? l) (VNum? r))
                              (let ((nl (VNum-n l)) (nr (VNum-n r)))
                                (case op
                                  [(/ //) (if (zero? nr) (err e2 s2 "divide by zero!")
                                              (ValA (VNum ((case op ['/ /] ['// (lambda (x y) (floor (/ x y)))]) nl nr)) e2 s2))]
                                  [else (ValA (VNum ((case op ['+ +] ['- -] ['* *] ['% remainder] ['** expt]) nl nr)) e2 s2)]))]
                             [(and (VStr? l) (VStr? r))
                              (case op
                                ['+ (ValA (VStr (s+ (VStr-s l) (VStr-s r))) e2 s2)]
                                [else (err e2 s2 "invalid operation on strings: " (symbol->string op))])]
                             [else (err e2 s2 "invalid operation!")]))]
                                       
    [CPrim1 (prim arg) 
            (interp-as env store ([(v e s) arg])
                       (ValA (python-prim1 prim v) e s))]
    [Compare (op left right)
             (interp-as env store ([(l e s) left] [(r e2 s2) right])
                        (case op
                          [(== is) (ValA (VBool (equal? l r)) e2 s2)]
                          ['!= (ValA (VBool (not (equal? l r))) e2 s2)]
                          [(< <= >= >) (ValA (cond [(and (VNum? l) (VNum? r))
                                              (VBool ((case op ['< <] ['<= <=] ['> >] ['>= >=]) (VNum-n l) (VNum-n r)))]
                                             [(and (VStr? l) (VStr? r))
                                              (VBool ((case op ['< string<?] ['<= string<=?] ['> string>?] ['>= string>=?]) (VStr-s l) (VStr-s r)))])
                                             e2 s2)]
                                              
                          [else (err e2 s2 "comparator not implemented: " (symbol->string op))]))]))

(define (interp expr) : CVal
  (begin ;(display expr)
  (type-case Ans (interp-full expr (hash (list)) (hash (list)))
    [ValA (v e s) v]
    [ExnA (v e s) (begin (error 'interp (pretty v)) v)])))

