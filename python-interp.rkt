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
    [(err store s) (ExnA (VStr s) store)]
    [(err store s ...) (ExnA (VStr (foldr string-append "" (list s ...))) store)]))

(define-syntax interp-as
  (syntax-rules ()
    [(interp-as env0 store0 () body) body]
    [(interp-as env0 store0 ([(v0 s0) x0] [(v-rest s-rest) x-rest]...) body)
     (type-case Ans (interp-full x0 env0 store0)
       [ValA (v0 s0) (interp-as env0 s0 ([(v-rest s-rest) x-rest] ...) body)]
       [ExnA (exn-val exn-sto) (ExnA exn-val exn-sto)])]))

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
       [ValA (v s) 
             (type-case CVal v
               [VReturn (value) (ValA value s)]
               [else (ValA (VNone) s)])]
       [ExnA (v s) (ExnA v s)])]
    [(and (empty? ids) (cons? vals) (empty? defs))
     (err sto "Application failed with an arity mismatch")]
    [(and (empty? ids) (cons? vals) (cons? defs)) ; use val instead of def
     (interp-as caller-env sto ([(v s) (first vals)])
                (local ((define-values (newenv newsto) (update-env-store (VD-id (first defs)) v clo-env s)))
                  (Apply ids (rest vals) (rest defs) caller-env newsto newenv clo-body)))]
    [(and (empty? ids) (empty? vals) (cons? defs))
     (local ((define-values (newenv newsto) (update-env-store (VD-id (first defs)) (VD-val (first defs)) clo-env sto)))
       (Apply ids vals (rest defs) caller-env newsto newenv clo-body))]
    [(and (cons? ids) (cons? vals))
     (interp-as caller-env sto ([(v s) (first vals)])
                (local ((define-values  (newenv newsto) (update-env-store (first ids) v clo-env s)))
                  (Apply (rest ids) (rest vals) defs caller-env newsto newenv clo-body)))]
    
    [else (err sto "Application failed with arity mismatch")]))

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
    [VNotDefined () (VFalse)]
    [VReturn (val) (BoolEval val)]))

(define (interp-full (expr : CExp)  (env : Env)  (store : Store)) : Ans
  (type-case CExp expr
    [CNum (n) (ValA (VNum n) store)]
    [CStr (s) (ValA (VStr s) store)]
    [CTrue () (ValA (VTrue) store)]
    [CFalse () (ValA (VFalse) store)]
    [CNone () (ValA (VNone) store)]
    [CNotDefined () (ValA (VNotDefined) store)]

    [CUnary (op expr)
            (interp-as env store ([(v s) expr])
                       (case op
                         ['not (ValA (if (VTrue? (BoolEval v)) (VFalse) (VTrue)) s)]
                         [else (type-case CVal v
                                 [VNum (n) (case op
                                             ['+ (ValA v s)] ['- (ValA (VNum (- 0 n)) s)] 
                                             [else (err s "not defined on numbers: " (symbol->string op))])]
                                 [else (err s "bad operand for unary operation" (symbol->string op))])]))]
    
    [CError (ex) (type-case Ans (interp-full ex env store)
                  [ValA (v s) (ExnA v s)]
                  [ExnA (v s) (ExnA v s)])]
    [CIf (i then_block else_block)
         (interp-as env store ([(v s) i])
                    (interp-full (if (VTrue? (BoolEval v)) then_block else_block) env s))]
    
    [CId (x) 
         (type-case (optionof Location) (hash-ref env x)
               [some (loc) (ValA (some-v (hash-ref store loc)) store)]
               [none () (err store "Unbound identifier: " (symbol->string x))])]
    
    [CSet! (id val) (interp-as env store ([(v s) val])
                               (type-case (optionof Location) (hash-ref env id)
                                 [some (loc) (ValA v (hash-set s loc v))]
                                 [none () (err s "identifier '" (symbol->string id) "' not found in environment")]))]
    
    [CLet (x bind body)
          (interp-as env store([(v s) bind])
                     (local ((define-values (ne ns) (update-env-store x v env s)))
                       (interp-full body ne ns)))]
    
    [CSeq (ex1 ex2) (interp-as env store ([(v1 s1) ex1])
                               (begin
                                      (type-case CVal v1
                                        [VReturn (val) (ValA v1 s1)]
                                        [else (interp-full ex2 env s1)])))]
    
    [CReturn (val) (interp-as env store ([(v s) val])
                              (ValA (VReturn v) s))]
    
    [CApp (fun args)
          (interp-as env store ([(clos s) fun])
                     (type-case CVal clos
                       [VClosure (clo-env ids defaults body)
                                 (Apply ids args defaults env s clo-env body)]
                       [else (err s "Not a closure at application")]))]
    
    ;;iterate through default arguments
    [CFunc (args defaults body)
           (local ((define (iter cdefs vdefs sto)
                     (cond [(empty? cdefs) (ValA (VClosure env args vdefs body) sto)]
                           [else (interp-as env sto ([(v s) (CD-expr (first cdefs))])
                                            (iter (rest cdefs) (cons (VD (CD-id (first cdefs)) v) vdefs) s))])))
             (iter defaults empty store))]
          
    [CBinOp (op left right) 
            (interp-as env store ([(l s) left] [(r s2) right])
                       (cond [(and (VNum? l) (VNum? r))
                              (let ((nl (VNum-n l)) (nr (VNum-n r)))
                                (case op
                                  [(/ //) (if (zero? nr) (err s2 "divide by zero!")
                                              (ValA (VNum ((case op ['/ /] ['// (lambda (x y) (floor (/ x y)))]) nl nr)) s2))]
                                  [else (ValA (VNum ((case op ['+ +] ['- -] ['* *] ['% remainder] ['** expt]) nl nr)) s2)]))]
                             [(and (VStr? l) (VStr? r))
                              (case op
                                ['+ (ValA (VStr (s+ (VStr-s l) (VStr-s r))) s2)]
                                [else (err s2 "invalid operation on strings: " (symbol->string op))])]
                             [else (err s2 "invalid operation: " (pretty l) " " (symbol->string op) " " (pretty r) )]))]
                                       
    [CPrim1 (prim arg) 
            (interp-as env store ([(v s) arg])
                       (ValA (python-prim1 prim v) s))]
    [Compare (op left right)
             (interp-as env store ([(l s) left] [(r s2) right])
                        (case op
                          [(== is) (ValA (VBool (equal? l r)) s2)]
                          ['!= (ValA (VBool (not (equal? l r))) s2)]
                          [(< <= >= >) (ValA (cond [(and (VNum? l) (VNum? r))
                                              (VBool ((case op ['< <] ['<= <=] ['> >] ['>= >=]) (VNum-n l) (VNum-n r)))]
                                             [(and (VStr? l) (VStr? r))
                                              (VBool ((case op ['< string<?] ['<= string<=?] ['> string>?] ['>= string>=?]) (VStr-s l) (VStr-s r)))])
                                             s2)]
                                              
                          [else (err s2 "comparator not implemented: " (symbol->string op))]))]))

(define (interp expr) : CVal
  (begin ;(display expr)
  (type-case Ans (interp-full expr (hash (list)) (hash (list)))
    [ValA (v s) v]
    [ExnA (v s) (begin (error 'interp-derp (pretty v)) v)])))
