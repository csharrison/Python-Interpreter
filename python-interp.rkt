#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

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
(define (Apply (ids : (listof symbol)) (vals : (listof CExp)) (caller-env : Env) (sto : Store) (clo-env : Env) (clo-body : CExp)) : Ans
  (cond [(and (empty? ids) (empty? vals)) (interp-full clo-body clo-env sto)]
        [(and (cons? ids) (cons? vals))
         (interp-as caller-env sto ([(v s) (first vals)])
                    (local ((define-values  (newenv newsto) (update-env-store (first ids) v clo-env s)))
                      (Apply (rest ids) (rest vals) caller-env newsto newenv clo-body)))]
                      
        [else (err sto "Application failed with arity mismatch")]))

(define (interp-full (expr : CExp)  (env : Env)  (store : Store)) : Ans
  (type-case CExp expr
    [CNum (n) (ValA (VNum n) store)]
    [CStr (s) (ValA (VStr s) store)]
    [CTrue () (ValA (VTrue) store)]
    [CFalse () (ValA (VFalse) store)]
    [CNone () (ValA (VNone) store)]

    [CError (e) (type-case Ans (interp-full e env store)
                  [ValA (v s) (ExnA v s)]
                  [ExnA (v s) (ExnA v s)])]
    [CIf (i t e)
         (interp-as env store ([(v s) i])
                    (interp-full (if (VTrue? v) t e) env s))]
    
    [CId (x) (type-case (optionof Location) (hash-ref env x)
               [some (loc) (ValA (some-v (hash-ref store loc)) store)]
               [none () (err store "Unbound identifier")])]
    
    [CLet (x bind body)
          (interp-as env store([(v s) bind])
                     (local ((define-values (ne ns) (update-env-store x v env s)))
                       (interp-full body ne ns)))]
    [CSeq (e1 e2) (interp-as env store ([(v1 s1) e1]) 
                             (interp-full e2 env s1))]
    [CApp (fun args)
          (interp-as env store ([(clos s) fun])
                     (type-case CVal clos
                       [VClosure (clo-env ids body)
                                 (Apply ids args env s clo-env body)]
                       [else (err s "Not a closure at application")]))]

    [CFunc (args body) (ValA (VClosure env args body) store)] 

    [CPrim1 (prim arg) 
            (interp-as env store ([(v s) arg])
                       (ValA (python-prim1 prim v) s))]
    [else (err store "Not implemented")]))

(define (interp expr) : CVal
  (begin ;(display expr)
  (type-case Ans (interp-full expr (hash (list)) (hash (list)))
    [ValA (v s) v]
    [ExnA (v s) (begin (error 'interp (pretty v)) v)])))

