#lang plai-typed

(require "core-syntax.rkt"
         "methods.rkt"
         "primitives.rkt")
(require (typed-in racket 
                   (string<? : (string string -> boolean))
                   (string<=? : (string string -> boolean))
                   (take : ((listof 'a)  number -> (listof 'a)))
                   [drop : ((listof 'a)  number -> (listof 'a))]
                   (string>? : (string string -> boolean))
                   (string>=? : (string string -> boolean))
                   (expt : (number number -> number))))
(define s+ string-append)

(define (append-n lst n)
  (cond [(zero? n) empty]
        [else (append lst (append-n lst (- n 1)))]))

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


(define globals : (hashof symbol CVal) (hash empty))

(define update-env-store
  (let ((count 0))
    (lambda (id (val : CVal) env store (type : symbol)) : (Env * Store)
      (begin (set! count (add1 count))
             (type-case Env env
               [Ev (locals nonlocals)
                   (values (case type
                             ['local (Ev (hash-set locals id count) nonlocals)]
                             ['nonlocal (Ev locals (hash-set nonlocals id count))])
                           (hash-set store count val))])))))
(define (get-list (h : (hashof 'a 'b))) : (listof ('a * 'b))
  (map (lambda (k) (values k (some-v (hash-ref h k)))) (hash-keys h)))

;;go through each of our scopes
(define (lookup sym env type) : (optionof Location)
  (type-case Env env
    [Ev (locals nonlocals)
        (case type
          ['local
           (type-case (optionof Location) (hash-ref locals sym)
             [some (loc) (some loc)]
             [none () (hash-ref nonlocals sym)])]
          ['nonlocal (hash-ref nonlocals sym)]
          [else (none)])]))

(define (reset-scopes env)
  (type-case Env env
    [Ev (locals nonlocals)
        (local ((define (r locs nonlocs)
                  (if (empty? locs) nonlocs
                      (r (rest locs) (hash-set nonlocs (first locs) (some-v (hash-ref locals (first locs))))))))
          (Ev (hash empty) (r (hash-keys locals) nonlocals)))]))

;;the bulk of the AppC case
;;recursively builds up the arguments (binds their values to ids)
;;and updates the closures environment / callers store



(define (iter-lst cvals vals env sto mutable) : Ans
  (cond [(empty? cvals) (ValA (VList mutable vals) sto)]
        [(cons? cvals) (interp-as env sto ([(val s) (first cvals)])
                                  (iter-lst (rest cvals) (cons val vals) env s mutable))]))

(define (val-lst (x1 : (listof symbol)) (x2 : (listof CVal)))
  (map2 (lambda (x y) (values x y)) x1 x2))
(define (get-vals h)
  (map (lambda (x) (some-v (hash-ref h x))) (hash-keys h)))
(define (get-val-lst h)
  (map (lambda (x) (values x (some-v (hash-ref h x)))) (hash-keys h)))

;;ordered, keys
;;ids, defaults
(define (apply-it (ordered : (listof CVal)) (keys : (hashof symbol CVal)) (closure : CVal) store)
  (type-case CVal closure
    [VClosure (env ids defaults star kwarg body)
              (local ((define li (length ids))
                      (define lo (length ordered))
                      (define-values (restids restordered val-hash) 
                        (if (> lo li)
                            (values empty (drop ordered li) (make-hash (val-lst ids (take ordered li))))
                            (values (take ids lo) empty (make-hash (val-lst (take ids lo) ordered))))))
                (begin (if (some? star)
                           (hash-set! val-hash (some-v star) (VList false restordered))
                           (void))
                       ;;ADD THE KEYS
                       ;;returns an option of an ExnA, default tells us we shouldnt error out if id is already bound (but dont overwrite it)
                       (local ((define (add-keys (ks : (listof (symbol * CVal))) default?)
                                 (cond [(empty? ks) (none)]
                                       [(cons? ks)
                                        (local ((define-values (k v) (first ks))
                                                (define op (hash-ref val-hash k)))
                                          (cond [(some? op) (if default?
                                                                (add-keys (rest ks) default?)
                                                                (some (err store "identifier already bound: " (symbol->string k))))]
                                                [else (begin (hash-set! val-hash k v) (add-keys (rest ks) default?))]))])))
                         (type-case (optionof Ans) (add-keys (get-val-lst keys) false)
                           [some (v) v]
                           [none () (type-case (optionof Ans) (add-keys (get-val-lst defaults) true)
                                      [some (v) (error 'interp "should never get here: default arguemnts")]
                                      [none () ;;add all in val-hash to closures environment
                                            (local ((define (add-to-env (ks : (listof (symbol * CVal))) this-env this-store)
                                                      (cond [(empty? ks)      
                                                             (type-case Ans (interp-full body this-env this-store)
                                                               [ValA (v s) 
                                                                     (type-case CVal v
                                                                       [VReturn (value) (ValA value s)]
                                                                       [else (ValA (VNone) s)])]
                                                               [ExnA (v s) (ExnA v s)])]
                                                            [(cons? ks) (local ((define-values (k v) (first ks))
                                                                                (define-values (newe news) (update-env-store k v this-env this-store 'local)))
                                                                          (add-to-env (rest ks) newe news))])))
                                              (add-to-env (get-val-lst val-hash) env store))])]))))]
    [else (err store "non closure at application")]))
                                                                             
     
;;apply the other reverse~
(define (Apply (vals : (listof CExp)) (keys : (hashof symbol CExp)) (caller-env : Env) (sto : Store) closure) : Ans
  (type-case Ans (iter-lst vals empty caller-env sto false)
    [ValA (v s) 
          (type-case CVal v
            [VList (m ordered) 
                   (type-case Ans (iter-lst (get-vals keys) empty caller-env sto false)
                     [ValA (key-vals s) 
                           (type-case CVal key-vals
                             [VList (m key-lst)
                                    (let ((key-dict (hash (val-lst (hash-keys keys) (reverse key-lst)))))
                                      (apply-it (reverse ordered) key-dict closure s))]
                             [else (error 'interp "shouldnt get here")])]
                     [ExnA (v s) (ExnA v s)])]
            [else (error 'interp "shouldn't get here")])]
    [ExnA (v s) (ExnA v s)]))
#|
  (cond
    [(and (empty? ids) (empty? vals) (empty? defs)) 
     (type-case Ans (interp-full clo-body clo-env sto)
       [ValA (v s) 
             (type-case CVal v
               [VReturn (value) (ValA value s)]
               [else (ValA (VNone) s)])]
       [ExnA (v s) (ExnA v s)])]
    [(and (empty? ids) (cons? vals) (empty? defs))
     (err sto "Application failed with an arity mismatch: too many values given")]
    [(and (empty? ids) (cons? vals) (cons? defs)) ; use val instead of def
     (interp-as caller-env sto ([(v s) (first vals)])
                (local ((define-values (newenv newsto) (update-env-store (VD-id (first defs)) v clo-env s 'local)));;update the local environment
                  (Apply ids (rest vals) (rest defs) caller-env newsto newenv clo-body)))]
    [(and (empty? ids) (empty? vals) (cons? defs))
     (local ((define-values (newenv newsto) (update-env-store (VD-id (first defs)) (VD-val (first defs)) clo-env sto 'local)))
       (Apply ids vals (rest defs) caller-env newsto newenv clo-body))]
    [(and (cons? ids) (cons? vals))
     (interp-as caller-env sto ([(v s) (first vals)])
                (local ((define-values  (newenv newsto) (update-env-store (first ids) v clo-env s 'local)))
                  (Apply (rest ids) (rest vals) defs caller-env newsto newenv clo-body)))]
    
    [else (err sto "Application failed with arity mismatch: not enough values given to function")]))
|#
;; RANDOM ASS COMMENT LINE!

(define (iter-hash cvals vals env sto type)
  (cond [(empty? cvals) (ValA (type (make-hash vals)) sto)]
        [(cons? cvals) (local ((define-values (k c) (first cvals)))
                         (interp-as env sto ([(key s) k] [(value s2) c])
                                    (iter-hash (rest cvals) (cons (values key value) vals) env s type)))]))

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
         (type-case (optionof Location) (lookup x env 'local)
           [some (loc) (ValA (some-v (hash-ref store loc)) store)]
           [none () (type-case (optionof CVal) (hash-ref globals x)
                      [some (v) (ValA v store)]
                      [none () (err store "identifier not found " (symbol->string x))])])]
    [CNonLocalId (x)(type-case (optionof Location) (lookup x env 'nonlocal)
                      [some (loc) (ValA (some-v (hash-ref store loc)) store)]
                      [none () (err store "Unbound identifier: " (symbol->string x))])]
    [CGlobalId (x) 
               (type-case (optionof CVal) (hash-ref globals x)
                 [some (v) (ValA v store)]
                 [none () (err store "Unbound global identifier: " (symbol->string x))])]
    
    [CSet! (id val type) 
           (interp-as env store ([(v s) val])
                      (case type
                        [(local nonlocal)
                         (type-case (optionof Location) (lookup id env type)
                           [some (loc) (ValA v (hash-set s loc v))]
                           [none () (err s "CSet!: identifier '" (symbol->string id) "' not found in environment")])]
                        ['global (begin (set! globals (hash-set globals id v)) (ValA v s))]))]
    
    
    [CLet (x type bind body)
          (begin 
            (interp-as env store([(v s) bind])
                       (case type
                         ['local (local ((define-values (ne ns) (update-env-store x v env s 'local)))
                                   (interp-full body ne ns))]
                         ;;the globals / nonlocals in a function
                         [else (type-case (optionof Location) (lookup x env type)
                                 [some (loc) (interp-full body env s)];do nothing cause it's already in our store
                                 [none ()  (case type
                                             ['global (begin (if (and (VNotDefined? v) (some? (hash-ref globals x))) 
                                                                 (void) 
                                                                 (set! globals (hash-set globals x v))) (interp-full body env s))]
                                             ['nonlocal (err s "no binding for nonlocal '" (symbol->string x) "' found")])])])))]
    
    [CSeq (ex1 ex2) (interp-as env store ([(v1 s1) ex1])
                               (type-case CVal v1
                                 [VReturn (val) (ValA v1 s1)]
                                 [else (interp-full ex2 env s1)]))]
    [CList (mutable elts) (local ((define (iter cvals vals sto)
                                    (cond [(empty? cvals) (ValA (VList mutable (reverse vals)) sto)]
                                          [(cons? cvals) (interp-as env sto ([(v s) (first cvals)])
                                                                    (iter (rest cvals) (cons v vals) s))])))
                            (iter elts empty store))]
    [CSlice (l low up st)
            (interp-as env store ([(lst s) l] [(lower s2) low] [(upper s3) up] [(step s4) st])
                       (slice lst lower upper step s4))]
    [CIndex (l i) (interp-as env store ([(lst s) l] [(idex s2) i])
                             (index lst idex s2))]
    
    [CReturn (val) (interp-as env store ([(v s) val])
                              (ValA (VReturn v) s))]
    
    [CObject (fields) (iter-hash (get-list fields) empty env store VObject)]
    [CDict (fields) (iter-hash (get-list fields) empty env store VDict)]
    [CSet (fields) (iter-hash (get-list fields) empty env store VSet)]
    [CGet (obj field)
          (begin 
            (interp-as env store ([(o s) obj] [(f s2) field])
                       (type-case CVal o
                         [VObject (fields)
                                  (type-case (optionof CVal) (hash-ref fields f)
                                    [some (v) (ValA v s2)]
                                    [none () (err s2 "object lookup failed: " (pretty f))])]
                         [VDict (elts)
                                (type-case CVal f
                                  [VStr (s) (case (string->symbol s)
                                              [(items clear values keys) (ValA (VClosure env (list '-the-dict) (hash empty) (none) (none)
                                                                                         (CReturn (CPrim1 (string->symbol s) (CId '-the-dict)))) s2)]
                                              
                                              [else (err s2 "dict has not method " s)])]
                                  [else (err s2 "dict lookup")])]
                         [else (err s2 (pretty o) " is not an object, failed at lookup")])))]
                         
    [CSetAttr (obj field val)
              (interp-as env store ([(o s) obj] [(f s2) field] [(v s3) val])
                         (type-case CVal o
                           [VObject (fields)
                                    (type-case CVal f
                                      [VStr (s) (begin (hash-set! fields f v) (ValA (VNone) s3))]
                                      [else (err s3 "cannot reference object with " (pretty f))])]
                           [else (err s3 "cannot access field of " (pretty o))]))]
    [CApp (fun args keys star kwarg)
          (interp-as env store ([(clos s) fun])
                     (type-case CVal clos
                       [VClosure (clo-env ids defaults star kwarg body)
                                 (Apply args keys env s clos)]
                       [VObject (fields)
                                (type-case (optionof CVal) (hash-ref fields (VStr "__call__"))
                                  [some (v) (type-case CVal v
                                              [VClosure (clo-env ids defaults star kwarg body)
                                                        (let ((newargs (type-case (optionof CVal) (hash-ref fields (VStr "__class__"))
                                                                         [some (st) (type-case CVal st
                                                                                      [VStr (class) (if (string=? class "class") args (cons fun args))]
                                                                                      [else (cons fun args)])]
                                                                         [none () (cons fun args)])))
                                                          (Apply newargs keys env s v))]                                                       
                                              [else (err s "cannot call the object")])]
                                  [none () (err s "object does not have a __call__ attr")])]
                       
                       [else (err s "Not a closure at application: " (pretty clos))]))]
;;INVARIANT: fun + arg must be ids
    [CPartialApply (fun arg)
                   (interp-as env store ([(clos s) fun] [(a s2) arg])
                              (type-case CVal clos;;we know that clos and arg is going to be an id, so interping is fine
                                [VClosure (clo-env ids defaults star kwarg body)
                                          (local ((define-values (enew snew) (update-env-store (first ids) a clo-env s 'local)))
                                            (ValA (VClosure enew (rest ids) defaults star kwarg body) snew))]
                                [else (err s2 "partial application of nonfunction")]))]
    
    ;;iterate through default arguments
    [CFunc (args defaults star kwarg body)
           (local ((define (iter cdefs vdefs sto)
                     (cond [(empty? cdefs)
                            (ValA (VClosure (reset-scopes env) args (hash vdefs) star kwarg body) sto)]
                           [else (interp-as env sto ([(v s) (some-v (hash-ref defaults (first cdefs)))])
                                            (iter (rest cdefs) (cons (values (first cdefs) v) vdefs) s))])))
             (iter (hash-keys defaults) empty store))]
    
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
                             [(and (VList? l) (VList? r) (eq? (VList-mutable r) (VList-mutable l)))
                              (case op
                                ['+ (ValA (VList (VList-mutable l) (append (VList-elts l) (VList-elts r))) s2)])]
                             [(and (VList? l) (VNum? r))
                              (case op
                                ['* (ValA (VList (VList-mutable l) (append-n (VList-elts l) (VNum-n r))) s2)])]
                             [(and (VList? r) (VNum? l))
                              (case op
                                ['* (ValA (VList (VList-mutable r) (append-n (VList-elts r) (VNum-n l))) s2)])]
                             [(and (VSet? r) (VSet? l)) (set-op op l r s2)]
                             
                             [else (err s2 "invalid operation: " (pretty l) " " (symbol->string op) " " (pretty r) )]))]
    
    [CPrim1 (prim arg) 
            (interp-as env store ([(v s) arg])
                       (python-prim1 prim v s))]
    [Compare (op left right)
             (interp-as env store ([(l s) left] [(r s2) right])
                        (case op
                          [(is isnot) (ValA (VBool ((case op ['is identity] ['isnot not])
                                                    ((type-case CVal l
                                                      [VList (m l) eq?]
                                                      [VObject (f) eq?]
                                                      [else equal?]) l r))) s2)]
                          ['isnot (ValA (VBool (not (eq? l r))) s2)]
                          [(==) (ValA (VBool (equal? l r)) s2)]
                          [(!=) (ValA (VBool (not (equal? l r))) s2)]
                          [(< <= >= >) (ValA (cond [(and (VNum? l) (VNum? r))
                                                    (VBool ((case op ['< <] ['<= <=] ['> >] ['>= >=]) (VNum-n l) (VNum-n r)))]
                                                   [(and (VStr? l) (VStr? r))
                                                    (VBool ((case op ['< string<?] ['<= string<=?] ['> string>?] ['>= string>=?]) (VStr-s l) (VStr-s r)))])
                                             s2)]
                          [(in notin) (type-case CVal r
                                        [VList (m elts) (ValA (VBool ((case op ['in identity] ['notin not]) (member l elts))) s2)]
                                        [VDict (h) (ValA (VBool ((case op ['in identity] ['notin not]) (member l (hash-keys h)))) s2)]
                                        [else (err s2 "in not implemented for this type")])]
                          
                          [else (err s2 "comparator not implemented: " (symbol->string op))]))]))


(define (interp expr) : CVal
  (begin ;(display expr)
    (set! globals (hash empty))
    (type-case Ans (interp-full expr (Ev (hash empty) (hash empty)) (hash empty))
      [ValA (v s) v]
      [ExnA (v s) (begin (error 'interp-derp (pretty v)) v)])))

