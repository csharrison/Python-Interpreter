#lang plai-typed

(require "core-syntax.rkt"
         "methods.rkt"
         "primitives.rkt"
         "python-lib.rkt")
(require (typed-in racket 
                   (string<? : (string string -> boolean))
                   (string<=? : (string string -> boolean))
                   (take : ((listof 'a)  number -> (listof 'a)))
                   [drop : ((listof 'a)  number -> (listof 'a))]
                   (string>? : (string string -> boolean))
                   (inexact? : (number -> boolean))
                   (exact? : (number -> boolean))
                   (string>=? : (string string -> boolean))
                   (expt : (number number -> number))))
(define s+ string-append)

(define (str-mult str n)
  (cond [(zero? n) ""]
        [else (string-append str (str-mult str (- n 1)))]))
(define (append-n lst n)
  (cond [(zero? n) empty]
        [else (append lst (append-n lst (- n 1)))]))


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
                                                                (some (err store "TypeError" "identifier already bound: " (symbol->string k))))]
                                                [else (begin (hash-set! val-hash k v) (add-keys (rest ks) default?))]))])))
                         (type-case (optionof Ans) (add-keys (get-val-lst keys) false)
                           [some (v) v]
                           [none () (type-case (optionof Ans) (add-keys (get-val-lst defaults) true)
                                      [some (v) (error 'interp "should never get here: default arguemnts")]
                                      [none () ;;add all in val-hash to closures environment
                                            (local ((define (add-to-env (ks : (listof (symbol * CVal))) this-env this-store) : Ans
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
                                              (if (or (< (length (hash-keys val-hash)) (length ids))
                                                      (and (none? star) (> (length ordered) (length ids))))
                                                  (err store "TypeError" "arity mismatch")
                                                  (begin 
                                                  (add-to-env (get-val-lst val-hash) env store))))])]))))]
    [else (err store "TypeError" "non closure at application")]))
                                                                             
     
;;apply the other reverse~
(define (Apply (vals : (listof CExp)) (keys : (hashof symbol CExp)) (caller-env : Env) (sto : Store) closure stararg) : Ans
  (type-case Ans (iter-lst vals empty caller-env sto false)
    [ValA (v s) 
          (type-case CVal v
            [VList (m ordered) 
                   (type-case Ans (iter-lst (get-vals keys) empty caller-env s false)
                     [ValA (key-vals s) 
                           (type-case CVal key-vals
                             [VList (m key-lst)
                                    (let ((key-dict (hash (val-lst (hash-keys keys) (reverse key-lst)))))
                                      (apply-it (append (reverse ordered) (if (VList? stararg) (VList-elts stararg) empty)) key-dict closure s))]
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

(define (find-first (l : (listof 'a)) (tst : ('a -> boolean))) : (optionof 'a)
  (cond
    [(empty? l) (none)]
    [(cons? l) (let ([consider (first l)])
                 (if (tst consider)
                     (some consider)
                     (find-first (rest l) tst)))]))

(define (matches-any-exn (l : (listof CVal)) (exn-type : CVal)) : boolean
  (cond
    [(empty? l) false]
    [(cons? l) (let ([consider (first l)])
                 (type-case CVal consider
                   [VObject (fields) (type-case (optionof CVal) (hash-ref fields (VStr "__exceptionclass__"))
                                       [some (classname) (if (equal? classname exn-type)
                                                             true
                                                             (matches-any-exn (rest l) exn-type))]
                                       [none () (error 'interp-matches-any-exn "non class exception declaration found")])]
                   [else (error 'interp-matches-any-exn "uh oh, non object found")]))]))

(define (findhandler (exn : CVal) (handlers : (listof CExp)) (catchall : (optionof CExp)) (env : Env) (s : Store)) : (optionof CExp)
  (cond 
    [(empty? handlers) catchall]
    [(cons? handlers) (let ([handler (first handlers)]) 
                        (type-case CVal exn
                          [VObject (exn-fields) 
                                   (type-case (optionof CVal) (hash-ref exn-fields (VStr "__exceptiontype__"))
                                     [some (given-type-vstr)
                                           (type-case CExp handler
                                             [CExceptHandler (body type name) 
                                                             (type-case (optionof CExp) type
                                                               [some (type-actual)
                                                                     (type-case Ans (interp-full type-actual env s)
                                                                       [ValA (v ns)
                                                                             (type-case CVal v
                                                                               [VObject (fields) 
                                                                                        (type-case (optionof CVal) (hash-ref fields (VStr "__exceptionclass__"))
                                                                                          [some (e) (if (equal? e given-type-vstr)
                                                                                                        (some handler)
                                                                                                        (findhandler exn (rest handlers) catchall env s))]
                                                                                          [none () (error 'interp-findhandler "handler specified exception must be a class")])]
                                                                               [VList (mut lst) (if (matches-any-exn lst given-type-vstr)
                                                                                                    (some handler)
                                                                                                    (findhandler exn (rest handlers) catchall env s))]
                                                                                      
                                                                               [else (error 'interp-findhandler "exceptions must be base objects")])]
                                                                       [else (error 'interp-findhandler-holyshit "HAHAHA")])]
                                                               [none () (findhandler exn (rest handlers) (some handler) env s)])]
                                             [else (error 'interp-findhandler "non-CExceptHandler found in handlers list")])]
                                     [none () (error 'interp-findhandler "exception must have __exceptiontype__ field")])]
                          [else (error 'interp-findhandler "non obect exception given")]))]))
                        
(define (in-scope (id : symbol) (env : Env) (check-global : boolean)) : boolean
  (type-case (optionof Location) (hash-ref (Ev-locals env) id)
    [some (loc) true]
    [none () (if check-global
                 (type-case (optionof Location) (hash-ref (Ev-nonlocals env) id)
                   [some (loc) true]
                   [none () false])
                 false)]))

(define (interp-full (expr : CExp)  (env : Env)  (store : Store)) : Ans
  (begin ; (display env) (display "\n") (display store) (display "\n") (display expr) (display "\n\n\n")
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
                                             [else (err s "TypeError" "not defined on numbers: " (symbol->string op))])]
                                 [VTrue () (ValA (VNum (case op ['+ 1] ['- -1] ['~ -2])) s)]
                                 [VFalse () (ValA (VNum (case op ['~ -1] [else 0])) s)]
                                 [else (err s  "TypeError" "bad operand for unary operation" (symbol->string op))])]))]
    
    [CError (ex) (type-case Ans (interp-full ex env store)
                   [ValA (v s) (type-case CVal v
                                 [VObject (fields) (ExnA v s)]
                                 [VStr (msg) (err s "Exception" msg)]
                                 [VNone () (type-case Ans (interp-full (CId '__the-exn__) env store)
                                             [ValA (exn-v exn-s) (ExnA exn-v exn-s)]
                                             [ExnA (exn-v exn-s) (if (and (exn-is exn-v "NameError") (exn-says exn-v "__the-exn__"))
                                                                     (interp-full (CError (Cmake-exn "RuntimeError" "No active exception")) env store)
                                                                     (ExnA exn-v exn-s))])]
                                 [else (err s "TypeError" "raised exceptions must be base type objects")])]
                   [ExnA (v s) (type-case CVal v
                                 [VObject (fields)
                                          ; CHANGE BELOW TO CHECK FOR NAME ERROR ON 'the-exn VERSUS OTHER STUF
                                          (type-case (optionof CVal) (hash-ref fields (VStr "__errexp__"))
                                            [some (errexp) (ExnA v s)]
                                            [none () (ExnA v s)])]
                                 [VNone () (type-case Ans (interp-full (CId '__the-exn__) env store)
                                             [ValA (exn-v exn-s) (ExnA exn-v exn-s)]
                                             [ExnA (exn-v exn-s) (if (and (exn-is exn-v "NameError") (exn-says exn-v "__the-exn__"))
                                                                     (interp-full (CError (Cmake-exn "RuntimeError" "No active exception")) env store)
                                                                     (ExnA exn-v exn-s))])]
                                 [VStr (msg) (err s "Exception" msg)]
                                 [else (err s "TypeError" "raised exceptions must be base type obects")])])]
    [CIf (i then_block else_block)
         (interp-as env store ([(v s) i])
                    (interp-full (if (VTrue? (BoolEval v)) then_block else_block) env s))]
    
    [CId (x) (begin ;(display (string-append "  env pre lookup of " (symbol->string x))) (display env) (display " ") (display (Ev-locals env)) (display (hash-keys (Ev-locals env))) (display (hash-ref (Ev-locals env) x))  (display "\n\n")
         (type-case (optionof Location) (lookup x env 'local)
           [some (loc) (type-case (optionof CVal) (hash-ref store loc)
                         [some (gotit) (if (VNotDefined? gotit) (if (some? (hash-ref (Ev-locals env) x)) 
                                                                    (err store "UnboundLocalError" "not found" (symbol->string x))
                                                                    (err store "NameError" "not found")) (ValA gotit store))]
                         [none () (error 'interp (string-append "value in environment not in store: " (symbol->string x)))])]
           [none () (type-case (optionof CVal) (hash-ref globals x)
                      [some (v) (ValA v store)]
                      [none () (err store "NameError" "identifier not found " (symbol->string x))])]))]
    [CNonLocalId (x)(type-case (optionof Location) (lookup x env 'nonlocal)
                      [some (loc) (ValA (some-v (hash-ref store loc)) store)]
                      [none () (err store "NameError" "Unbound identifier: " (symbol->string x))])]
    [CGlobalId (x) 
               (type-case (optionof CVal) (hash-ref globals x)
                 [some (v) (ValA v store)]
                 [none () (err store "NameError" "Unbound global identifier: " (symbol->string x))])]
    
    [CSet! (id val type) 
           (interp-as env store ([(v s) val])
                      (case type
                        [(local nonlocal)
                         (type-case (optionof Location) (lookup id env type)
                           [some (loc) (begin (ValA v (hash-set s loc v)))]
                           [none () (err s "NameError" "CSet!: identifier '" (symbol->string id) "' not found in environment")])]
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
                                             ['nonlocal (err s "NameError" "no binding for nonlocal '" (symbol->string x) "' found")])])])))]
    
    [CSeq (ex1 ex2) (interp-as env store ([(v1 s1) ex1])
                               (type-case CVal v1
                                 [VReturn (val) (ValA v1 s1)]
                                 [else (interp-full ex2 env s1)]))]
    [CList (mutable elts) (local ((define (iter cvals vals sto)
                                    (cond [(empty? cvals) (ValA (VList mutable (reverse vals)) sto)]
                                          [(cons? cvals) (interp-as env sto ([(v s) (first cvals)])
                                                                    (iter (rest cvals) (cons v vals) s))])))
                            (iter elts empty store))]
    [CRange (start end step)
            (interp-as env store ([(sta s) start] [(en s2) end] [(st s3) step])
                       (type-case CVal sta
                         [VNum (s) (type-case CVal en
                                     [VNum (stop) (type-case CVal st
                                                    [VNum (step) 
                                                          (if (foldl (lambda (x r) (and (exact? x) r)) true (list s stop step))
                                                              (if (not (= step 0))    
                                                                  (ValA (VRange sta en st) s3)
                                                                  (err store "ValueError" "step size 0"))
                                                              (err store "TypeError" "range takes exact params"))]
                                                    [else (err store "TypeError" "cannot call range with non int arguments")])]
                                     [else (err store "TypeError" "cannot call range with non int arguments")])]
                         [else (err store "TypeError" "cannot call range with non int arguments")]))]
    [CSlice (l low up st)
            (interp-as env store ([(lst s) l] [(lower s2) low] [(upper s3) up] [(step s4) st])
                       (slice lst lower upper step s4))]
    [CIndex (l i) (interp-as env store ([(lst s) l] [(idex s2) i])
                             (index lst idex s2))]
    [CDelete (target)
             (type-case CExp target
               [CIndex (l i) (interp-as env store ([(lst s) l] [(idex s2) i])
                                        (type-case CVal lst
                                          [VDict (elts) (begin (hash-remove! elts idex) (ValA lst store))]
                                          ;todo - lists!!!
                                          [else (err store "TypeError" "only dicts are subscriptable")]))]
               [else (err store "TypeError" "delete only implemented for indexes")])]
    
    [CReturn (val) (interp-as env store ([(v s) val])
                              (ValA (VReturn v) s))]
    
    [CObject (fields) (iter-hash (get-list fields) empty env store VObject)]
    [CDict (fields) (iter-hash (get-list fields) empty env store VDict)]
    [CTryFinally (body finally) 
                 (let ((try-val (interp-full body env store))
                       (fin-val (interp-full finally env store)))
                   (type-case Ans fin-val
                     [ValA (f-v s) 
                           (type-case CVal f-v
                             [VReturn (some-val) fin-val]
                             [else try-val])]
                     [ExnA (vf s) try-val]))]
    
    [CTryExcept (body handlers elsebody) 
                (let ((vbody (interp-full body env store)))
                  (type-case Ans vbody
                    [ValA (v s) (interp-full elsebody env s)]
                    [ExnA (exnobj s) 
                          (type-case CVal exnobj
                            [VObject (fields)
                                     (type-case (optionof CVal) (hash-ref fields (VStr "__type__"))
                                       [some (should-be-exn) 
                                             (if (equal? should-be-exn (VStr "exception"))
                                                 (let ([handler-opt (findhandler exnobj handlers (none) env s)])
                                                   (begin ;(display handler-opt)
                                                   (type-case (optionof CExp) handler-opt
                                                     [some (handler)
                                                           (type-case CExp handler
                                                             [CExceptHandler (body type name)
                                                                             (local ((define-values (ne ns)
                                                                                       (update-env-store '__the-exn__ exnobj env s 'local)))
                                                                               (let ([handled (type-case (optionof symbol) name
                                                                                                [some (name-act) 
                                                                                                      (local ((define-values (n-ne n-ns) 
                                                                                                                (update-env-store name-act exnobj ne ns 'local)))
                                                                                                        (interp-full handler n-ne n-ns))]
                                                                                                [none () (interp-full handler ne ns)])])
                                                                                 (type-case Ans handled
                                                                                   [ValA (v s) handled]
                                                                                   [ExnA (v s) (if (and (exn-is v "RuntimeError") (exn-says v "No active exception"))
                                                                                                   (ExnA exnobj ns)
                                                                                                   handled)])))]
                                                             [else (error 'interp "findhandler returned non CExceptHandler, whaa?")])]
                                                     [none () (interp-full elsebody env s)])))
                                                 (error 'interp "caught non exception object"))]
                                       [none () (error 'interp "object caught does not have __type__ field")])]
                            [else vbody])]))]
                                                                          
    
    [CExceptHandler (body type name) (interp-full body env store)]
                
    [CSet (fields) (iter-hash (get-list fields) empty env store VSet)]
    [CGet (obj field)
          (begin 
            (interp-as env store ([(o s) obj] [(f s2) field])
                       (type-case CVal o
                         [VObject (fields)
                                  (type-case (optionof CVal) (hash-ref fields f)
                                    [some (v) (ValA v s2)]
                                    [none () (err s2 "AttributeError" "object lookup failed: " (pretty f))])]
                         [VDict (elts)
                                (type-case CVal f ;only for dict method (band aid)
                                  [VStr (s) (case (string->symbol s)
                                              [(items clear values keys) (ValA (VClosure env (list '-the-dict) (hash empty) (none) (none)
                                                                                         (CReturn (CPrim1 (string->symbol s) (CId '-the-dict)))) s2)]
                                              
                                              ['update (ValA (VClosure env (list '-the-dict) (hash empty) (some '-args) (none)
                                                                       (CIf (Compare '> (CApp (CId 'prim-len) (list (CId '-args)) (hash empty) (none) (none)) (CNum 0))
                                                                            (CIf (Compare '== (CApp (CId 'prim-len) (list (CId '-args)) (hash empty) (none) (none)) (CNum 1))
                                                                                 (CReturn (CBinOp (string->symbol s) (CId '-the-dict) (CIndex (CId '-args) (CNum 0))))
                                                                                 (CError (CStr "update takes no more than one arg!")))
                                                                            (CReturn (CId '-the-dict)))) s2)]
                                              ['__getitem__ (ValA (VClosure env (list '-the-dict 'the-item) (hash empty) (none) (none)
                                                                            (CReturn (CIndex (CId '-the-dict) (CId 'the-item)))) s2)]
                                              ['get (ValA (VClosure env (list '-the-dict) (hash empty) (some '-args) (none)
                                                                    (CLet '-len- 'local (CApp (CId 'prim-len) (list (CId '-args)) (hash empty) (none) (none))
                                                                          (CIf (Compare '> (CId '-len-) (CNum 0))
                                                                               (CIf (Compare 'in (CIndex (CId '-args) (CNum 0)) (CId '-the-dict))
                                                                                    (CReturn (CIndex (CId '-the-dict) (CIndex (CId '-args) (CNum 0))))
                                                                                    (CIf (Compare '== (CId '-len-) (CNum 2))
                                                                                         (CReturn (CIndex (CId '-args) (CNum 1)))
                                                                                         (CIf (Compare '== (CId '-len-) (CNum 1))
                                                                                              (CReturn (CNone))
                                                                                              (CError (Cmake-exn "TypeError" "get takes no more than 2 args")))))
                                                                               (CError (Cmake-exn "TypeError" "get takes more than one arg"))))) s2)]
                                                                                    
                                              [else (err s2 "AttributeError" "dict has not method " s)])]
                                  [else (err s2 "KeyError" "dict lookup")])]
                         [else (err s2 "AttributeError" (pretty o) " is has no attributes, failed at lookup")])))]
                         
    [CSetAttr (obj field val)
              (interp-as env store ([(o s) obj] [(f s2) field] [(v s3) val])
                         (type-case CVal o
                           [VObject (fields)
                                    (type-case CVal f
                                      [VStr (s) (begin (hash-set! fields f v) (ValA (VNone) s3))]
                                      [else (err s3 "AttributeError" "cannot reference object with " (pretty f))])]
                           [VDict (elts) (begin (hash-set! elts f v) (ValA o s3))]
                           [else (err s3 "AttributeError" "cannot access field of " (pretty o))]))]
    [CApp (fun args keys app-star app-kwarg)
            (interp-as env store ([(clos s) fun] [(star-arg s2) (if (some? app-star) (some-v app-star) (CNone))])
                       (type-case CVal clos
                         [VClosure (clo-env ids defaults star kwarg body)
                                   (Apply args keys env s clos star-arg)]
                         [VObject (fields)
                                  (type-case (optionof CVal) (hash-ref fields (VStr "__call__"))
                                    [some (v) (type-case CVal v
                                                [VClosure (clo-env ids defaults star kwarg body)
                                                          (let ((newargs (type-case (optionof CVal) (hash-ref fields (VStr "__class__"))
                                                                           [some (st) (type-case CVal st
                                                                                        [VStr (class) (if (string=? class "class") args (cons fun args))]
                                                                                        [else (cons fun args)])]
                                                                           [none () (cons fun args)])))
                                                            (Apply newargs keys env s v star-arg))]                                                       
                                                [else (err s "TypeError" "cannot call the object")])]
                                    [none () (err s "TypeError" "object does not have a __call__ attr")])]
                         
                         [else (err s "TypeError" "Not a callable at application: " (pretty clos))]))]
;;INVARIANT: fun + arg must be ids
    [CPartialApply (fun arg)
                   (interp-as env store ([(clos s) fun] [(a s2) arg])
                              (type-case CVal clos;;we know that clos and arg is going to be an id, so interping is fine
                                [VClosure (clo-env ids defaults star kwarg body)
                                          (local ((define-values (enew snew) (update-env-store (first ids) a clo-env s 'local)))
                                            (ValA (VClosure enew (rest ids) defaults star kwarg body) snew))]
                                [else (err s2 "TypeError" "partial application of nonfunction")]))]
    
    ;;iterate through default arguments
    [CFunc (args defaults star kwarg body)
           (local ((define (iter cdefs vdefs sto)
                     (cond [(empty? cdefs)
                            (ValA (VClosure (reset-scopes env) args (hash vdefs) star kwarg body) sto)]
                           [else (interp-as env sto ([(v s) (some-v (hash-ref defaults (first cdefs)))])
                                            (iter (rest cdefs) (cons (values (first cdefs) v) vdefs) s))])))
             (iter (hash-keys defaults) empty store))]
    
    [CBinOp (op left right) 
            (interp-as env store ([(real-left s) left] [(real-right s2) right])
                       (let ((l (type-case CVal real-left [VFalse () (VNum 0)] [VTrue () (VNum 1)] [else real-left]))
                             (r (type-case CVal real-right [VFalse () (VNum 0)] [VTrue () (VNum 1)] [else real-right])))
                       (cond [(and (VNum? l) (VNum? r))
                              (let ((nl (VNum-n l)) (nr (VNum-n r)))
                                (case op
                                  [(/ // %) (if (zero? nr) (interp-full (CError (CApp (CId 'ZeroDivisionError) (list (CStr "divided by zero yo")) (hash empty) (none) (none))) env s2)
                                              (ValA (VNum ((case op ['% remainder] ['/ /] ['// (lambda (x y) (floor (/ x y)))]) nl nr)) s2))]
                                  [else (ValA (VNum ((case op ['+ +] ['- -] ['* *] ['** expt]) nl nr)) s2)]))]
                             [(and (VStr? l) (VStr? r))
                              (case op
                                ['+ (ValA (VStr (s+ (VStr-s l) (VStr-s r))) s2)]
                                [else (err s2 "TypeError" "invalid operation on strings: " (symbol->string op))])]
                             [(and (VList? l) (VList? r) (eq? (VList-mutable r) (VList-mutable l)))
                              (case op
                                ['+ (ValA (VList (VList-mutable l) (append (VList-elts l) (VList-elts r))) s2)])]
                             [(and (VList? l) (VNum? r))
                              (case op['* (ValA (VList (VList-mutable l) (append-n (VList-elts l) (VNum-n r))) s2)])]
                             [(and (VList? r) (VNum? l))
                              (case op ['* (ValA (VList (VList-mutable r) (append-n (VList-elts r) (VNum-n l))) s2)])]
                             [(and (VSet? r) (VSet? l)) (set-op op l r s2)]
                             [(and (VDict? r) (VDict? l)) (dict-op op l r s2)]
                             [(and (VStr? l) (VNum? r))
                              (case op ['* (ValA (VStr (str-mult (VStr-s l) (VNum-n r))) s2)])]
                             [(and (VStr? r) (VNum? l))
                              (case op ['* (ValA (VStr (str-mult (VStr-s r) (VNum-n l))) s2)])]
                              
                             [else 
                              (if (and (symbol=? op 'isinstance) (VObject? r))
                                  (type-case CVal r
                                    [VObject (fields)
                                             (type-case (optionof CVal) (hash-ref fields (VStr "__name__"))
                                               [some (name)
                                                     (type-case CVal name
                                                       [VStr (namestr) (cond [(and (or (string=? namestr "bool") (string=? namestr "int")) (or (VTrue? real-left) (VFalse? real-left))) (ValA (VTrue) s2)]
                                                                             [(VObject? real-left)
                                                                              (type-case (optionof CVal) (hash-ref (VObject-fields real-left) (VStr "__class__"))
                                                                                [some (class) 
                                                                                      (type-case CVal class
                                                                                        [VStr (classtr) (ValA (VBool (string=? classtr namestr)) s2)]
                                                                                        [else (ValA (VFalse) s2)])]
                                                                                [none () (ValA (VFalse) s2)])]
                                                                             [else (ValA (VFalse) s2)])]
                                                       [else (ValA (VFalse) s2)])]
                                               [none () (ValA (VFalse) s2)])]
                                    [else (err s2 "TypeError" "invalid operation: " (pretty l) " " (symbol->string op) " " (pretty r) )])
                                  (err s2 "TypeError" "invalid operation: " (pretty l) " " (symbol->string op) " " (pretty r) ))])))]
    
    [CPrim1 (prim arg)
            (begin ;(display env) (display "\n") (display store) (display "\n")
            (interp-as env store ([(v s) arg])
                       (if (and (symbol=? prim 'list)
                                (VFilter? v))
                           (interp-full (VFilter-expr v) (VFilter-env  v) s)
                           (python-prim1 prim v env s))))]
    [CFilter (e) (ValA (VFilter e env) store)];suspend
    [Compare (op left right)
             (interp-as env store ([(l s) left] [(r s2) right])
                        (case op
                          [(is isnot) (ValA (VBool ((case op ['is identity] ['isnot not])
                                                    ((type-case CVal l
                                                      [VList (m l) eq?]
                                                      [VObject (f) eq?]
                                                      [else equal?]) l r))) s2)]
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
                                        [VStr (s) 
                                              (type-case CVal l
                                                [VStr(left) (ValA (VBool ((case op ['in identity] ['notin not]) (str-in s left))) s2)]
                                                [else (err s2 "TypeError" "cannot find nonstring in string")])]
                                        [else (err s2 "TypeError" "in not implemented for this type")])]
                          
                          [else (err s2 "TypeError" "comparator not implemented: " (symbol->string op))]))])))

(define (interp expr) : CVal
  (begin ;(display expr)
    (set! globals (hash empty))
    (type-case Ans (interp-full expr (Ev (hash empty) (hash empty)) (hash empty))
      [ValA (v s) v]
      [ExnA (v s) (type-case CVal v
                    [VObject (elts) (type-case (optionof CVal) (hash-ref elts (VStr "__exceptiontype__"))
                                      [some (t) (type-case (optionof CVal) (hash-ref elts (VStr "__errexp__"))
                                                      [some (errmessage) (begin (error 'interp (string-append (string-append (pretty t) ": ") (pretty errmessage))) v)]
                                                      [none () (error 'interp  "exception must have value __errexp__")])]
                                      [none () (error 'interp "exception must have value __exceptiontype__")])]
                    [VStr (s) (begin (error 'interp-internal s) v)]
                    [else (error 'interp (string-append "exceptions must extend the base type exception: " (pretty v)))])])))

(define (exn-is (exn : CVal) (type : string)) : boolean
  (type-case CVal exn
    [VObject (fields) 
            (type-case (optionof CVal) (hash-ref fields (VStr "__exceptiontype__"))
              [some (t) (equal? (VStr-s t) type)]
              [none () false])]
    [else false]))

(define (exn-says (exn : CVal) (s : string)) : boolean
  (type-case CVal exn
    [VObject (fields) 
            (type-case (optionof CVal) (hash-ref fields (VStr "__errexp__"))
              [some (t) (str-in (VStr-s t) s)]
              [none () false])]
    [else false]))