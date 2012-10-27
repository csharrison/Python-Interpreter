#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#


(define (get-structure pyjson)
  (match pyjson
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PySeq (map get-structure expr-list))]
    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structure expr)]

    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structure func-expr)
            (map get-structure args-list))]
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (match id
       ["True" (PyTrue)]
       ["False" (PyFalse)]
       ["None" (PyNone)]
       [_  (PyId (string->symbol id))])]
    [(hash-table ('nodetype "Lambda")
                 ('args args)
                 ('body body))
     (local ((define-values (defaults a) (get-structure args)))
       (PyFun a defaults (get-structure body)))]
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list dec_lst)
                 ('returns returns))
     (local ((define-values (ds a) (get-structure args)))
       (PyFunDef (string->symbol name) 
                 a ds
                 (PySeq (map get-structure body))))]
    [(hash-table ('nodetype "Return")
                 ('value value))
     (PyReturn (get-structure value))]
    [(hash-table ('nodetype "arg")
                 ('arg id)
                 ('annotation ann))
     (string->symbol id)]
    [(hash-table ('nodetype "arguments")
                 ('args args)
                 ('defaults defaults)
                 ('vararg vararg)
                 ('kwarg kwarg)
                 ('kwargannotation kanno)
                 ('varargannotation varanno)
                 ('kw_defaults kw_defaults)
                 ('kwonlyargs kwonlyargs))
     (local ((define defs (map get-structure defaults))
             (define as (map get-structure args))
             (define reg-args (take as (- (length as) (length defs))))
             (define def-args (drop as (- (length as) (length defs)))))
       (values (map (lambda (a v) (Default a v)) def-args defs)
                     reg-args))]
                     ;(if (equal? #\nul vararg) empty (VarArg (string->symbol vararg)))
                     ;(if (equal? #\nul kwarg) empty (Kwarg (string->symbol kwarg)))))]
    [(hash-table ('nodetype "List")
                 ('ctx ctx)
                 ('elts elts))
     ;(PyList (map get-structure elts))]
     (PyNotImplemented)]
    [(hash-table ('nodetype "Compare")
                 ('left left)
                 ('ops ops)
                 ('comparators comparators))
     (PyCompare (get-structure left)
                (map get-structure ops)
                (map get-structure comparators))]
    [(hash-table ('nodetype "Eq")) '==]
    [(hash-table ('nodetype "NotEq")) '!=]
    [(hash-table ('nodetype "Lt")) '<]
    [(hash-table ('nodetype "LtE")) '<=]
    [(hash-table ('nodetype "Gt")) '>]  
    [(hash-table ('nodetype "GtE")) '>=]
    [(hash-table ('nodetype "Is")) 'is]
    [(hash-table ('nodetype "IsNot")) 'isnot]  
    [(hash-table ('nodetype "In")) 'in]
    [(hash-table ('nodetype "NotIn")) 'notin]  
    
    [(hash-table ('nodetype "If")
                 ('test test)
                 ('body body)
                 ('orelse orelse))
     (PyIf (get-structure test) 
           (PySeq (map get-structure body))
           (PySeq (map get-structure orelse)))]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    
    [(hash-table ('nodetype "Raise")
                 ('cause cause)
                 ('exc exc))
     (PyRaise (get-structure exc))]
    
    [(hash-table ('nodetype "BoolOp")
                 ('op op)
                 ('values values))
     ((match op 
        [(hash-table ('nodetype "Or")) PyOr]
        [(hash-table ('nodetype "And")) PyAnd]) (map get-structure values))]
    [(hash-table ('nodetype "UnaryOp")
                 ('op op)
                 ('operand operand))
     (PyUnary (match (hash-ref op 'nodetype)
                ["Not" 'not] ["Invert" '~] ["UAdd" '+] ["USub" '-]) (get-structure operand))]
     
    [(hash-table ('nodetype "BinOp")
                 ('op op)
                 ('left l)
                 ('right r))
     (PyBinOp (get-structure op) (get-structure l) (get-structure r))]
    [(hash-table ('nodetype "Add")) '+]
    [(hash-table ('nodetype "Sub")) '-]
    [(hash-table ('nodetype "Div")) '/]
    [(hash-table ('nodetype "FloorDiv")) '//]
    [(hash-table ('nodetype "Mult")) '*]
    [(hash-table ('nodetype "Pow")) '**]
    [(hash-table ('nodetype "LShift")) '<<]
    [(hash-table ('nodetype "RShift")) '>>]
    [(hash-table ('nodetype "BitOr")) 'bitor]
    [(hash-table ('nodetype "BitXor")) 'bitxor]
    [(hash-table ('nodetype "BitAnd")) '&]
    [(hash-table ('nodetype "Mod")) '%]
    [a (begin (display "\nwhat is this: ") (display a) (display "\n") (PyPass))]
    [_ (error 'parse "Haven't handled a case yet")]))

(define (get-structured-python pyjson)
  (begin ;(display pyjson)
         (get-structure pyjson)))