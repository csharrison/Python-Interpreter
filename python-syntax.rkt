#lang plai-typed

(define Some some)
(define None none)
(define Hash hash)
(define (values2 x y) (values x y))
(define (values4 x y z p) (values x y z p))

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyStr (s : string)]
  [PyNone]
  [PyTrue]
  [PyFalse]
  [PyOr (es : (listof PyExpr))]
  [PyAnd (es : (listof PyExpr))]
  
  [PyAugAssign (target : PyExpr) (op : symbol) (value : PyExpr)]
  [PyGen (elt : PyExpr) (target : PyExpr) (iter : PyExpr)]
  
  [PyCompare (left : PyExpr) (ops : (listof symbol)) (right : (listof PyExpr))]
  
  
  [PyUnary (op : symbol) (expr : PyExpr)];+,-,~,not
  
  [PyList (elts : (listof PyExpr))]
  [PyTuple (elts : (listof PyExpr))]
  [PySlice (lst : PyExpr) (lower : PyExpr) (upper : PyExpr) (step : PyExpr)]
  [PyIndex (lst : PyExpr) (i : PyExpr)]
  
  [PyId (x : symbol)]
  [PyGlobal (x : symbol)]
  [PyNonLocal (x : symbol)]
  
  [PyDict (keys : (listof PyExpr)) (vals : (listof PyExpr))]
  [PySet (elts : (listof PyExpr))]
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]
  
  [PyClassDef (name : symbol) (base : symbol) (body : (listof PyExpr))]
  [PyGetAttr (target : PyExpr) (attr : PyExpr)]
  [PyDelete (target : PyExpr)]
  [PyFunDef (name : symbol) (args : (listof symbol)) (defaults : (hashof symbol PyExpr)) (starargs : (optionof symbol)) (kwargs : (optionof symbol)) (body : PyExpr)]
  [PyClassMethod (name : symbol) (args : (listof symbol)) (defaults : (hashof symbol PyExpr)) (starargs : (optionof symbol)) (kwargs : (optionof symbol)) (body : PyExpr)]
  [PyFun (args : (listof symbol)) (defaults : (hashof symbol PyExpr)) (starargs : (optionof symbol)) (kwargs : (optionof symbol)) (body : PyExpr)]

  [PyReturn (val : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr)) (keys : (hashof symbol PyExpr)) (star : (optionof PyExpr)) (kwarg : (optionof PyExpr))]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyPass]
  [PyBinOp (op : symbol) (left : PyExpr) (right : PyExpr)]
 
  [PyTryFinally (body : PyExpr) (finallyBody : PyExpr)]
  [PyTryExcept (body : PyExpr) (handlers : (listof PyExpr)) (else : PyExpr)]
  [PyExceptHandler (body : PyExpr) (type : (optionof PyExpr)) (name : (optionof symbol))]
  [PyLocals]
  [PyNotImplemented]
  [PyRaise (e1 : PyExpr)])
