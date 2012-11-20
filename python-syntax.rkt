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
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]
  
  [PyClassDef (name : symbol) (base : symbol) (body : (listof PyExpr))]
  [PyGetAttr (target : PyExpr) (attr : PyExpr)]
  
  [PyFunDef (name : symbol) (args : (listof symbol)) (defaults : (hashof symbol PyExpr)) (starargs : (optionof symbol)) (kwargs : (optionof symbol)) (body : PyExpr)]
  [PyFun (args : (listof symbol)) (defaults : (hashof symbol PyExpr)) (starargs : (optionof symbol)) (kwargs : (optionof symbol)) (body : PyExpr)]
  
  [PyReturn (val : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr)) (keys : (hashof symbol PyExpr)) (star : (optionof PyExpr)) (kwarg : (optionof PyExpr))]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyPass]
  [PyBinOp (op : symbol) (left : PyExpr) (right : PyExpr)]
  
  [PyNotImplemented]
  
  [PyRaise (e1 : PyExpr)])
