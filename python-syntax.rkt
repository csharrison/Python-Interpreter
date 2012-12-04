#lang plai-typed

(define (Some n) (some n))
(define (None) (none))

(define-type PyDefault
  [PD (id : symbol) (val : PyExpr)])

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
  
  [PyFunDef (name : symbol) (args : (listof symbol)) (defaults : (listof PyDefault)) (body : PyExpr)]
  [PyFun (args : (listof symbol)) (defaults : (listof PyDefault))  (body : PyExpr)]
  
  [PyReturn (val : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyPass]
  [PyBinOp (op : symbol) (left : PyExpr) (right : PyExpr)]
  
  [PyTryFinally (body : PyExpr) (finallyBody : PyExpr)]
  [PyTryExcept (body : PyExpr) (handlers : (listof PyExpr)) (else : PyExpr)]
  [PyExceptHandler (body : PyExpr) (type : (optionof PyExpr)) (name : (optionof PyExpr))]

  [PyNotImplemented]
  
  [PyRaise (e1 : PyExpr)])
