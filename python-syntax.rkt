#lang plai-typed

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
  
  [PyCompare (left : PyExpr) (ops : (listof symbol)) (right : (listof PyExpr))]
  
  
  [PyUnary (op : symbol) (expr : PyExpr)];+,-,~,not
  
  [PyList (elts : (listof PyExpr))]
  
  [PyId (x : symbol)]
  [PyGlobal (x : symbol)]
  [PyNonLocal (x : symbol)]
  
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]
  
  [PyClassDef (name : symbol) (base : symbol) (body : (listof PyExpr))]
  [PyFunDef (name : symbol) (args : (listof symbol)) (defaults : (listof PyDefault)) (body : PyExpr)]
  [PyFun (args : (listof symbol)) (defaults : (listof PyDefault))  (body : PyExpr)]
  
  [PyReturn (val : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyPass]
  [PyBinOp (of : symbol) (left : PyExpr) (right : PyExpr)]
  
  [PyNotImplemented]
  
  [PyRaise (e1 : PyExpr)])
