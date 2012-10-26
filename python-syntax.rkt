#lang plai-typed


(define-type PyArg
  [Arg (id : symbol)] ;; id
  [Default (id : symbol) (expr : PyExpr)] ;;id=expr
  [Kwarg (id : symbol)] ;;**id
  [VarArg (id : symbol)]);;*id

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyStr (s : string)]
  
  [PyTrue]
  [PyFalse]
  [PyOr (es : (listof PyExpr))]
  [PyAnd (es : (listof PyExpr))]
  
  [PyUnary (op : symbol) (expr : PyExpr)];+,-,~,not
  
  [PyList (elts : (listof PyExpr))]
  
  [PyId (x : symbol)]
  
  [PyFunDef (name : symbol) (args : (listof PyArg )) (body : PyExpr)]
  [PyReturn (val : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyPass]
  [PyBinOp (of : symbol) (left : PyExpr) (right : PyExpr)]
  
  [PyNotImplemented]
  
  [PyRaise (e1 : PyExpr)])
  

