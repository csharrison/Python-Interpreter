#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

sequence types:
strings
lists
tuples
ranges

containers: dict, (set)

|#

(define-type CDefault
  [CD (id : symbol) (expr : CExp)])

(define-type CExp
  [CNum (n : number)]
  [CStr (s : string)]
  [CTrue]

  [CFalse]
  [CNone]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CList (mutable : boolean) (elts : (listof CExp))]
  [CSlice (lst : CExp) (lower : CExp) (upper : CExp) (step : CExp)]
  [CIndex (lst : CExp) (i : CExp)]
  
  [CError (e1 : CExp)]
  
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CUnary (op : symbol) (expr : CExp)];+,-,~,not
  
  [CSet! (id : symbol) (val : CExp) (type : symbol)]
  
  [CId (x : symbol)]
  [CGlobalId (x : symbol)]
  [CNonLocalId (x : symbol)]
  
  [CLet (x : symbol) (type : symbol) (bind : CExp) (body : CExp)]
  
  [CReturn (val : CExp)]
  
  [CApp (fun : CExp) (args : (listof CExp))]
  [CPartialApply (fun : CExp) (arg : CExp)]
  [CFunc (args : (listof symbol)) (defaults : (listof CDefault)) (body : CExp)]
  
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CBinOp (op : symbol) (left : CExp) (right : CExp)];+ - * / // ** << >> bitor bitxor & %
  [Compare (op : symbol) (l : CExp) (r : CExp)]
  [CNotDefined]
  
  ;only difference between objects and classes: classes do not take implicit self, type=class
  [CObject (fields : (hashof CExp CExp))]
  [CDict (fields : (hashof CExp CExp))]
  [CGet (obj : CExp) (field : CExp)]
  [CSetAttr (obj : CExp) (field : CExp) (val : CExp)]
  )
;;objects
; questionable things: notimplemented, ellipses



(define-type VDefault
  [VD (id : symbol) (val : CVal)])
(define-type CVal
  [VList (mutable : boolean) (elts : (listof CVal))];tuple = immutable, list = mutable
  [VNum (n : number)]
  [VStr (s : string)]
  [VTrue]
  [VFalse]
  [VNone]
  [VClosure (env : Env) (args : (listof symbol)) (defaults : (listof VDefault)) (body : CExp)]
  [VReturn (val : CVal)]
  [VObject (fields : (hashof CVal CVal))];mutable hash (MAKE VOBJECTS HAVE HASHES)
  [VDict (fields : (hashof CVal CVal))]
  [VNotDefined])


(define-type Ans
  [ValA (v : CVal) (store : Store)]
  [ExnA (v : CVal) (store : Store)])

(define-type-alias Location number)

(define-type-alias Store (hashof Location CVal))

(define-type Env
  [Ev (locals : (hashof symbol Location)) (nonlocals : (hashof symbol Location))])


