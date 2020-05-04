type method_id =
  MethId of string

type class_id =
  ClassId of string

type type_id =
  TypeId of string      

type data_type =
  | Int
  | ClassType of class_id

type var =
  Var of string
          
type value =
  | IntVal of Int
  | ObjVal of class_id * (var * location) list
  | LocsVal of int
       
type mod_ops =
  | Plus | Minus | Xor
       
type ops =
  | ModOps of mod_ops | Times | Div | Mod | OpAnd | OpOr | And | Or
  | Greater | Less | Equal | NotEq | LessEq | GreaterEq                 
type exp =
  | Const of int
  | EVar of var
  | Nill
  | Op of exp * ops * exp
       
type statement =
  | Assign of var * mod_ops * exp
  | Swap of var * var
  | If of exp * statement * statement * exp
  | From of exp * statement * statement * exp
  | Construct of class_id * var * statement * var
  | LocalCall of method_id * var list 
  | LocalUncall of method_id * var list
  | Call of var * method_id * var list
  | UnCall of var * method_id * var list
  | Skip
  | Seq of statement * statement


type meth =
  | Method of method_id * (data_type * var) list * statement

type cl =
  | Class of class_id * (data_type * var) list * meth list
  | ClassInherits of class_id * class_id * (data_type * var) list * meth list
           
type prog =
  Cl of cl list

(* p22 サンプルプログラム
Cl [Class(ClassId("Program"),[(Int,Var("result"));(Int,Var("n"))],[Method(MethId("main"),[],Seq(Assign(Var("n"),Xor,Const 4), Construct(ClassId("Fib"),Var("f"),Seq(Seq(Call(Var("f"),MethId("fib"),[Var("n")]),Call(Var("f"),MethId("get"),[Var("n")])),Uncall(Var("f"),MethId("fib"),[Var("n")])),Var("f"))))]);Class(ClassId("Fib"),[(Int,Var("x1"));(Int,Var("x2"))],[Method(MethId("fib"),[(Int,Var("n"))],If(Op(EVar(Var("n")),Equal,Const 0),Seq(Assign(Var("x1"),Xor,Const 1),Assign(Var("x2"),Xor,Const 1)),Seq(Seq(Seq(Assign(Var("n"),Minus,Const 1),Local_call(MethId("fib"),[Var("n")])),Assign(Var("x1"),Plus,EVar(Var("x2")))),Swap(Var("x1"),Var("x2"))),Op(EVar(Var("x1")),Equal,EVar(Var("x2")))));Method(MethId("get"),[(Int,Var("out"))],Assign(Var("out"),Xor,EVar(Var("x2"))))])];;
*)
          
(*----------------------------                             
type var =
  Var of string
type oplus =
  | Plus
  | Minus
  | Caret

  
type otime =
  | OPlus of oplus
  | Time
  | Div
  | Equal
  | Less  (*<*)
  | Greater (*>*)
  | Less_Eq (*<=*)
  | Greater_Eq (*>=*)
  | Not_Eq    (*!=*)

type exp =
  | EConst of int
  | EVar of var
  | EIn of var * exp
  | ETime of exp * otime * exp
  | ETop of var
  | EEmpty of var
            
type step =
  | Plus_Eq of var * oplus * exp
  | Plus_In of var * exp * oplus * exp
  | Push of var * var
  | Pop of var * var
  | Skip 

(*language SRL*)
  
type blk =
  | SStep of step
  | SIf of exp * blk * blk * exp
  | SCon of blk * blk
  | SFrom of exp * blk * blk * exp

type srl = SBLK of blk

(*language RL*)
                
type label = Label of string
                     
type jump =
  | RGoto of label
  | RIf of exp * label * label
  | RExit

type from =
  | RFrom of label
  | RFi of exp * label * label
  | REntry

type rlblk =
  | RBlk of label * from * step list * jump

type rl = RLBLK of rlblk list 
 *)
