(*環境：変数名とロケーションのリストを拡張する関数*)
let ext_envs env x v = (x,v) :: env
                     
(*ストア：ロケーションと値のリストを拡張する関数*)
let ext_sores store x v = (x,v) :: store　

(*第一引数に変数名、第２引数に環境を指定し、環境の中に指定した変数名があれば、
その変数のロケーションを返す関数*)
let rec lookup_envs x env =
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v) :: tl -> if x = y then v
                   else lookup x tl

(*第一引数にロケーション、第２引数にストアを指定し、ストアの中に指定したロケーションがあれば、
そのロケーションの値を返す関数*)
let rec lookup_stores x store =
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v) :: tl -> if x = y then v
                   else lookup x tl

let rec eval_exp env store = function
  | Const(n) -> IntVal(n)
  | EVar(x) -> lookup_stores(lookup_envs(x,env),stores)
  | Nill -> IntVal(0)
  | Op(e1,

let


  
                          
