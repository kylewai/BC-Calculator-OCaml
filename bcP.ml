open Core
type sExpr = 
    | Atom of string
    | List of sExpr list

type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list

type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list
    | Continue of string
    | Break of string


type block = statement list 

type env = (string, float) Hashtbl.t (* complete *)

type envQueue = env list

type returnType =
    |Normal of envQueue
    |Return of expr*envQueue
    |Break of envQueue
    |Continue of envQueue


type func = (string, block) Hashtbl.t

type funcSigs = (string, string list) Hashtbl.t

let funcQueue : func = Hashtbl.create 10;;

let funcParams : funcSigs = Hashtbl.create 10;;

let varEval (_v: string) (_q:envQueue): float  =   
    match _q with
        | h::t -> if(Hashtbl.mem h _v) then Hashtbl.find h _v else 0.0




let testEnv : env = Hashtbl.create 10;;
Hashtbl.replace testEnv "x" 36.0;;
let scopes : envQueue = testEnv :: [];;


print_endline "varEval-------------------------";;

print_float (varEval "x" scopes);;
print_endline "";;

Hashtbl.replace testEnv "x" 24.0;;
let scopes : envQueue = testEnv :: [];;

print_float (varEval "x" scopes);;
print_endline "";;





let rec evalExpr (_e: expr) (_q:envQueue): float  = 
    begin match _e with
        |Num(_f) -> _f
        |Var(_s) -> varEval _s _q
        |Op1(_s, _ex) -> 
            if(compare _s "++a" = 0) then
                begin match _q with
                |h::t -> 
                    let newEx = (evalExpr _ex _q) +. 1.0 in
                    begin match _ex with
                        |Var(_s) -> Hashtbl.replace h _s newEx; newEx
                        (*|err*)
                    end
                end
            else if(compare _s "--a" = 0) then
                begin match _q with
                |h::t -> 
                    let newEx = (evalExpr _ex _q) -. 1.0 in
                    begin match _ex with
                        |Var(_s) -> Hashtbl.replace h _s newEx; newEx
                        (*|err*)
                    end
                end
            else 0.0
                            
        |Op2(_s, _exl, _exr) -> 
            if(compare _s "+" = 0) 
            then (evalExpr _exl _q) +. (evalExpr _exr _q)
            else if(compare _s "-" = 0)
            then (evalExpr _exl _q) -. (evalExpr _exr _q)
            else if(compare _s "*" = 0)
            then (evalExpr _exl _q) *. (evalExpr _exr _q)
            else if(compare _s "/" = 0)
            then (evalExpr _exl _q) /. (evalExpr _exr _q)
            else if(compare _s "^" = 0)
            then (evalExpr _exl _q) ** (evalExpr _exr _q)
            else if(compare _s "<" = 0)
            then if((evalExpr _exl _q) < (evalExpr _exr _q)) then 1.0 else 0.0
            else if(compare _s ">" = 0)
            then if((evalExpr _exl _q) > (evalExpr _exr _q)) then 1.0 else 0.0
            else if(compare _s "<=" = 0)
            then if((evalExpr _exl _q) <= (evalExpr _exr _q)) then 1.0 else 0.0
            else if(compare _s ">=" = 0)
            then if((evalExpr _exl _q) >= (evalExpr _exr _q)) then 1.0 else 0.0
            else if(compare _s "==" = 0)
            then if((evalExpr _exl _q) = (evalExpr _exr _q)) then 1.0 else 0.0
            else if(compare _s "!=" = 0)
            then if((evalExpr _exl _q) <> (evalExpr _exr _q)) then 1.0 else 0.0
            else 0.0

                                        
        |Fct(_s, _exList) -> let newScope : (string, float) Hashtbl.t = Hashtbl.create 10 in
                            let paramNames = Hashtbl.find funcParams _s in
                            let params = getParams _exList [] _q in
                            let newQ = addParams paramNames params [newScope]@_q in
                            let funcBod = Hashtbl.find funcQueue _s in
                            let retVal = evalCode funcBod newQ in
                            begin match retVal with
                                |Normal(q) -> 0.0
                                |Return(_ex, q) -> 
                                    let newExp = evalExpr _ex q in
                                    newExp;
                            end
    end

and getParams (params: expr list) (res: float list) (q: envQueue): float list =
    begin match params with
        |[] -> res
        |h::t -> 
            begin match res with
                |[] -> 
                    let newVal = evalExpr h q in
                    getParams t [newVal] q;
                |h1::t1 ->
                    let newVal = evalExpr h q in 
                    let newList = h1::(t1@[newVal]) in
                    getParams t newList q;
            end
    end

    

and addParams (paramNames : string list) (params: float list) (_q: envQueue): envQueue = 
    begin match paramNames with
    |paramName::t -> 
        begin match params with
        |param::t2 -> 
            begin match _q with
            |h3::t3 -> 
                Hashtbl.replace h3 paramName param;
                addParams t t2 _q; 
                _q
            end
        end
    |[] -> _q
    end


(*let x = Op1("++a", Var("x"));;
print_float (evalExpr x scopes);;
let y = Op2("-", Var("x"), Var("x"));;
print_float (evalExpr y scopes)*)

and whileFunc (code: block) (_ex: expr) (q: envQueue) : returnType = 
    let cond = evalExpr _ex q in
        if(cond > 0.0) then
            let retVal = evalCode code q in
            begin match retVal with
                |Normal(_q) -> whileFunc code _ex _q
                |Return(_ex, _q) -> Return(_ex, _q)
                |Break(_q) -> Normal(_q)
            end
        else
            Normal(q)

and forFunc (_ex: expr) (_st2: statement) (code: block) (q: envQueue) : returnType =
    let cond = evalExpr _ex q in
        if(cond > 0.0) then
            let retVal = evalCode code q in
            begin match retVal with
                |Normal(_q) -> 
                    evalStatement (Normal(_q)) _st2;
                    forFunc _ex _st2 code _q;
                |Return(_ex, _q) -> Return(_ex, _q)
                |Break(_q) -> Normal(_q)
                |Continue(_q) ->
                    evalStatement (Normal(_q)) _st2; 
                    forFunc _ex _st2 code _q;
            end
        else
            Normal(q)


and evalStatement (retType: returnType) (s: statement): returnType =
    begin match retType with
        |Return(_ex, q) -> Return(_ex, q)
        |Break(q) -> Break(q)
        |Continue(q) -> Continue(q)
        |Normal(q) ->
            begin match s with 
                | Assign(_v, _e) -> let newExpr = evalExpr _e q
                                    in begin match q with
                                        |h::t -> Hashtbl.replace h _v newExpr; Normal(q)
                                        end
                | Expr(_ex) -> print_float (evalExpr _ex q); print_endline ""; Normal(q)
                | If(e, codeT, codeF) -> 
                    let cond = evalExpr e q in
                        if(cond>0.0) then
                            evalCode codeT q
                        else
                            evalCode codeF q
                    ;
                | While(_ex, code) -> whileFunc code _ex q
                | For(_st, _ex, _st2, code) -> 
                    evalStatement (Normal(q)) _st;
                    forFunc _ex _st2 code q;
                | FctDef(_s, _sl, code) ->
                    Hashtbl.replace funcParams _s _sl;
                    Hashtbl.replace funcQueue _s code;
                    Normal(q)
                | Return(_ex) -> Return(_ex, q)
                | Break(_s) -> Break(q)
                | Continue(_s) -> Continue(q)
                | _ -> Normal(q) (*ignore *)
            end
    end

and evalCode (_code: block) (_q:envQueue): returnType = 
    (* crate new environment *)
    (* user fold_left  *)
    (* pop the local environment *)
    begin match _q with
        |h::t-> List.fold_left evalStatement (Normal(_q)) _code;
        |[] -> 
            let initQ = [Hashtbl.create 10]@_q in
            List.fold_left evalStatement (Normal(initQ)) _code;
    end

;;

    print_endline "evalNum-------------------------";;
    print_float (evalExpr (Num(5.0)) []);;
    print_endline "";;
    
    print_endline "evalVar-------------------------";;
    let testEnv : env = Hashtbl.create 10;;
    Hashtbl.replace testEnv "y" 45.0;;
    let scopes : envQueue = testEnv::[];;
    print_float (evalExpr ((Var("y"))) scopes);;
    print_endline "";;
    
    print_endline "evalOp1-------------------------";;
    print_float(evalExpr (Op1("++a", Var("y"))) scopes);;
    print_endline "";;
    print_float(evalExpr (Var("y")) scopes);;
    print_endline "";;
    
    print_endline "evalOp2-------------------------";;
    print_float(evalExpr (Op2("+", Var("y"), Var("y"))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("-", Var("y"), Num(20.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("*", Num(60.0), Num(20.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("/", Num(92.0), Var("y"))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("^", Num(8.0), Num(2.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("<", Num(8.0), Num(2.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("<", Num(2.0), Num(8.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2(">", Var("y"), Num(22.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2(">", Num(22.0),Var("y"))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("<=", Num(8.0), Num(8.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("<=", Num(2.0), Num(8.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2(">=", Var("y"), Num(22.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2(">=", Var("y"),Var("y"))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("==", Var("y"), Num(46.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("==", Var("y"),Num(45.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("!=", Var("y"), Num(46.0))) scopes);;
    print_endline "";;
    print_float(evalExpr (Op2("!=", Var("y"),Num(45.0))) scopes);;
    print_endline "";;

    print_endline "evalAssign-------------------------";;
    Hashtbl.replace testEnv "y" 3.0;;
    Hashtbl.replace testEnv "x" 1.0;;
    let scopes : envQueue = testEnv::[];;
    print_float (evalExpr (Var("x")) scopes);;
    print_endline "";;
    evalStatement (Normal(scopes)) (Assign("x", Op2("-", Var("y"), Num(1.0))));;
    print_float (evalExpr (Var("x")) scopes);;
    print_endline "";;
    
    print_endline "If Statements:-------------------------";;
    evalStatement (Normal(scopes)) (If(Op2(">", Var("x"), Num(1.0)), [Expr(Num(1.0))], [Expr(Num(0.0))]));; 
    evalStatement (Normal(scopes)) (If(Num(0.0), [Expr(Num(1.0))], [Expr(Num(0.0))]));; 
    
    print_endline "While statements:-------------------------";;
    evalStatement (Normal(scopes)) (While(Op2("<", Var("y"), Num(10.0)), [Expr(Op1("++a", Var("y")))]));;
    evalStatement (Normal(scopes)) (Assign("y", Num(3.0)));;
    evalStatement (Normal(scopes)) (While(Op2("<", Var("y"), Num(10.0)), [Expr(Op1("++a", Var("y"))); Break("break")]));;

    print_endline "For statements:-------------------------";;
    evalStatement (Normal(scopes)) (For(
        Assign("i", Num(2.0)),
        Op2("<", Var("i"), Num(10.0)),
        Expr(Op1("++a", Var("i"))),
        [
            Assign("v", Op2("+", Var("v"), Var("i")))
        ]
    ));;
    print_float (evalExpr (Var("v")) scopes);;
    print_endline "";;


(* 
    v = 1; 
    v // display v
 *)

print_endline "p1-------------------------";;

let p1: block = [Assign("v", Num(1.0)); Expr(Var("v"))];;

(evalCode p1 []);;



(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)

print_endline "p2-------------------------";;
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
];;

(evalCode p2 []);;

(*  Fibbonaci sequence
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
 *)
 print_endline "p3-------------------------";;
let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<=", Var("x"), Num(1.0)),
                [Return(Var("x"))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ];;

evalCode p3 [];;



(*
    define g(s) {
        for(i = s; i < 10; i++){
            5;
            if(i == 4) then 
                return 1
            else 
                continue
            6;
        }
        return 0;
    }
    g(3);
    g(5);
*)
print_endline "p4-------------------------";;
let p4: block = 
    [
        FctDef("g", ["s"], [
            For(
                Assign("i", Var("s")),
                Op2("<", Var("i"), Num(10.0)),
                Expr(Op1("++a", Var("i"))),
                [
                    Expr(Num(5.0));
                    If(Op2("==", Var("i"), Num(4.0)), [Return(Num(1.0))], [Continue("continue")]);
                    Expr(Num(6.0));

                ]
            );
            Return(Num(0.0))
        ]);

        Expr(Fct("g", [Num(3.0)]));
        Expr(Fct("g", [Num(5.0)]));
    ]
    ;;

evalCode p4 [];;



(*
    
    define fac(x){
        if(x == 0) then
            return 1
        else 
            return x * fac(x - 1)
    }
    fac(6);

*)
print_endline "p5-------------------------";;
let p5: block = 
    
    [
        FctDef("fac", ["x"], [
            If(Op2("==", Var("x"), Num(0.0)), 
                [Return(Num(1.0))], 
                [Return(Op2("*", Var("x"), Fct("fac", [Op2("-", Var("x"), Num(1.0))])))]);
        ]);
        Expr(Fct("fac", [Num(6.0)]));

    ]
;;
evalCode p5 [];;


(*
    define fac(x){
        if(x == 0) then 
            return 1
        else 
            return x * fac(x - 1)
    }
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }
    p = 66;
    while(p > 0){
        --p
        if(p < 10){
            fac(p);
            f(p);
        }
    }
*)

print_endline "p6-------------------------";;
let p6: block =
    [
        FctDef("fac", ["x"], [
            If(Op2("==", Var("x"), Num(0.0)), 
                [Return(Num(1.0))], 
                [Return(Op2("*", Var("x"), Fct("fac", [Op2("-", Var("x"), Num(1.0))])))]);
        ]);
        FctDef("f", ["x"], [
            If(
                Op2("<=", Var("x"), Num(1.0)),
                [Return(Var("x"))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        Assign("p", Num(66.0));
        While(Op2(">", Var("p"), Num(0.0)), 
            [
                Expr(Op1("--a", Var("p")));
                If(Op2("<", Var("p"), Num(10.0)), 
                   [
                       Expr(Fct("fac", [Var("p")]));
                       Expr(Fct("f", [Var("p")]));
                   ],
                   [] 
                )
            ]
        
        )
    ]
;;

evalCode p6 [];;