open Source
open Types
open Values
open Ast

let transform_phrase (ph: 'a Source.phrase) (f: 'a -> 'b) : ('b Source.phrase) =
    let it' = f ph.it in
    let ph' = it' @@ no_region in
    ph'

let ensure_type (m: Ast.module_) (ftyp: Types.func_type): (Ast.module_ * var) =
    let m' = transform_phrase m (fun m_it -> {m_it with types = m_it.types @ [(ftyp @@ no_region)] }) in
    let type_index = (List.length m'.it.types) - 1 in
    let t_var = (Int32.of_int type_index) @@ no_region in
    (m', t_var)

(* Transforms an export by name *)
let transform_export (m: Ast.module_) (ex_name: Ast.name) (f: Ast.export' -> Ast.export') : Ast.module_ =
    transform_phrase m (fun m_it ->
        { m_it with exports = List.map (fun exp ->
            if exp.it.name = ex_name then
                transform_phrase exp f
            else
                exp
        ) m_it.exports })

let replace_in_list (l: 'a list) (where_: int -> 'a -> bool) (by: 'a -> 'a): 'a list =
    List.mapi (fun i el ->
        if where_ i el
        then by el
        else el
    ) l

let instr_str (i: Ast.instr) =
    Sexpr.to_string 80 (Arrange.instr i)

(*
(* Transforms an export by name *)
let transform_func (m: Ast.module_) (v: Ast.var) (f: Ast.func' -> Ast.func') : Ast.module_ =
    transform_phrase m (fun m_it ->
        { m_it with funcs = replace_in_list m_it.funcs
            (fun i _ -> i = )
            (fun func -> transform_phrase exp f)
        })
*)
(*
let export_desc (m: Ast.module_) (ex_name: Ast.name) : Ast.export_desc =
    let exp = List.find (fun exp -> exp.it.name = ex_name) m.it.exports in
    exp.it.edesc


type nested_instr = N of (Ast.instr * (nested_instr list))

let rec group (is: Ast.instr list): nested_instr * (Ast.instr list) = 
    let h, is' = match is with
    | [] -> rais (Failure "ugh")
    | h::is' -> h,is'
    in
    match h.it with
    | Drop ->
        let tbd, tl = group is' in
        let pre, tl = group tl in
        N (h, [pre; tbd]), tl
    |
    | _ -> N (h, []), is'

*)

let secify_memory (m: Ast.memory) =
    let MemoryType (l, _) = m.it.mtype in
    {m with it = {mtype = MemoryType (l, Secret)}}

let secify_value_type (vt: Types.value_type) =
    match vt with
    | I32Type | S32Type -> S32Type
    | I64Type | S64Type -> S64Type
    | _ -> raise (Failure "Attempted to secify a float")

let secify_testop op =
    match op with
    | I32 IntOp.Eqz -> S32 SecOp.Eqz
    | I64 IntOp.Eqz -> S64 SecOp.Eqz
    | _ -> op

let secify_const (i: Ast.instr) =
    let v = match i.it with
        | Const v -> v
        | _ -> raise (Failure "You called secify_const on an instruction that wasn't const... I mean seriously? It's in the name")
    in
    let it' = match v.it with
        | I32 x | S32 x -> S32 x
        | I64 x | S64 x -> S64 x
        | F32 x -> F32 x
        | F64 x -> F64 x
    in
    Const {v with it = it'}

let secify_unop op =
    match op with
    | I32 IntOp.Clz -> S32 SecOp.Clz
    | I32 IntOp.Ctz -> S32 SecOp.Ctz
    | I32 IntOp.Popcnt -> S32 SecOp.Popcnt
    | I64 IntOp.Clz -> S64 SecOp.Clz
    | I64 IntOp.Ctz -> S64 SecOp.Ctz
    | I64 IntOp.Popcnt -> S64 SecOp.Popcnt
    | _ -> op

let secify_binop op =
    match op with
    | I32 IntOp.Add -> S32 SecOp.Add
    | I32 IntOp.Sub -> S32 SecOp.Sub
    | I32 IntOp.Mul -> S32 SecOp.Mul
    | I32 IntOp.And -> S32 SecOp.And
    | I32 IntOp.Or -> S32 SecOp.Or
    | I32 IntOp.Xor -> S32 SecOp.Xor
    | I32 IntOp.Shl -> S32 SecOp.Shl
    | I32 IntOp.ShrS -> S32 SecOp.ShrS
    | I32 IntOp.ShrU -> S32 SecOp.ShrU
    | I32 IntOp.Rotl -> S32 SecOp.Rotl
    | I32 IntOp.Rotr -> S32 SecOp.Rotr
    | I64 IntOp.Add -> S64 SecOp.Add
    | I64 IntOp.Sub -> S64 SecOp.Sub
    | I64 IntOp.Mul -> S64 SecOp.Mul
    | I64 IntOp.And -> S64 SecOp.And
    | I64 IntOp.Or -> S64 SecOp.Or
    | I64 IntOp.Xor -> S64 SecOp.Xor
    | I64 IntOp.Shl -> S64 SecOp.Shl
    | I64 IntOp.ShrS -> S64 SecOp.ShrS
    | I64 IntOp.ShrU -> S64 SecOp.ShrU
    | I64 IntOp.Rotl -> S64 SecOp.Rotl
    | I64 IntOp.Rotr -> S64 SecOp.Rotr
    | _ -> op

let secify_relop op =
  match op with
    | I32 IntOp.Eq -> S32 SecOp.Eq
    | I32 IntOp.Ne -> S32 SecOp.Ne
    | I32 IntOp.LtS -> S32 SecOp.LtS
    | I32 IntOp.LtU -> S32 SecOp.LtU
    | I32 IntOp.GtS -> S32 SecOp.GtS
    | I32 IntOp.GtU -> S32 SecOp.GtU
    | I32 IntOp.LeS -> S32 SecOp.LeS
    | I32 IntOp.LeU -> S32 SecOp.LeU
    | I32 IntOp.GeS -> S32 SecOp.GeS
    | I32 IntOp.GeU -> S32 SecOp.GeU
    | I64 IntOp.Eq -> S64 SecOp.Eq
    | I64 IntOp.Ne -> S64 SecOp.Ne
    | I64 IntOp.LtS -> S64 SecOp.LtS
    | I64 IntOp.LtU -> S64 SecOp.LtU
    | I64 IntOp.GtS -> S64 SecOp.GtS
    | I64 IntOp.GtU -> S64 SecOp.GtU
    | I64 IntOp.LeS -> S64 SecOp.LeS
    | I64 IntOp.LeU -> S64 SecOp.LeU
    | I64 IntOp.GeS -> S64 SecOp.GeS
    | I64 IntOp.GeU -> S64 SecOp.GeU
    | _ -> op

let secify_cvtop op =
    match op with
        | S64 SecOp.Classify
        | S32 SecOp.Classify -> []
        | I64 IntOp.ExtendSI32 -> [S64 SecOp.ExtendSS32]
        | I64 IntOp.ExtendUI32 -> [S64 SecOp.ExtendUS32]
        | I32 IntOp.WrapI64 -> [S32 SecOp.WrapS64]
        | I32 x -> [I32 x; S32 SecOp.Classify]
        | I64 x -> [I64 x; S64 SecOp.Classify]
        | _ -> [op]


let secify_nth_type (n: int) (lst: Types.value_type list): Types.value_type list =
    List.mapi (fun i e -> if i = n then secify_value_type e else e) lst

(*
let secify_op o =
    let op_map io =
        match io with
        |Clz | Ctz | Popcnt
        |Add | Sub | Mul | DivS | DivU | RemS | RemU
         And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
        | Eqz
        | Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
    | ExtendSI32 | ExtendUI32 | WrapI64
             | TruncSF32 | TruncUF32 | TruncSF64 | TruncUF64
             | ReinterpretFloat | Declassify
    match o with
    | I32 IntOp.Add -> S32 SecOp.Sub
    | _ -> o
*)
let secify (i: Ast.instr): Ast.instr list =
    let it' = match i.it with
        | Select
        | SecretSelect -> [SecretSelect]

        | Call _
        | GetGlobal _
        | SetGlobal _
        | TeeLocal _
        | GetLocal _
        | SetLocal _
        | BrIf _
        | Br _
        | BrTable _
        | CallIndirect _
        | CurrentMemory
        | Return
        | Unreachable
        | Drop
        | GrowMemory
        | Nop -> [i.it]

        | Convert cvtop -> List.map (fun x -> Convert x) (secify_cvtop cvtop)
        | Compare relop -> [Compare (secify_relop relop)]
        | Test testop -> [Test (secify_testop testop)]
        | Unary unop -> [Unary (secify_unop unop)]
        | Binary binop -> [Binary (secify_binop binop)]
        | Const v -> [secify_const i]
        | Load memop -> [Load {memop with ty = secify_value_type memop.ty}]
        | Store memop -> [Store {memop with ty = secify_value_type memop.ty}]
        | _ -> raise (Failure ("Unsupported secify (this should never happen) " ^ (instr_str i)))
    in
    List.map (fun x -> x @@ Source.no_region) it'

type expectation = Pub | Sec
let exp_str exp = match exp with
    | Pub -> "Pub"
    | Sec -> "Sec"



type var_exp = PubVar | AnyVar

let var_exp_str = function
    | PubVar -> "PubVar"
    | AnyVar -> "AnyVar"

type pub_answer = {
    locals: (var_exp list) list;
    returns: (var_exp list);
    globals: var_exp list;
}

type pub_context = {
    answer: pub_answer;
    lstack: var_exp list;
    brstack: var_exp list;
    func_i: int;
    param_counts: int list;
}

let replace (i: int) (el: 'a) (lst: 'a list) =
    List.mapi (fun i' x -> if i' = i then el else x) lst

let pubify_func (i: int) (a: pub_answer) =
    {a with
        returns=replace i PubVar a.returns
    }

let pubify_global (i: int) (a: pub_answer) =
    {a with
        globals=replace i PubVar a.returns
    }

let pubify_local (f_i: int) (i: int) (a: pub_answer) =
    {a with
        locals=List.mapi (fun f_i' el -> if f_i = f_i' then replace i PubVar el else el) a.locals
    }

let merge_exp_list ((a1,a2): var_exp list * var_exp list) =
    List.map (fun (a,b) -> if a = PubVar || b = PubVar then PubVar else AnyVar) (List.combine a1 a2)

let merge_pub_answers (a1: pub_answer) (a2: pub_answer) =
    {
        locals = List.map merge_exp_list  (List.combine a1.locals a2.locals);
        returns = merge_exp_list (a1.returns,a2.returns);
        globals = merge_exp_list (a1.globals, a2.globals);
    }

let rec first_n (n: int) (lst: 'a list): 'a list =
    first_n' n lst []
and first_n' n l out =
    if n <= 0
        then out
        else first_n' (n-1) (List.tl l) ((List.hd l)::out)

let rec pub_reducer (i: Ast.instr) (ctx: pub_context) =
    let expected, lstack' = match ctx.lstack with
        | [] -> AnyVar, ctx.lstack
        | e::ls -> e, ls
    in
    let ctx' = match i.it with
        | If (v, t_b,f_b) ->
            let sub_expected = if v = [] then AnyVar else expected in
            let sub_ctx = {ctx with lstack=[sub_expected]; brstack=sub_expected::ctx.brstack} in
            let ctx_t =
                List.fold_right pub_reducer t_b sub_ctx
            in
            let ctx_f =
                List.fold_right pub_reducer t_b sub_ctx
            in
            {ctx with
                answer=merge_pub_answers ctx_t.answer ctx_f.answer;
            }
        | Block(v, i's) ->
            let sub_expected = if v = [] then AnyVar else expected in
            let sub_ctx = {ctx with lstack=[sub_expected]; brstack=sub_expected::ctx.brstack} in
            let sub_ctx' = (List.fold_right pub_reducer i's sub_ctx) in
            { ctx with
                answer= sub_ctx'.answer
            }
        | Loop(v, i's) ->
            let sub_expected = if v = [] then AnyVar else expected in
            let sub_ctx = {ctx with lstack=[sub_expected]; brstack=AnyVar::ctx.brstack} in
            let sub_ctx' = (List.fold_right pub_reducer i's sub_ctx) in
            { ctx with
                answer= sub_ctx'.answer
            }
        | Call(var) ->
            let i = Int32.to_int var.it in
            if expected = PubVar
                then {ctx with answer= pubify_func i ctx.answer}
                else ctx

        | TeeLocal(var)
        | GetLocal(var) ->
            let i = Int32.to_int var.it in
            if expected = PubVar
                then {ctx with answer=pubify_local ctx.func_i i ctx.answer}
                else ctx
        | GetGlobal(var) ->
            let i = Int32.to_int var.it in
            if expected = PubVar
                then {ctx with answer=pubify_global i ctx.answer}
                else ctx
        | BrIf(var) ->
            let i = Int32.to_int var.it in
            if expected = PubVar
                then {ctx with brstack=replace i PubVar ctx.brstack}
                else ctx
        | BrTable(vars, var) ->
            let i's = List.map (fun x -> Int32.to_int x.it) (var::vars) in
            let var_exps = List.map (fun i -> (i, List.nth ctx.brstack i)) i's in
            let comb_exp = if List.exists (fun (_,x) -> x = PubVar) var_exps then PubVar else AnyVar in 
            if comb_exp = PubVar
            then
                let mapper i exp =
                    match List.assoc_opt i var_exps with
                    | None -> exp
                    | Some e -> PubVar
                in
                {ctx with brstack=List.mapi mapper ctx.brstack}
            else ctx

        | _ -> ctx
    in
    let remove_me = i in
    let lstack'': var_exp list = match i.it with
        | Call var ->
            let i = Int32.to_int var.it in
            (first_n (List.nth ctx.param_counts i) (List.nth ctx.answer.locals i)) @ lstack'
        | GetLocal _ -> lstack'
        | SetGlobal var ->
            let i = Int32.to_int var.it in
            Printf.printf "\nCurrent f: %d\n" ctx.func_i;
            Printf.printf "\nGlobals : %s\n" (String.concat ", " (List.map var_exp_str ctx.answer.globals));
            Printf.printf "\nLooking for : %d\n" i; 
            Printf.printf "\nAT: %s\n" (Source.string_of_region remove_me.at);
            let eee = (List.nth ctx.answer.globals i) in
            Printf.printf "%s %d set" (var_exp_str eee) i;
            eee::expected::lstack'
        | SetLocal var ->
            let i = Int32.to_int var.it in
            Printf.printf "\nCurrent f: %d\n" ctx.func_i;
            Printf.printf "\nLocals : %s\n" (String.concat ", " (List.map var_exp_str (List.nth ctx.answer.locals ctx.func_i)));
            Printf.printf "\nLooking for : %d\n" i; 
            Printf.printf "\nAT: %s\n" (Source.string_of_region remove_me.at);
            let eee = (List.nth (List.nth ctx.answer.locals ctx.func_i) i) in
            Printf.printf "%s %d set" (var_exp_str eee) i;
            eee::expected::lstack'
        | TeeLocal var ->
            let i = Int32.to_int var.it in
            (List.nth (List.nth ctx.answer.locals ctx.func_i) i)::lstack'
        | GetGlobal _ -> lstack'

        | Convert cvtop -> (match cvtop with
            | S32 SecOp.Classify
            | S64 SecOp.Classify -> lstack'
            | I64 IntOp.ExtendSI32
            | I64 IntOp.ExtendUI32
            | I32 IntOp.WrapI64 -> expected::lstack'
            | _ -> PubVar::lstack'
        )
        | Compare _ -> expected::expected::lstack'
        | If ([], _, _) -> PubVar::expected::lstack'
        | If (_, _, _) -> PubVar::lstack'
        | Block ([], _)
        | Loop ([], _) -> expected::lstack'
        | Block (_, _)
        | Loop (_, _) -> lstack'
        | BrIf var -> 
            let i = Int32.to_int var.it in
            PubVar::(List.nth ctx.brstack i)::lstack'
        | BrTable (_, var)
        | Br var -> 
            let i = Int32.to_int var.it in
            [List.nth ctx.brstack i]

        | CurrentMemory -> lstack'
        | CallIndirect _ -> PubVar::lstack'
        | SecretSelect
        | Select -> expected::expected::expected::lstack'
        | Return -> (List.nth ctx.answer.returns ctx.func_i)::[] (*maybe revisit*)
        | Drop -> AnyVar::expected::lstack'
        | Unreachable -> []
        | Nop -> lstack'
        | Test testop -> expected::lstack'
        | Unary unop -> expected::lstack'
        | GrowMemory -> PubVar::lstack'
        | Binary binop -> expected::expected::lstack'
        | Store memop -> expected::PubVar::lstack'
        | Load memop -> PubVar::lstack'
        | Const _ -> lstack'
    in
    {ctx' with lstack=lstack''}

let apply_var_exp ((e, v): var_exp * value_type) =
    match (e,v) with
    | PubVar, S32Type
    | PubVar, I32Type -> I32Type
    | PubVar, S64Type
    | PubVar, I64Type -> I64Type
    | _, F32Type -> F32Type
    | _, F64Type -> F64Type
    | AnyVar, S32Type
    | AnyVar, I32Type -> S32Type
    | AnyVar, S64Type
    | AnyVar, I64Type -> S64Type

let apply_exp ((e, v): expectation * value_type) =
    match (e,v) with
    | Pub, S32Type
    | Pub, I32Type -> I32Type
    | Pub, S64Type
    | Pub, I64Type -> I64Type
    | _, F32Type -> F32Type
    | _, F64Type -> F64Type
    | Sec, S32Type
    | Sec, I32Type -> S32Type
    | Sec, S64Type
    | Sec, I64Type -> S64Type


let rec rm_n (n: int) (l: 'a list): 'a list =
    if n = 0
    then l
    else rm_n (n-1) (List.tl l)


let apply_local_exps ((exps, f): var_exp list * Ast.func) =
    let local_pairs = List.combine exps f.it.locals in
    {f with it= {f.it with locals=List.map apply_var_exp local_pairs}}

let apply_ans (ans: pub_answer) (m: Ast.module_) =
    let local_pairs = List.map (fun (exps, f) ->
        let FuncType(_, params, _) = Ast.func_type_for m f.it.ftype in
        let exps' = rm_n (List.length params) exps in
        (exps', f)
    ) (List.combine ans.locals m.it.funcs) in
    let funcs' = List.map apply_local_exps local_pairs in
    let m' = {m with it={m.it with funcs=funcs'}} in
    let (m'', vars) =  List.fold_left (fun (m, vars) ((exps, f), ret_exp) ->
        let ftype = Ast.func_type_for m f.it.ftype in
        let FuncType(_, params, ret) = Ast.func_type_for m f.it.ftype in
        let par_exps = first_n (List.length params) exps in
        let params' = List.map apply_var_exp (List.combine par_exps params) in
        let ret' = if ret = [] then [] else [apply_var_exp (ret_exp, List.hd ret)] in
        let ftype' = (FuncType (Untrusted, params', ret')) in
        let m', v' = if ftype = ftype' then (m, f.it.ftype) else ensure_type m ftype' in
        (m', vars @ [v'])
    ) (m', []) (List.combine (List.combine ans.locals m'.it.funcs) ans.returns) in
    let funcs'' = List.map (fun (f, v) -> {f with it={f.it with ftype = v}}) (List.combine (m''.it.funcs) vars)in
    let m''' = {m'' with it={m''.it with funcs=funcs''}} in
    m'''


let rewrite_vars (m: Ast.module_): Ast.module_ =
    let any_list lst = List.map (fun l -> AnyVar) lst in
    let func_types = List.map (fun f -> Ast.func_type_for m f.it.ftype)  m.it.funcs in
    let param_counts = List.map (fun (FuncType (_, p, _)) -> List.length p) func_types in
    let default_ans = {
        locals= List.map (fun (f: Ast.func) -> 
            let FuncType (_, p, _) = Ast.func_type_for m f.it.ftype in
            (any_list p) @ (any_list f.it.locals)
        ) m.it.funcs;
        returns= any_list m.it.funcs;
        globals= any_list m.it.globals;
    }
    in
    let func_reducer ((i , ans): int * pub_answer) (f: Ast.func) =
        let ctx = {
            answer= ans;
            lstack= [List.nth ans.returns i];
            brstack= [];
            func_i= i;
            param_counts = param_counts;
        } in
        let ctx' = List.fold_right pub_reducer f.it.body ctx in
        (i + 1, ctx'.answer)
    in
    let rec find_ans (ans: pub_answer) =
        let (_, ans') = List.fold_left func_reducer (0, ans) m.it.funcs in
        if ans <> ans'
        then find_ans ans'
        else ans'
    in
    let final_ans = find_ans default_ans in
    apply_ans final_ans m


let type_exp (v: value_type): expectation =
    match v with
        | S32Type | S64Type -> Sec
        | _ -> Pub

let func_exps (m: Ast.module_) (v: var): expectation list =
    let v_i = Int32.to_int v.it in
    let ftv = (List.nth m.it.funcs v_i).it.ftype in
    let ftv_i = Int32.to_int ftv.it in
    let FuncType (_, pars, _) = (List.nth m.it.types ftv_i).it in
    print_string (string_of_value_types pars);
    List.map type_exp pars

let return_exps (m: Ast.module_) (f_i: int): expectation list =
    let ftv = (List.nth m.it.funcs f_i).it.ftype in
    let ftv_i = Int32.to_int ftv.it in
    let FuncType (_, _, rets) = (List.nth m.it.types ftv_i).it in
    List.map type_exp rets

let local_exp (m: Ast.module_) (f_i: int) (v: var): expectation = 
    let f_e = func_exps m (Int32.of_int f_i @@ no_region) in
    let f_e_l = List.length f_e in
    let v_i = Int32.to_int v.it in
    let f = List.nth m.it.funcs f_i in
    if v_i < f_e_l then
        List.nth f_e v_i
    else
        type_exp (List.nth f.it.locals (v_i - f_e_l))

let global_exp (m: Ast.module_) (v: var): expectation = 
    let v_i = Int32.to_int v.it in
    let GlobalType (gvt, _) = (List.nth m.it.globals v_i).it.gtype in
    type_exp gvt


let rec rewrite_instrs (i: Ast.instr) (m, f_i, instrs, exps, brs): (Ast.module_ * int * Ast.instr list * expectation list * expectation list) = 
    let expected, exps_tail = match exps with
        | [] -> Sec, []
        | e::exps_tail -> e, exps_tail
    in
    let is': Ast.instr list = match i.it with
    | Block (v, b) -> 
        let br_exp = match v with
        | [] -> Sec
        | _ -> expected
        in
        let v' = match v with
        | [] -> []
        | t::_ -> [apply_exp (expected,t)]
        in
        let (_,_,inner_is, _, _) = List.fold_right rewrite_instrs b (m, f_i, [], [br_exp], br_exp::brs)
        in
        [Block(v', inner_is) @@ no_region]
    | Loop (v, b) ->
        let br_exp = Sec in
        let v' = match v with
        | [] -> []
        | t::_ -> [apply_exp (expected,t)]
        in
        let (_,_,inner_is, _, _) = List.fold_right rewrite_instrs b (m, f_i, [], [expected], br_exp::brs)
        in
        [Loop(v',inner_is) @@ no_region]

    | If(v, tb, fb) ->
        let br_exp = match v with
        | [] -> Sec
        | _ -> expected
        in
        let v' = match v with
        | [] -> []
        | t::_ -> [apply_exp (expected, t)]
        in
        let (_,_,t_is, _, _) = List.fold_right rewrite_instrs tb (m, f_i, [], [br_exp], br_exp::brs) in
        let (_,_,f_is, _, _) = List.fold_right rewrite_instrs fb (m, f_i, [], [br_exp], br_exp::brs) in
        [If(v', t_is, f_is) @@ no_region]
    | _ -> if expected = Pub then [i] else secify i
    in
    let exps': expectation list = match i.it with
        | Call var -> (func_exps m var) @ exps_tail
        | GetGlobal _
        | GetLocal _ -> exps_tail
        | SetGlobal var -> (global_exp m var)::expected::exps_tail
        | SetLocal var -> (local_exp m f_i var)::expected::exps_tail
        | TeeLocal var -> (local_exp m f_i var)::exps_tail

        | Convert cvtop -> (match cvtop with
            | I64 IntOp.ExtendSI32
            | I64 IntOp.ExtendUI32
            | I32 IntOp.WrapI64 -> expected::exps_tail
            | _ -> Pub::exps_tail
        )
        | Compare _ -> expected::expected::exps_tail
        | If ([], _, _) -> Pub::expected::exps_tail
        | If (_, _, _) -> Pub::exps_tail
        | Block ([], _)
        | Loop ([], _) -> expected::exps_tail
        | Block (_, _)
        | Loop (_, _) -> exps_tail
        | BrIf var ->
            let i = Int32.to_int var.it in
            Pub::(List.nth brs i)::exps_tail
        | BrTable (_, var)
        | Br var ->
            let i = Int32.to_int var.it in
            [List.nth brs i]
        | CurrentMemory -> exps_tail
        | CallIndirect _ -> Pub::exps_tail
        | SecretSelect
        | Select -> expected::expected::expected::exps_tail
        | Return -> (return_exps m f_i)
        | Drop -> Pub::expected::exps_tail
        | Unreachable -> [Sec]
        | Nop -> exps_tail
        | Test testop -> expected::exps_tail
        | Unary unop -> expected::exps_tail
        | GrowMemory -> Pub::exps_tail
        | Binary binop -> expected::expected::exps_tail
        | Store memop -> expected::Pub::exps_tail
        | Load memop -> Pub::exps_tail
        | Const _ -> exps_tail
        in
        (m, f_i, is' @ instrs, exps', brs)

let rewrite_func (m: Ast.module_) (f_i: int) (f: Ast.func): Ast.func =
    let (_, _, is', _, _) =
        List.fold_right rewrite_instrs f.it.body (m, f_i, [], [], []) in
    {f with it={f.it with body=is'}}

let rewrite_import (m: Ast.module_) (im: import): import =
    let desc = match im.it.idesc.it with
    | MemoryImport (MemoryType (l, _)) -> MemoryImport (MemoryType (l, Secret))
    | i -> i
    in
    {im with it={im.it with idesc = desc @@ no_region}}


(* produces a new module with the function rewritten *)
let rewrite (m: Ast.module_) =
    let m' = rewrite_vars m in
    let m_it = m'.it in
    let m_it' = {m_it with
        imports = List.map (rewrite_import m') m_it.imports;
        funcs = List.mapi (rewrite_func m') m_it.funcs;
        memories = List.map secify_memory m_it.memories;
    } in
    {m' with it=m_it'}
