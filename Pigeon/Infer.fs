module Infer

open Ast

let current_id = ref 0
let next_id () =
    let id = !current_id in
    current_id := id + 1
    id
let reset_id = current_id := 0

let new_unbound_var level = TVar(ref(Unbound(next_id(),level)))
let new_generic_var () = TVar(ref(Generic(next_id())))

type Env = Map<string,TypeExpr>
let check_error tvar_id tvar_level ty =
    let rec f tty =
        match tty with
        | TVar({contents = Link tty}) -> f tty
        | TVar({contents = Generic _}) -> assert false
        | TVar({contents = Unbound(other_id, other_level)} as other_tvar) ->
            if other_id = tvar_id then
                failwith "Error Recursive TypeExprpes!"
            else
                if other_level > tvar_level then
                    other_tvar := Unbound(other_id,tvar_level)
                else
                    ()
        | TApp(ty,ty_args) ->
            f ty
            List.iter f ty_args
        | TArrow(ty_params,ret_ty) ->
            List.iter f ty_params
            f ret_ty
        | TRow r -> f r
        | TRowExtend(lab,field_ty,row) -> f field_ty;f row
        | TConst _ -> ()
        | TRowEmpt -> ()
    f ty
let rec unifier ty1 ty2 =
    if ty1 = ty2 then 
        () 
    else
        match (ty1,ty2) with
            | TConst n1,TConst n2 when n1 = n2 -> ()
            | TApp(ty1,ty_args1),TApp(ty2,ty_args2) ->
                unifier ty1 ty2;
                List.iter2 unifier ty_args1 ty_args2
            | TArrow(ty_params1,ret_ty1),TArrow(ty_params2,ret_ty2) ->
                List.iter2 unifier ty_params1 ty_params2
                unifier ret_ty1 ret_ty2
            | TRow r1,TRow r2 -> unifier r1 r2
            | TRowEmpt,TRowEmpt -> ()
            | TVar({contents = Link ty1}),ty2 | ty1,TVar({contents = Link ty2})-> unifier ty1 ty2
            | TVar({contents = Unbound(id1,_)}),TVar({contents = Unbound(id2,_)}) when id1 = id2 ->
                assert false (*怎么可能??!! 傻了逼了,他俩咋一样的?!*)
            | ty,TVar({contents = Unbound(id,lv)} as tv) | TVar({contents = Unbound(id,lv)} as tv),ty ->
                check_error id lv ty;
                tv := Link ty          
            | TRowExtend(lab1,f1,r1),TRowExtend(lab2,f2,r2) ->
                let rest_r1_tvar_ref_opt =
                    match r1 with
                    | TVar ({contents = Unbound _} as tvar_ref) -> Some tvar_ref
                    | _ -> None
                in
                    let rest_row2 = rewrite_row r2 lab1 f1 in
                        match rest_r1_tvar_ref_opt with
                            | Some {contents = Link _} -> failwith "recursive row!"
                            | _ -> ()
                     ;
                     unifier r1 rest_row2
            | _ , _ -> printf "wtm? 这俩不合适啊!"
and rewrite_row row2 lab1 f1 =
    match row2 with
    | TRowEmpt -> failwithf "row does not contain label %A" lab1
    | TRowExtend(label2, field_ty2, rest_row2) when label2 = lab1 ->
        unifier f1 field_ty2
        rest_row2
    | TRowExtend(label2, field_ty2, rest_row2) ->
        TRowExtend(label2, field_ty2, rewrite_row rest_row2 lab1 f1)
    | TVar {contents = Link row2} -> rewrite_row row2 lab1 f1
    | TVar ({contents = Unbound(id, level)} as tvar) ->
        let rest_row2 = new_unbound_var level in
        let ty2 = TRowExtend(lab1, f1, rest_row2) in
        tvar := Link ty2 ;
        rest_row2
    | _ -> failwith "row type expected"


let rec generalize level x =
    match x with
    | TVar {contents = Unbound(id, other_level)} when other_level > level ->
            TVar (ref (Generic id))
    | TApp(ty, ty_arg_list) ->
            TApp(generalize level ty, List.map (generalize level) ty_arg_list)
    | TArrow(param_ty_list, return_ty) ->
            TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
    | TVar {contents = Link ty} -> generalize level ty
    | TRow row -> TRow (generalize level row)
    | TRowExtend(label, field_ty, row) ->
            TRowExtend(label, generalize level field_ty, generalize level row)
    | TVar {contents = Generic _} | TVar {contents = Unbound _}
    | TConst _ | TRowEmpt as ty -> ty


let instantiate level ty =
    let id_var_map = ref (Map [] : Map<Id,TypeExpr>) in
        let rec f ty =
            match ty with
            | TConst _ -> ty
            | TVar {contents = Link ty} -> f ty
            | TVar {contents = Generic id} ->
                match (!id_var_map).TryFind id with
                | Some ty -> ty
                | None ->
                    let var = new_unbound_var(level) 
                    in id_var_map := (!id_var_map).Add(id,var);
                    var
            | TVar {contents = Unbound _} -> ty
            | TApp(ty, ty_arg_list) ->
                TApp(f ty, List.map f ty_arg_list)
            | TArrow(param_ty_list, return_ty) ->
                TArrow(List.map f param_ty_list, f return_ty)
            | TRow row -> TRow (f row)
            | TRowEmpt -> ty
            | TRowExtend(label, field_ty, row) ->
                        TRowExtend(label, f field_ty, f row)
            in f ty

let rec match_fun_ty num_params x =
    match x with
    | TArrow(param_ty_list, return_ty) ->
            if List.length param_ty_list <> num_params
            then failwith "参数对不上啊!"
            else param_ty_list, return_ty
    | TVar {contents = Link ty} -> match_fun_ty num_params ty
    | TVar ({contents = Unbound(id, level)} as tvar) ->
            let param_ty_list = 
                let rec f = function
                    | 0 -> []
                    | n -> new_unbound_var level :: f (n - 1)
                in
                f num_params
            in
            let return_ty = new_unbound_var level in
            tvar := Link (TArrow(param_ty_list, return_ty)) ;
            param_ty_list, return_ty
    | _ -> failwith "是函数吗?就往里怼?"

// TODO: 坑自己填
// 就让你调用一下会吧
let rec infer (env: Env) level x =
    match x with
    | Variable name ->
        let res = env.TryFind name in
            match res with
                | Some ty -> instantiate level ty
                | None -> failwith "找不着啊!"