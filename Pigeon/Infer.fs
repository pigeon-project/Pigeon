module Infer

open Ast
open Context

type CheckError =
    | TypeError
    | NameIsNotDefine of string
    | TypeIsNotDefine of string
    | NameIsNotTypeConstructor of string
    | TypeApplyParamsLengthError

type CheckErrorAndPos = Pos list * CheckError

type TypeResult = Result<TypeExpr, CheckErrorAndPos>

exception InstantiateError of CheckError

let rec Instantiate(tcontext: TypeContext) (expr: TypeExpr): TypeExpr =
    match expr with
    | TypeConstructor _ as r -> r
    | TypeApply (n, tl) as r ->
        match tcontext.TryFind n with
        | Some x ->
            match x with
            | TypeConstructor (tp, _) ->
                if tp.Length = tl.Length then r
                else raise (InstantiateError TypeApplyParamsLengthError)
            | _ -> raise (InstantiateError (NameIsNotTypeConstructor n))
        | None -> raise (InstantiateError (TypeIsNotDefine n))
    | UnionType tl ->
        List.map (fun (n, v) -> (n, Instantiate tcontext v)) tl |> UnionType
    | Record    tl ->
        List.map (fun (n, v) -> (n, Instantiate tcontext v)) tl |> Record
    | TupleType tl -> List.map (Instantiate tcontext) tl |> TupleType
    | Callable  tl -> List.map (Instantiate tcontext) tl |> Callable
    | TypeName _ as r -> r
    | TypeVar   (Ref n) -> TypeName n |> Instantiate tcontext
    | TypeVar   (Link e) -> Instantiate tcontext e
    | TypeVar   (Id id) -> failwith "emmmmm..."

let GetLitType e =
    match e with
    | Bool _    -> TypeName "bool"
    | Char _    -> TypeName "char"
    | Int _     -> TypeName "int64"
    | Uint _    -> TypeName "uint64"
    | Float _   -> TypeName "float64"
    | String _  -> TypeName "str"
    | RowEmpt _ -> TypeName "unit"

let Infer (Context (vc, tc): Context) (expr: Expr): TypeResult =
    match expr with
    | Variable n ->
        match vc.TryFind n with
        | Some x ->
            try Instantiate tc x |> Ok
            with | :? InstantiateError as x -> Error ([], x.Data0)
        | None -> Error ([], NameIsNotDefine n)
    | Lit v -> Ok (GetLitType v)
    | _ -> failwith "还没实现呢慌什么"