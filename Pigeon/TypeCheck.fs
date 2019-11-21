module TypeCheck

open Ast
open System

let GetLiteralType lv =
    match lv with
    | Bool      _ -> "Bool"
    | Char      _ -> "Char"
    | Int       _ -> "Int"
    | UInt      _ -> "UInt"
    | Float     _ -> "Float"
    | String    _ -> "String"
    |> String.Intern
    |> TypeName

let TypeInference (e: Expr): TypeExpr option =
    match e with
    | Literal(v) -> Some (GetLiteralType v)
    | _ -> failwith "xxx"