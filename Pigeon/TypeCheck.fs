module TypeCheck

open Ast
open System

(*
考虑废弃


let GetLiteralType lv =
    match lv with
    | Bool      _ -> "Bool"
    | Char      _ -> "Char"
    | Int       _ -> "Int"
    | Uint      _ -> "UInt"
    | Float     _ -> "Float"
    | String    _ -> "String"
    |> String.Intern
    //|> TypeName

let TypeInference e =
    match e with
    | Literal(v) -> Some (GetLiteralType v)
    | _ -> failwith "xxx"

*)