module Ast

open System.IO
open Context

(*
type Const =
    | Bool      of bool
    | Char      of char
    | Int       of uint64
    | Float     of double
    | String    of string

type GAstNode =
    | Unknow
    | Symbol    of string
    | Const     of Const
    | SubTree   of GAstNode list

type GAst =
    class
        val Name: string
        val Attributes: GAst list
        val SubTree: obj list
    end

//type GAst = obj list
*)

type TypeExpr =
    | TypeApply of string * TypeExpr list
    | UnionType of string * TypeExpr list
    | TupleType of TypeExpr list
    | Record    of TypeExpr list
    | Callable  of TypeExpr list
    | TypeName  of string

type TypeDef =
    | TypeAlias of string * string
    | NewType   of string * TypeExpr

type Pattern = unit

type LiteralValue =
    | Bool      of bool
    | Char      of char
    | Int       of int64
    | UInt      of uint64
    | Float     of double
    | String    of string

type Expr =
    | FuncCall  of Expr * Expr list
    | Cond      of (Expr * Expr) list
    | Match     of (Pattern * Expr option * Expr) list
    | Loop      of                  Expr list
    | While     of           Expr * Expr list
    | For       of Pattern * Expr * Expr list
    | LetIn     of Pattern * Expr * Expr List
    | Let       of Pattern * Expr
    | Literal   of LiteralValue
    | Symbol    of string

type FuncDef = string * Pattern list * Expr list

type TopLevel =
    | Comment       of string
    | Typedef       of TypeDef
    | Funcdef       of FuncDef
    | TypeAssert    of string * TypeExpr
    | Let           of Pattern * Expr

let Ast2Gast = 0
let Gast2Ast = 0

let LoadAst filepath =
    0

let DumpAst filepath ast =
    0
    //File.WriteAllText(filepath, JsonSerializer.Serialize(ast))