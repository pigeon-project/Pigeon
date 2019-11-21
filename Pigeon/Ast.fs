module Ast

type TypeExpr =
    | TypeApply of string * TypeExpr list
    | UnionType of string * TypeExpr list
    | TupleType of TypeExpr list
    | Record    of TypeExpr list
    | Callable  of TypeExpr list
    | TypeName  of string

type TypeAssert = string * TypeExpr

type Pattern = unit

type LiteralValue =
    | Bool      of bool
    | Char      of char
    | Int       of int64
    | UInt      of uint64
    | Float     of double
    | String    of string

type Expr =
    | NewClosure    of Pattern list * Expr list
    | FuncCall      of Expr * Expr list
    | Cond          of (Expr * Expr) list
    | Match         of (Pattern * Expr option * Expr) list
    | Loop          of                  Expr list
    | While         of           Expr * Expr list
    | For           of Pattern * Expr * Expr list
    | LetIn         of Pattern * Expr * Expr List
    | Let           of Pattern * Expr
    | Literal       of LiteralValue
    | Symbol        of string

type FuncDef = string * string list * Expr list

type TopLevel =
    | Comment       of string
    | TypeAssert    of TypeAssert
    | Funcdef       of FuncDef
    | Let           of Pattern * Expr
    | Do            of Expr list

let Ast2Gast = 0
let Gast2Ast = 0

let LoadAst filepath =
    0

let DumpAst filepath ast =
    0
    //File.WriteAllText(filepath, JsonSerializer.Serialize(ast))