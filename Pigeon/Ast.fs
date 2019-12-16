module Ast

type Id = int
type Level = int

type Name = string

type TypeExpr =
    | TypeApply of Name * TypeExpr list
    | UnionType of Name * TypeExpr list
    | TupleType of TypeExpr list
    | Record    of TypeExpr list
    | Callable  of TypeExpr list
    | TypeName  of Name
    | TypeVar   of TVar
and TVar =
    | Ref   of Name
    | Link  of TypeExpr
    | Id    of Id

type LiteralValue =
    | Bool      of bool
    | Char      of char
    | Int       of int64
    | Uint      of uint64
    | Float     of double
    | String    of string
    //| Symbol    of string (*你的恶趣味*)
    | RowEmpt   of string

// Deconstruction
type Pattern =
    | Variable of Name
    | MatchVar of Name
    | Const of LiteralValue
    | Tuple of Pattern list
    | Union of Name * Pattern
    | Ignore

type Expr =
    | Variable      of Name
    | FuncCall      of Expr * Expr list
    | Func          of Name * Pattern list * Expr
    | Lambda        of Pattern list * Expr
    | LetIn         of Pattern * Expr * Expr
    //| Let           of Pattern * Expr
    | Lit           of LiteralValue
    | RowGet        of Expr * Name
    | Match         of (Pattern * Expr option * Expr) list
    | Cond          of (Expr * Expr) list
    | Loop          of                  Expr list
    | While         of           Expr * Expr list
    | For           of Pattern * Expr * Expr list
