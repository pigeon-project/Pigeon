module AST

//type TypeExpr =
//    | TypeApply of string * TypeExpr list
//    | UnionType of string * TypeExpr list
//    | TupleType of TypeExpr list
//    | Record    of TypeExpr list
//    | Callable  of TypeExpr list
//    | TypeName  of string

type Pattern = unit

type LiteralValue =
    | Bool      of bool
    | Char      of char
    | Int       of int64
    | Uint      of uint64
    | Float     of double
    | String    of string
    | Symbol    of string (*你的恶趣味*)
    | RowEmpt   of string
type Name = string
type Expr =
    | Variable of Name
    | FuncCall of Expr* Expr list
    | Func of Name list * Expr
    | LetIn of Name * Expr * Expr
    | Lit of LiteralValue
    | RowGet of Expr * Name
    | RowExtend of Name * Expr * Expr
    // FIXME:
    // 这些恶趣味都是啥?
    // 能不能是语法糖?
    //| Match         of (Pattern * Expr option * Expr) list
    //| Cond          of (Expr * Expr) list
    //| Loop          of                  Expr list
    //| While         of           Expr * Expr list
    //| For           of Pattern * Expr * Expr list
    //| Let           of Pattern * Expr
    | Literal       of LiteralValue

// FIXME: Env给你吃了.jpg
// type FuncDef = string * string list * Expr list

// 用意不明
//type TopLevel =
//    | Comment       of string
//    | TypeAssert    of TypeAssert
//    | Funcdef       of FuncDef
//    | Let           of Pattern * Expr
//    | Do            of Expr list

// TODO: 以下不知道干啥的? 请注释
let Ast2Gast = 0
let Gast2Ast = 0

let LoadAst filepath =
    0

let DumpAst filepath ast =
    0
    //File.WriteAllText(filepath, JsonSerializer.Serialize(ast))

type Id = int
type Level = int

type TypeExpr =
    | TConst of Name (*就像Int Bool这种*)
    | TApp of TypeExpr * TypeExpr list
    | TArrow of TypeExpr list * TypeExpr
    | TVar of TVar ref
    | TRow of TypeExpr (*只使用RowEmpt和RowExtend*)
    | TRowEmpt
    | TRowExtend of Name * TypeExpr * TypeExpr
and TVar =
    | Unbound of Id * Level
    | Link of TypeExpr (*TypeExpr 垃圾F#*)
    | Generic of Id
// FIXME: Env 被吃掉了
//type TypeAssert = string * TypeExpr

(*请自己写tostr*)    