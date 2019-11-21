module Context

//open System.IO
open Ast

type ModuleContext = {
    Path        : string
    Father      : ModuleContext
    TypeInfos   : (string * TypeExpr list) ref
    FuncInfos   : FuncTemplateInfo list ref
}

and FuncTemplateInfo = {
    Module      : ModuleContext
    FunctionName: string
    TypeVar     : string Set
    FuncTypeInfo: TypeExpr list
    ParameNames : string Set
    IsPure      : bool
    IsAsync     : bool
    bodys       : (string list * TypeExpr list * Expr list) list
}

type PackageContext = Map<string, ModuleContext>

type Contexts =
    | PackageContext of PackageContext
    | ModuleContext of ModuleContext
    | ScopeContext of string list * TypeExpr list

let ScopeContext (ss: string list) (ts: TypeExpr list) =
    assert (ss.Length = ts.Length)
    Contexts.ScopeContext (ss, ts)
    