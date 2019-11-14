module Context

//open System.IO

type TypeInfo = {
    a: int
}

type FuncInfo = {
    a: int
}

type ModuleContext = {
    Path        : string
    TypeInfos   : TypeInfo list
    FuncInfos   : FuncInfo list
}

type PackageContext = PackageContext of Map<string, ModuleContext>


