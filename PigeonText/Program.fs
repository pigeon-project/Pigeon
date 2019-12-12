module Program =

    open Ast
    open Infer

    [<EntryPoint>]
    let main _ =
        Func (["a"], Variable "a")
        |> infer (Map []) 0
        |> printfn "type: %A"
        0
