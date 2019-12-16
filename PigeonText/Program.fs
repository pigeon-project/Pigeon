module Program =

    open Ast
    open Infer

    [<EntryPoint>]
    let main _ =
        (Func (["a"], Variable "a")
        |> infer (Map []) 0).ToString ()
        |> printfn "type: %s"
        (Func (["a"], Lit (Int 1L))
        |> infer (Map []) 0).ToString ()
        |> printfn "type: %s"
        0
