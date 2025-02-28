namespace fplab3.service

module ArgsParserService =
    type Method =
        | Lineal = 0
        | Newton = 1
        | None = 3

    type Args(step, methods: list<Method>) =
        member val step = step
        member val methods = methods

    let DEFAULT_METHOD = Method.Newton
    let DEFAULT_STEP = 1.0

    let parse (args: string array) =
        let mapFun name =
            match name with
            | "lineal" -> Method.Lineal
            | "newton" -> Method.Newton
            | _ -> Method.None

        let methods, step =
            args
            |> Array.fold
                (fun (curAlgs, curStep) arg ->
                    match arg.Split('=') with
                    | [| "-method"; algs |] -> (algs.Split(',') |> Array.toList |> List.map mapFun, curStep)
                    | [| "-step"; step |] -> (curAlgs, float step)
                    | _ -> (curAlgs, curStep))
                ([ DEFAULT_METHOD ], DEFAULT_STEP)

        Args(step, methods)
