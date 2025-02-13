namespace fplab3.service

open System
open fplab3.lib.io.ConsoleInput
open fplab3.lib.io.ConsoleOutput
open fplab3.lib.method.LinealInterpolation
open fplab3.lib.method.NewtonInterpolation
open fplab3.service.ArgsParserService

module MainService =
    let parseStr (s : String) =
        let a = s.Split(" ") |> Array.map Double.Parse
        (a[0], a[1])
        
    let execute (arg : Args) (reader : Reader) =
        let methods = arg.methods |> List.filter ((<>) Method.None) |> List.distinct
        let point1 = (getLine reader) |> parseStr
        let newReader = next reader
        let point2 = (getLine (newReader)) |> parseStr
        let lin = LinealInterpolation.create (fst point1) (snd point1) |> LinealInterpolation.add (fst point2) (snd point2)
        let newton = NewtonInterpolation.create (fst point1) (snd point1) |> NewtonInterpolation.add (fst point2) (snd point2)
        let rec subExecute (lin : LinealInterpolation.LinState) (newton : NewtonInterpolation.NewtonState)  xStart xEnd reader count = 
            let rec help method =
                match method with
                   | Method.Lineal ->
                       printfn "lineal"
                       let a = ConsoleOutput.printResult xStart xEnd arg.step lin.result
                       printfn ""
                       a
                   | Method.Newton ->
                       match count with
                       | 4 ->
                          printfn "newton"
                          let a = ConsoleOutput.printResult (fst point1) xEnd arg.step newton.result
                          printfn ""
                          a
                       | x when x > 4 ->
                          printfn "newton"
                          let a = ConsoleOutput.printResult xStart xEnd arg.step newton.result
                          printfn ""
                          a
                       | _ ->
                           xStart
                   | _ -> xStart
            let newEnd = methods |> List.map help |> List.max
            let newReader = next reader
            if (hasNext newReader) then
                let point = (getLine newReader) |> parseStr
                subExecute (lin |> LinealInterpolation.add (fst point) (snd point)) (newton |> NewtonInterpolation.add (fst point) (snd point)) newEnd (fst point) (newReader) (count + 1)
        subExecute lin newton (fst point1) (fst point2) (newReader) 2
        