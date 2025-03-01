## Лабораторная работа №3 (F#)

Потапенко Артем P3334

## Интерполяция 

В рамках лабораторной реализованы методы линейной интерполяции и интерполяции по Ньютону

##

Основной код, обеспечивающий выполнение в потоке:

'''F#
let execute (arg: Args) (reader: Reader) =
        let methods = arg.methods |> List.filter ((<>) Method.None) |> List.distinct
        let point1 = (getLine reader) |> parseStr
        let newReader = next reader
        let point2 = (getLine (newReader)) |> parseStr

        let lin =
            LinealInterpolation.create (fst point1) (snd point1)
            |> LinealInterpolation.add (fst point2) (snd point2)

        let newton =
            NewtonInterpolation.create (fst point1) (snd point1)
            |> NewtonInterpolation.add (fst point2) (snd point2)

        let rec subExecute
            (lin: LinealInterpolation.LinState)
            (newton: NewtonInterpolation.NewtonState)
            xStart
            xEnd
            reader
            count
            =
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
                    | _ -> xStart
                | _ -> xStart

            let newEnd = methods |> List.map help |> List.max
            let newReader = next reader

            if (hasNext newReader) then
                let point = (getLine newReader) |> parseStr

                subExecute
                    (lin |> LinealInterpolation.add (fst point) (snd point))
                    (newton |> NewtonInterpolation.add (fst point) (snd point))
                    newEnd
                    (fst point)
                    (newReader)
                    (count + 1)

        subExecute lin newton (fst point1) (fst point2) (newReader) 2

'''


## Реализация методов интерполирование

Ньютон 

'''F#
ype Point = { x: double; y: double }

    type NewtonState(x: double, y: double, result: double -> double, helpPoly: double -> double, count, set) =
        member this.result t =
            let a = (y - result x) / (helpPoly x)
            a * helpPoly t + result t

        member this.helpPoly t = (helpPoly t) * (t - x)
        member this.count = count
        member this.point = { x = x; y = y }
        member this.points = if count > 5 then (set |> Set.add this.point) else Set.empty

    let create x y =
        NewtonState(x, y, (fun _ -> y), (fun _ -> 1.0), 1, Set.empty)

    let rec add x y (state: NewtonState) =
        if state.count = 9 then
            let addPoint state point = add point.x point.y (state)
            let newState = (state.points) |> Set.fold addPoint (create x y)
            newState
        else
            NewtonState(x, y, state.result, state.helpPoly, state.count + 1, state.points)
'''

Линейная

'''F#
module LinealInterpolation =
    type LinState(x: double, y: double, result: double -> double) =
        member val step = 1.0
        member val point = (x, y)
        member this.result t = result t

    let add (x2: double) (y2: double) (lin: LinState) : LinState =
        let x1, y1 = lin.point
        let f (x: double) = (y1 - y2) / (x1 - x2) * (x - x1) + y1
        LinState(x2, y2, f)

    let create x1 y1 = LinState(x1, y1, fun _ -> y1)
'''
