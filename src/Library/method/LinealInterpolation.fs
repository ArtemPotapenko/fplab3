namespace fplab3.lib.method.LinealInterpolation

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
