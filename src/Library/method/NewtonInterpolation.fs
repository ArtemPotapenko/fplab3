namespace fplab3.lib.method.NewtonInterpolation

module NewtonInterpolation =
    type Point = {x : double
                  y : double}
    type NewtonState (x : double, y : double, result : double -> double, helpPoly : double -> double, count, set) =
        member this.result t =
            let a = (y - result x) / (helpPoly x)
            a * helpPoly t + result t
        member this.helpPoly t = (helpPoly t) * (t - x)
        member this.count = count
        member  this.point = { x = x;
                             y = y }
        member this.points = if count > 5 then (set |> Set.add this.point) else Set.empty
    
    let create x y = NewtonState(x, y, (fun _ -> y), (fun _ -> 1.0), 1, Set.empty)
    
    let rec add x y (state : NewtonState) =
        if state.count = 9 then
            let addPoint state point = add point.x point.y (state)
            let newState = (state.points) |> Set.fold addPoint (create x y)
            newState
        else    
            NewtonState(x, y, state.result, state.helpPoly, state.count + 1, state.points)
