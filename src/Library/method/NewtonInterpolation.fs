namespace fplab3.lib.method.NewtonInterpolation

module NewtonInterpolation =
    type NewtonState (x : double, y : double, result : double -> double, helpPoly : double -> double) =
        member this.result t =
            let a = (y - result x) / (helpPoly x)
            a * helpPoly t + result t
        member this.helpPoly t = (helpPoly t) * (t - x)
        
        member val point = (x, y)
        
    
    let create x y = NewtonState(x, y, (fun _ -> y), (fun _ -> 1.0))
    
    let add x y (state : NewtonState) = NewtonState(x, y, state.result, state.helpPoly)