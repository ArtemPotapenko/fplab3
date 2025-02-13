module test

open NUnit.Framework
open fplab3.lib.method.LinealInterpolation
open fplab3.lib.method.LinealInterpolation.LinealInterpolation
open fplab3.lib.method.NewtonInterpolation

[<SetUp>]
let Setup () =
    ()

[<Test>]
let linealTest () =
    let state = LinealInterpolation.create 1 1
    let state2 = state |> LinealInterpolation.add 2 2 |> LinealInterpolation.add 3 3
    Assert.AreEqual(state2.result 3, 3)
    let state3 = state2 |> LinealInterpolation.add 5 7
    Assert.AreEqual(state3.result 4, 5)

[<Test>]
let newthonTest () =
    let state = NewtonInterpolation.create 1 1
    let state2 = state |> NewtonInterpolation.add 2 4 |> NewtonInterpolation.add 3 9
    Assert.AreEqual(state2.result 5, 25)
    let state3 = state2 |> NewtonInterpolation.add 4 16
    Assert.AreEqual(state3.result 4, 16)