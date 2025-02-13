namespace fplab3.lib.io.ConsoleOutput

module ConsoleOutput =
    let rec printResult startPoint endPoint step f=
        if (startPoint < endPoint) then
             printf "%f, %f \n" startPoint (f startPoint)
             printResult (startPoint + step) endPoint step f
        else
            startPoint
            