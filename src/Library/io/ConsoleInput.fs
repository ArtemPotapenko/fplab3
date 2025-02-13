namespace fplab3.lib.io

open System.IO

module ConsoleInput =
    type Reader(input : TextReader) =
        member val input = input
        member val nextLine : string = input.ReadLine()
    
    let hasNext (reader : Reader) =
         (isNull >> not) reader.nextLine && reader.nextLine <> "end"
    
    let getLine (reader : Reader) =
        if hasNext reader then
            reader.nextLine
        else
            null
    
    let next (reader : Reader) =
        Reader(reader.input)