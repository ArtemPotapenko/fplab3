namespace fplab3

open System
open fplab3.lib.io.ConsoleInput
open fplab3.service
module App = 
    [<EntryPoint>]
    let main args =
        let reader = Reader(Console.In)
        MainService.execute (ArgsParserService.parse args) reader
        0