open System
open Chess

[<EntryPoint>]
let main args =
    Board.Create.starting ()
    |> Board.print

    0