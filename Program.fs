open System
open Chess

[<EntryPoint>]
let main args =
    Board.Create.starting ()
    |> Board.getPossibleMoves White
    |> List.iter Board.print
    0