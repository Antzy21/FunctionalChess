namespace Promotion

open Xunit
open Chess
open TestHelpers

module White =

    [<Fact>]
    let ``White can take promote`` () =
        let moves = 
            $"8/P7/8/8/8/8/8/2k2K3 w - - 0 1"
            |> getPossibleMovesForPieceType Pawn
        let expected = ["Pa7 -> Qa8"; "Pa7 -> Ra8"; "Pa7 -> Ba8"; "Pa7 -> Na8"]
        Assert.Equal<string list>(expected, moves)