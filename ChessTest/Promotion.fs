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
        let expected = ["Pa7 -> a8 = Q"; "Pa7 -> a8 = R"; "Pa7 -> a8 = B"; "Pa7 -> a8 = N"]
        Assert.Equal<string list>(expected, moves)

    [<Fact>]
    let ``Blocked from promoting`` () =
        let moves = 
            $"b7/P7/8/8/8/8/8/2k2K3 w - - 0 1"
            |> getPossibleMovesForPieceType Pawn
        Assert.Empty(moves)

    [<Fact>]
    let ``Promoting by taking`` () =
        let moves = 
            $"br6/P7/8/8/8/8/8/2k2K3 w - - 0 1"
            |> getPossibleMovesForPieceType Pawn
        let expected = ["Pa7 -> xb8 = Q"; "Pa7 -> xb8 = R"; "Pa7 -> xb8 = B"; "Pa7 -> xb8 = N"]
        Assert.Equal<string list>(expected, moves)