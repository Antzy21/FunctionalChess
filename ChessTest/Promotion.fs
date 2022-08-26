namespace Promotion

open Xunit
open Chess
open TestHelpers

module White =

    [<Fact>]
    let ``White can promote`` () =
        let result = 
            "8/P7/8/8/8/8/8/2k2K2 w - - 0 1"
            |> GameState.fromFen
            |> GameState.makeMoveFromNotation "a8 = Q"
            |> GameState.toFen
        Assert.Equal("Q7/8/8/8/8/8/8/2k2K2 b - - 1 2", result)

    [<Fact>]
    let ``White can promote move list`` () =
        let moves = 
            "8/P7/8/8/8/8/8/2k2K2 w - - 0 1"
            |> getPossibleMovesForPieceType Pawn
        let expected = ["Pa7 -> a8 = Q"; "Pa7 -> a8 = R"; "Pa7 -> a8 = B"; "Pa7 -> a8 = N"]
        Assert.Equal<string list>(expected, moves)

    [<Fact>]
    let ``Blocked from promoting`` () =
        let moves = 
            "b7/P7/8/8/8/8/8/2k2K2 w - - 0 1"
            |> getPossibleMovesForPieceType Pawn
        Assert.Empty(moves)

    [<Fact>]
    let ``Promoting by taking`` () =
        let result = 
            "br6/P7/8/8/8/8/8/2k2K2 w - - 0 1"
            |> GameState.fromFen
            |> GameState.makeMoveFromNotation "xb8 = Q"
            |> GameState.toFen
        Assert.Equal("bQ6/8/8/8/8/8/8/2k2K2 b - - 1 2", result)

    [<Fact>]
    let ``Promoting by taking move list`` () =
        let moves = 
            "br6/P7/8/8/8/8/8/2k2K2 w - - 0 1"
            |> getPossibleMovesForPieceType Pawn
        let expected = ["Pa7 -> xb8 = Q"; "Pa7 -> xb8 = R"; "Pa7 -> xb8 = B"; "Pa7 -> xb8 = N"]
        Assert.Equal<string list>(expected, moves)
