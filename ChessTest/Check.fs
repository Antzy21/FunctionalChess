namespace Check

open Xunit
open Chess
open TestHelpers

module White =

    [<Fact>]
    let ``White in check from Rook`` () =
        gameStateIsInCheck "3kr3/8/8/8/8/8/8/4K3 w KQkq - 1 2"
        |> Assert.True
    
    [<Fact>]
    let ``White can't move pawn`` () =
        "4k3/8/8/b7/8/2P5/8/4K3 w KQkq - 1 2"
        |> getPossibleMovesForPieceType Pawn
        |> Assert.Empty

    [<Fact>]
    let ``White can't move King into check`` () =
        "3q1r2/3P4/8/P3k2q/4nn2/7b/8/4K3 w - - 0 1"
        |> getPossibleMovesForPieceType King
        |> Assert.Empty
    
module Black = 

    [<Fact>]
    let ``Black in check from Rook`` () =
        gameStateIsInCheck "4k3/8/8/8/8/8/8/4RK2 b KQkq - 1 2"
        |> Assert.True
    
    [<Fact>]
    let ``Black can't move knight`` () =
        "4k3/4n3/8/8/8/8/8/4RK2 b KQkq - 1 2"
        |> getPossibleMovesForPieceType Knight
        |> Assert.Empty