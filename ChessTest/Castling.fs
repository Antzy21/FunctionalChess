namespace Castling

open Xunit
open TestHelpers

module White =

    [<Fact>]
    let ``White castling both sides possible`` () =
        let castlingMoves = getPossibleCastlingMoveNotations "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 1 2"
        Assert.StrictEqual<string list>(["0-0-0"; "0-0"], castlingMoves)

    [<Fact>]
    let ``White castling not allowed from gameState`` () =
        getPossibleCastlingMoveNotations "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w kq - 1 2"
        |> Assert.Empty
        
    [<Fact>]
    let ``White castling blocked by piece`` () =
        getPossibleCastlingMoveNotations "r1b1k1nr/pppppppp/8/8/8/8/PPPPPPPP/R1B1K1NR w KQkq - 1 2"
        |> Assert.Empty
        
    [<Fact>]
    let ``White castling blocked by moving through check`` () =
        getPossibleCastlingMoveNotations "3rkr2/8/8/8/8/8/8/R3K2R w KQkq - 1 2"
        |> Assert.Empty
    
module Black = 

    [<Fact>]
    let ``Black castling both sides possible`` () =
        let castlingMoves = getPossibleCastlingMoveNotations "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQkq - 1 2"
        Assert.StrictEqual<string list>(["0-0-0"; "0-0"], castlingMoves)

    [<Fact>]
    let ``Black castling not allowed from gameState`` () =
        let castlingMoves = getPossibleCastlingMoveNotations "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQ - 1 2"
        Assert.Empty
        
    [<Fact>]
    let ``Black castling blocked by piece`` () =
        getPossibleCastlingMoveNotations "r1b1k1nr/pppppppp/8/8/8/8/PPPPPPPP/R1B1K1NR b KQkq - 1 2"
        |> Assert.Empty
        
    [<Fact>]
    let ``Black castling blocked by moving through check`` () =
        getPossibleCastlingMoveNotations "r3k2r/8/8/8/8/8/8/3RKR2 b KQkq - 1 2"
        |> Assert.Empty
