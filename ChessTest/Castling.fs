namespace Castling

open Xunit
open TestHelpers

module White =

    [<Fact>]
    let ``White castling both sides possible`` () =
        let castlingMoves = getPossibleCastlingMoveNotations "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 1 2"
        Assert.StrictEqual<string list>(castlingMoves, ["0-0-0"; "0-0"])

    [<Fact>]
    let ``White castling not allowed from gameState`` () =
        let castlingMoves = getPossibleCastlingMoveNotations "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w kq - 1 2"
        Assert.StrictEqual<string list>(castlingMoves, [])
        
    [<Fact>]
    let ``White castling blocked by piece`` () =
        let castlingMoves = getPossibleCastlingMoveNotations "r1b1k1nr/pppppppp/8/8/8/8/PPPPPPPP/R1B1K1NR w KQkq - 1 2"
        Assert.StrictEqual<string list>(castlingMoves, [])
    
module Black = 

    [<Fact>]
    let ``Black castling both sides possible`` () =
        let castlingMoves = getPossibleCastlingMoveNotations "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQkq - 1 2"
        Assert.StrictEqual<string list>(castlingMoves, ["0-0-0"; "0-0"])

    [<Fact>]
    let ``Black castling not allowed from gameState`` () =
        let castlingMoves = getPossibleCastlingMoveNotations "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQ - 1 2"
        Assert.StrictEqual<string list>(castlingMoves, [])
        
    [<Fact>]
    let ``Black castling blocked by piece`` () =
        let castlingMoves = getPossibleCastlingMoveNotations "r1b1k1nr/pppppppp/8/8/8/8/PPPPPPPP/R1B1K1NR b KQkq - 1 2"
        Assert.StrictEqual<string list>(castlingMoves, [])
