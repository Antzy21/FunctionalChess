namespace GetMoves

open Xunit
open ChessTest.Helpers.Functions
open Chess

module Enpassant =
    [<Fact>]
    let ``White can take c6 en passing two ways`` () =
        let moves = 
            $"rnbqkbnr/p7/Pp1p1p1p/1PpPpPpP/2P1P1P1/8/8/RNBQKBNR w KQkq c6 0 1"
            |> GetPossibleMoves.forPieceType Pawn
        Assert.Contains("Pb5 -> xpc6 e.p.", moves)
        Assert.Contains("Pd5 -> xpc6 e.p.", moves)

    [<Fact>]
    let ``White moves include taking g6 en passing`` () =
        let moves = 
            $"rnbqkbnr/pppp1p1p/8/4pPp1/4P3/8/PPPP2PP/RNBQKBNR w KQkq g6 0 1"
            |> GetPossibleMoves.forPieceType Pawn
        Assert.Contains("Pf5 -> xpg6 e.p.", moves)
    
    [<Fact>]
    let ``Black moves include taking en passing`` () =
        let moves = 
            $"rnbqkbnr/2p5/2Pp1p1p/1p1PpPpP/pP2P1P1/P7/8/RNBQKBNR b KQkq b3 0 1"
            |> GetPossibleMoves.forPieceType Pawn
        Assert.Contains("pa4 -> xPb3 e.p.", moves)
    
module Castling =
    [<Fact>]
    let ``Castling both sides possible for White`` () =
        let result = GetPossibleMoves.castling "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 0"
        Assert.Equal<string list>(["0-0"; "0-0-0"], result)

    [<Fact>]
    let ``Castling both sides possible for Black`` () =
        let result = GetPossibleMoves.castling "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQkq - 0 1"
        Assert.Equal<string list>(["0-0"; "0-0-0"], result)

    [<Fact>]
    let ``Castling not allowed from gameState for White`` () =
        GetPossibleMoves.castling "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w kq - 0 0"
        |> Assert.Empty

    [<Fact>]
    let ``Castling not allowed from gameState for Black`` () =
        GetPossibleMoves.castling "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQ - 0 1"
        |> Assert.Empty
        
    [<Fact>]
    let ``Castling blocked by piece for White`` () =
        GetPossibleMoves.castling "r1b1k1nr/pppppppp/8/8/8/8/PPPPPPPP/R1B1K1NR w KQkq - 0 0"
        |> Assert.Empty
        
    [<Fact>]
    let ``Castling blocked by piece for Black`` () =
        GetPossibleMoves.castling "r1b1k1nr/pppppppp/8/8/8/8/PPPPPPPP/R1B1K1NR b KQkq - 0 1"
        |> Assert.Empty
        
    [<Fact>]
    let ``Castling blocked by moving through check for White`` () =
        GetPossibleMoves.castling "3rkr2/8/8/8/8/8/8/R3K2R w KQkq - 1 2"
        |> Assert.Empty

    [<Fact>]
    let ``King is not on starting square`` () =
        GetPossibleMoves.castling "r6r/4k3/8/8/8/8/4K3/R6R w KQkq - 0 0"
        |> Assert.Empty
        
    [<Fact>]
    let ``No rook available`` () =
        GetPossibleMoves.castling "4k3/8/8/8/8/8/8/4K3 w KQkq - 0 0"
        |> Assert.Empty
        
    [<Fact>]
    let ``Black castling blocked by moving through check`` () =
        GetPossibleMoves.castling "r3k2r/8/8/8/8/8/8/3RKR2 b KQkq - 1 2"
        |> Assert.Empty

module Promotion =

    [<Fact>]
    let ``White can promote move list`` () =
        let moves = 
            "8/P7/8/8/8/8/8/2k2K2 w - - 0 1"
            |> GetPossibleMoves.forPieceType Pawn
        let expected = ["Pa7 -> a8 = Q"; "Pa7 -> a8 = R"; "Pa7 -> a8 = B"; "Pa7 -> a8 = N"]
        Assert.Equal<string list>(expected, moves)

    [<Fact>]
    let ``Blocked from promoting`` () =
        "b7/P7/8/8/8/8/8/2k2K2 w - - 0 1"
        |> GetPossibleMoves.forPieceType Pawn
        |> Assert.Empty

    [<Fact>]
    let ``Promoting by taking move list`` () =
        let moves = 
            "br6/P7/8/8/8/8/8/2k2K2 w - - 0 1"
            |> GetPossibleMoves.forPieceType Pawn
        let expected = ["Pa7 -> xb8 = Q"; "Pa7 -> xb8 = R"; "Pa7 -> xb8 = B"; "Pa7 -> xb8 = N"]
        Assert.Equal<string list>(expected, moves)
