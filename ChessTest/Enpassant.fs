module Enpassant

open Xunit
open Chess
open ChessTest.Helpers.Functions

[<Fact>]
let ``White can take en passing two ways`` () =
    let enpassantSquare = "c6"
    let moves = 
        $"rnbqkbnr/p7/Pp1p1p1p/1PpPpPpP/2P1P1P1/8/8/RNBQKBNR w KQkq {enpassantSquare} 0 1"
        |> GetPossibleMoves.forPieceType Pawn
    Assert.Contains("Pb5 -> xpc6", moves)
    Assert.Contains("Pd5 -> xpc6", moves)

let ``White can take en passing`` () =
    let enpassantSquare = "g6"
    let moves = 
        $"rnbqkbnr/pppp1p1p/8/4pPp1/4P3/8/PPPP2PP/RNBQKBNR w KQkq {enpassantSquare} 0 1"
        |> GetPossibleMoves.forPieceType Pawn
    Assert.Contains("Pf5 -> xpg6", moves)
    
[<Fact>]
let ``Black can take en passing`` () =
    let enpassantSquare = "b3"
    let moves = 
        $"rnbqkbnr/2p5/2Pp1p1p/1p1PpPpP/pP2P1P1/P7/8/RNBQKBNR b KQkq {enpassantSquare} 0 1"
        |> GetPossibleMoves.forPieceType Pawn
    Assert.Contains("pa4 -> xPb3", moves)
