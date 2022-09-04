module Castling

open Xunit
open ChessTest.Helpers.Functions
open Chess

[<Fact>]
let ``Castling both sides possible for White`` () =
    let result = GetPossibleMoves.castling "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 0"
    Assert.StrictEqual<string list>(["0-0"; "0-0-0"], result)

[<Fact>]
let ``Castling both sides possible for Black`` () =
    let result = GetPossibleMoves.castling "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b KQkq - 0 1"
    Assert.StrictEqual<string list>(["0-0"; "0-0-0"], result)

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
        
[<Fact>]
let ``Parse Queenside castling`` () =
    let result =
        GameState.fromFen "r3k3/8/8/8/8/8/8/4K3 b q - 0 0"
        |> GameState.makeMoveFromNotation "0-0-0"
        |> GameState.toFen
    Assert.Equal("2kr4/8/8/8/8/8/8/4K3 w - - 1 0", result)
    
[<Fact>]
let ``Parse Kingside castling`` () =
    let result =
        GameState.fromFen "4k2r/8/8/8/8/8/8/4K3 b k - 0 0"
        |> GameState.makeMoveFromNotation "0-0"
        |> GameState.toFen
    Assert.Equal("5rk1/8/8/8/8/8/8/4K3 w - - 1 0", result)
