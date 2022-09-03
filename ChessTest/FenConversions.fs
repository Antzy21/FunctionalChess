module FenConversions

open Xunit
open Chess

[<Fact>]
let ``Starting Fen converts with inverse`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
    fen
    |> GameState.fromFen
    |> GameState.toFen
    |> fun f -> Assert.Equal(f, fen)

[<Fact>]
let ``Empty board`` () =
    let fen = "4k3/8/8/8/8/8/8/4K3 w KQkq - 0 0"
    fen
    |> GameState.fromFen
    |> GameState.toFen
    |> fun f -> Assert.Equal(f, fen)

[<Fact>]
let ``Asymetric board`` () =
    let fen = "4k3/8/8/nNnN4/8/8/PPPP4/4K3 w KQkq - 0 0"
    fen
    |> GameState.fromFen
    |> GameState.toFen
    |> fun f -> Assert.Equal(f, fen)

[<Fact>]
let ``Sets Enpassant square for White`` () =
    let game = GameState.newGame ()
    NotationParser.parse game.playerTurn game.board "a4"
    |> fun move -> GameState.makeMove move game
    |> GameState.toFen
    |> fun fen ->
        Assert.Equal("rnbqkbnr/pppppppp/8/8/P7/8/1PPPPPPP/RNBQKBNR b KQkq a3 1 1", fen)

[<Fact>]
let ``Sets Enpassant square for Black`` () =
    let game = 
        "rnbqkbnr/p1pppppp/6p1/1P6/8/8/P1PPPPPP/RNBQKBNR b KQkq - 3 2"
        |> GameState.fromFen
    NotationParser.parse game.playerTurn game.board "a5"
    |> fun move -> GameState.makeMove move game
    |> GameState.toFen
    |> fun fen ->
        Assert.Equal("rnbqkbnr/2pppppp/6p1/pP6/8/8/P1PPPPPP/RNBQKBNR w KQkq a6 4 2", fen)

[<Fact>]
let ``Parse e4 then e5`` () =
    let gameFen =
        GameState.newGame ()
        |> GameState.makeMoveFromNotation "e4"
        |> GameState.makeMoveFromNotation "e5"
        |> GameState.toFen
    Assert.Equal("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 2 1", gameFen)