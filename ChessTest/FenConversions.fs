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
let ``Sets Enpassant square`` () =
    let game = 
        "rnbqkbnr/p1pppppp/6p1/1P6/8/8/P1PPPPPP/RNBQKBNR b KQkq - 3 2"
        |> GameState.fromFen
    NotationParser.parse game.playerTurn game.board "a5"
    |> fun move -> GameState.makeMove move game
    |> GameState.toFen
    |> fun fen ->
        Assert.Equal("rnbqkbnr/2pppppp/6p1/pP6/8/8/P1PPPPPP/RNBQKBNR w KQkq a6 4 2", fen)