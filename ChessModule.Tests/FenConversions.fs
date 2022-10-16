namespace FenConversions

open Xunit
open Chess
open ChessTest.Helpers.Functions

module Inverses =
    [<Fact>]
    let ``Starting Fen converts with inverse`` () =
        fromFenToFenIsInverse "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
        |> Assert.True

    [<Fact>]
    let ``Empty board`` () =
        fromFenToFenIsInverse "4k3/8/8/8/8/8/8/4K3 w KQkq - 0 0"
        |> Assert.True

    [<Fact>]
    let ``Asymetric board`` () =
        fromFenToFenIsInverse "4k3/8/8/nNnN4/8/8/PPPP4/4K3 w KQkq - 0 0"
        |> Assert.True

module SetEnpassantSquare =
    [<Fact>]
    let ``Sets Enpassant square for White`` () =
        let game = GameState.Create.newGame ()
        MoveParser.parse game.playerTurn game.board "a4"
        |> fun move -> GameState.Update.makeMove move game
        |> GameState.toFen
        |> fun fen ->
            Assert.Equal("rnbqkbnr/pppppppp/8/8/P7/8/1PPPPPPP/RNBQKBNR b KQkq a3 1 1", fen)

    [<Fact>]
    let ``Sets Enpassant square for Black`` () =
        let game = 
            "rnbqkbnr/p1pppppp/6p1/1P6/8/8/P1PPPPPP/RNBQKBNR b KQkq - 3 2"
            |> GameState.Create.fromFen
        MoveParser.parse game.playerTurn game.board "a5"
        |> fun move -> GameState.Update.makeMove move game
        |> GameState.toFen
        |> fun fen ->
            Assert.Equal("rnbqkbnr/2pppppp/6p1/pP6/8/8/P1PPPPPP/RNBQKBNR w KQkq a6 4 2", fen)