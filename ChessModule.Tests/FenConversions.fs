namespace FenConversions

open Xunit
open Chess
open ChessTest.Helpers.Functions
open Checkerboard
open FSharp.Extensions

module FromFen =
    [<Fact>]
    let ``Board from Fen contains correctly placed pieces`` () =
        let board = BoardParser.fromFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
        let pieceAt_0_0 =
            Coordinates.construct 0 0 |> Result.failOnError
            |> Board.getSquareFromCoordinates board
            |> Option.map (fun p -> p.pieceType)
        Assert.Equal(Some Rook, pieceAt_0_0)
        let pieceAt_4_0 =
            Coordinates.construct 4 0 |> Result.failOnError
            |> Board.getSquareFromCoordinates board
            |> Option.map (fun p -> p.pieceType)
        Assert.Equal(Some King, pieceAt_4_0)
        let pieceAt_0_2 =
            Coordinates.construct 0 2 |> Result.failOnError
            |> Board.getSquareFromCoordinates board
        Assert.Equal(None, pieceAt_0_2)
        let colour_0_0 =
            Coordinates.construct 0 0 |> Result.failOnError
            |> Board.getSquareFromCoordinates board
            |> Option.map (fun p -> p.colour)
        Assert.Equal(Some White, colour_0_0)

module Inverses =
    [<Fact>]
    let ``Starting Fen converts with inverse`` () =
        fromFenToFenIsInverse "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"

    [<Fact>]
    let ``Empty board`` () =
        fromFenToFenIsInverse "4k3/8/8/8/8/8/8/4K3 w KQkq - 0 0"

    [<Fact>]
    let ``Asymetric board`` () =
        fromFenToFenIsInverse "4k3/8/8/nNnN4/8/8/PPPP4/4K3 w KQkq - 0 0"

module SetEnpassantSquare =
    [<Fact>]
    let ``Sets Enpassant square for White`` () =
        let game = GameState.Create.newGame ()
        MoveParser.tryParse game.playerTurn game.board "a4"
        |> Result.failOnError
        |> fun move -> GameState.Update.makeMove move game
        |> GameState.toFen
        |> fun fen ->
            Assert.Equal("rnbqkbnr/pppppppp/8/8/P7/8/1PPPPPPP/RNBQKBNR b KQkq a3 1 1", fen)

    [<Fact>]
    let ``Sets Enpassant square for Black`` () =
        let game = 
            "rnbqkbnr/p1pppppp/6p1/1P6/8/8/P1PPPPPP/RNBQKBNR b KQkq - 3 2"
            |> GameState.Create.fromFen
        MoveParser.tryParse game.playerTurn game.board "a5"
        |> Result.failOnError
        |> fun move -> GameState.Update.makeMove move game
        |> GameState.toFen
        |> fun fen ->
            Assert.Equal("rnbqkbnr/2pppppp/6p1/pP6/8/8/P1PPPPPP/RNBQKBNR w KQkq a6 4 2", fen)