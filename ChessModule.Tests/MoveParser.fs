namespace MoveParser

open Xunit
open Chess
open ChessTest.Helpers.Data
open FSharp.Extensions

module ParseFull =
    [<Fact>]
    let ``e4`` () =
        let result = 
            MoveParser.FullNotation.tryParse "Pe2 -> e4"
            |> Result.failOnError
        Assert.Equal(Moves.e4, NormalMove result)
        
    [<Fact>]
    let ``d5`` () =
        let result = 
            MoveParser.FullNotation.tryParse "pd7 -> d5"
            |> Result.failOnError
        Assert.Equal(Moves.d5, NormalMove result)
        
    [<Fact>]
    let ``xd5`` () =
        let result = 
            MoveParser.FullNotation.tryParse "Pe4 -> xpd5"
            |> Result.failOnError
        Assert.Equal(Moves.xd5, NormalMove result)
        
    [<Fact>]
    let ``nf6`` () =
        let result = 
            MoveParser.FullNotation.tryParse "ng8 -> f6"
            |> Result.failOnError
        Assert.Equal(Moves.Nf6, NormalMove result)
        
module ParseAlgebraic =
    [<Fact>]
    let ``e4`` () =
        let board = Board.createStarting ()
        let result = 
            MoveParser.AlgebraicNotation.tryParse White board "e4"
            |> Result.failOnError
        Assert.Equal(Moves.e4, result)
        
module Castling =
    [<Fact>]
    let ``White Kingside`` () =
        let game = Games.Castling.PreWhite ()
        let result = 
            MoveParser.tryParse game.gameState.playerTurn game.gameState.board "0-0"
            |> Result.failOnError
        Assert.Equal(Castling (Kingside, White), result)
    [<Fact>]
    let ``White Queenside`` () =
        let game = Games.Castling.PreWhite ()
        let result = 
            MoveParser.tryParse game.gameState.playerTurn game.gameState.board "0-0-0"
            |> Result.failOnError
        Assert.Equal(Castling (Queenside, White), result)
    [<Fact>]
    let ``Black Kingside`` () =
        let game = Games.Castling.PreBlack ()
        let result = 
            MoveParser.tryParse game.gameState.playerTurn game.gameState.board "0-0"
            |> Result.failOnError
        Assert.Equal(Castling (Kingside, Black), result)
    [<Fact>]
    let ``Black Queenside`` () =
        let game = Games.Castling.PreBlack ()
        let result = 
            MoveParser.tryParse game.gameState.playerTurn game.gameState.board "0-0-0"
            |> Result.failOnError
        Assert.Equal(Castling (Queenside, Black), result)

module Enpassant =
    [<Fact>]
    let ``White`` () =
        let game = Games.Enpassant.PreWhite ()
        let result = 
            MoveParser.tryParse game.gameState.playerTurn game.gameState.board "bxa6"
            |> Result.failOnError
        Assert.Equal(Moves.EnPassant.PostWhite, result)
    
    [<Fact>]
    let ``Black`` () =
        let game = Games.Enpassant.PreBlack ()
        let result = 
            MoveParser.tryParse game.gameState.playerTurn game.gameState.board "bxa3"
            |> Result.failOnError
        Assert.Equal(Moves.EnPassant.PostBlack, result)

module Promotion =
    [<Fact>]
    let ``White`` () =
        let game = Games.Promotion.PreWhite1 ()
        let result = 
           MoveParser.tryParse game.gameState.playerTurn game.gameState.board "a8=Q"
            |> Result.failOnError
        Assert.Equal(Moves.Promotion.White1, result)
    
    [<Fact>]
    let ``White taking`` () =
        let game = Games.Promotion.PreWhite2 ()
        let result = 
            MoveParser.tryParse game.gameState.playerTurn game.gameState.board "axb8=R"
            |> Result.failOnError
        Assert.Equal(Moves.Promotion.White2, result)

module pgnTests =
    [<Fact>]
    let ``Knights share square to move to`` () =
        let gamePgn = Game.pgn (Games.Special.knightsSharingMoveSquare ())
        let expected = "1.e4 e5 2.Nc3 Nf6 3.Nge2"
        Assert.Equal(expected, gamePgn)