namespace MoveParser

open Xunit
open Chess
open ChessTest.Helpers.Data

module ParseFull =
    [<Fact>]
    let ``e4`` () =
        let result = MoveParser.FullNotation.parse "Pe2 -> e4"
        Assert.Equal(Moves.WhiteMove1, NormalMove result)
        
    [<Fact>]
    let ``d5`` () =
        let result = MoveParser.FullNotation.parse "pd7 -> d5"
        Assert.Equal(Moves.BlackMove1, NormalMove result)
        
    [<Fact>]
    let ``xd5`` () =
        let result = MoveParser.FullNotation.parse "Pe4 -> xpd5"
        Assert.Equal(Moves.WhiteMove2, NormalMove result)
        
    [<Fact>]
    let ``nf6`` () =
        let result = MoveParser.FullNotation.parse "ng8 -> f6"
        Assert.Equal(Moves.BlackMove2, NormalMove result)
        
module ParseAlgebraic =
    [<Fact>]
    let ``e4`` () =
        let board = Board.Create.starting ()
        let result = MoveParser.AlgebraicNotation.parse White board "e4"
        Assert.Equal(Moves.WhiteMove1, result)
        
module Castling =
    [<Fact>]
    let ``White Kingside`` () =
        let gs = GameState.Create.fromFen Fens.Castling.PreWhite
        let result = MoveParser.parse gs.playerTurn gs.board "0-0"
        Assert.Equal(Castling (Kingside, White), result)
    [<Fact>]
    let ``White Queenside`` () =
        let gs = GameState.Create.fromFen Fens.Castling.PreWhite
        let result = MoveParser.parse gs.playerTurn gs.board "0-0-0"
        Assert.Equal(Castling (Queenside, White), result)
    [<Fact>]
    let ``Black Kingside`` () =
        let gs = GameState.Create.fromFen Fens.Castling.PreBlack
        let result = MoveParser.parse gs.playerTurn gs.board "0-0"
        Assert.Equal(Castling (Kingside, Black), result)
    [<Fact>]
    let ``Black Queenside`` () =
        let gs = GameState.Create.fromFen Fens.Castling.PreBlack
        let result = MoveParser.parse gs.playerTurn gs.board "0-0-0"
        Assert.Equal(Castling (Queenside, Black), result)

module Enpassant =
    [<Fact>]
    let ``White`` () =
        let gs = GameState.Create.fromFen Fens.Enpassant.PreWhite
        let result = MoveParser.parse gs.playerTurn gs.board "bxa6"
        Assert.Equal(Moves.EnPassant.White, result)
    
    [<Fact>]
    let ``Black`` () =
        let gs = GameState.Create.fromFen Fens.Enpassant.PreBlack
        let result = MoveParser.parse gs.playerTurn gs.board "bxa3"
        Assert.Equal(Moves.EnPassant.Black, result)

module Promotion =
    [<Fact>]
    let ``White`` () =
        let gs = GameState.Create.fromFen Fens.Promotion.PreWhite1
        let result = MoveParser.parse gs.playerTurn gs.board "a8=Q"
        Assert.Equal(Moves.Promotion.White1, result)
    
    [<Fact>]
    let ``White taking`` () =
        let gs = GameState.Create.fromFen Fens.Promotion.PreWhite2
        let result = MoveParser.parse gs.playerTurn gs.board "axb8=R"
        Assert.Equal(Moves.Promotion.White2, result)