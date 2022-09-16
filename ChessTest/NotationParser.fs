namespace NotationParser

open Xunit
open Chess
open ChessTest.Helpers.Data

module ParseLong =
    [<Fact>]
    let ``e4`` () =
        let result = NotationParser.parseFullNotation "Pe2 -> e4"
        Assert.Equal(Moves.WhiteMove1, NormalMove result)
        
    [<Fact>]
    let ``d5`` () =
        let result = NotationParser.parseFullNotation "pd7 -> d5"
        Assert.Equal(Moves.BlackMove1, NormalMove result)
        
    [<Fact>]
    let ``xd5`` () =
        let result = NotationParser.parseFullNotation "Pe4 -> xpd5"
        Assert.Equal(Moves.WhiteMove2, NormalMove result)
        
    [<Fact>]
    let ``nf6`` () =
        let result = NotationParser.parseFullNotation "ng8 -> f6"
        Assert.Equal(Moves.BlackMove2, NormalMove result)
        
module ParseShort =
    [<Fact>]
    let ``e4`` () =
        let board = Board.Create.starting ()
        let result = NotationParser.parse White board "e4"
        Assert.Equal(Moves.WhiteMove1, result)
        
module Castling =
    [<Fact>]
    let ``White Kingside`` () =
        let gs = GameState.Create.fromFen Fens.Castling.PreWhite
        let result = NotationParser.parse gs.playerTurn gs.board "0-0"
        Assert.Equal(Castling (Kingside, White), result)
    [<Fact>]
    let ``White Queenside`` () =
        let gs = GameState.Create.fromFen Fens.Castling.PreWhite
        let result = NotationParser.parse gs.playerTurn gs.board "0-0-0"
        Assert.Equal(Castling (Queenside, White), result)
    [<Fact>]
    let ``Black Kingside`` () =
        let gs = GameState.Create.fromFen Fens.Castling.PreBlack
        let result = NotationParser.parse gs.playerTurn gs.board "0-0"
        Assert.Equal(Castling (Kingside, Black), result)
    [<Fact>]
    let ``Black Queenside`` () =
        let gs = GameState.Create.fromFen Fens.Castling.PreBlack
        let result = NotationParser.parse gs.playerTurn gs.board "0-0-0"
        Assert.Equal(Castling (Queenside, Black), result)

module Enpassant =
    [<Fact>]
    let ``White`` () =
        let gs = GameState.Create.fromFen Fens.Enpassant.PreWhite
        let result = NotationParser.parse gs.playerTurn gs.board "Pb5 -> xpa6 e.p."
        Assert.Equal(Moves.EnPassant.White, result)
    
    [<Fact>]
    let ``Black`` () =
        let gs = GameState.Create.fromFen Fens.Enpassant.PreBlack
        let result = NotationParser.parse gs.playerTurn gs.board "pb4 -> xPa3 e.p."
        Assert.Equal(Moves.EnPassant.Black, result)

module Promotion =
    [<Fact>]
    let ``White`` () =
        let gs = GameState.Create.fromFen Fens.Promotion.PreWhite1
        let result = NotationParser.parse gs.playerTurn gs.board "Pa7 -> a8 = Q"
        Assert.Equal(Moves.Promotion.White1, result)
    
    [<Fact>]
    let ``White taking`` () =
        let gs = GameState.Create.fromFen Fens.Promotion.PreWhite2
        let result = NotationParser.parse gs.playerTurn gs.board "Pa7 -> xnb8 = R"
        Assert.Equal(Moves.Promotion.White2, result)