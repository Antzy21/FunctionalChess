namespace Moves

open Xunit
open Chess
open ChessTest.Helpers.Functions
open ChessTest.Helpers.Data

module Normal =
    [<Fact>]
    let ``e4`` () =
        let newGame = Game.Create.newGame ()
        let result = Game.Update.makeMove Moves.e4 newGame
        Assert.Equal(Games.ExampleGame.getWhite1 (), result)
        
    [<Fact>]
    let ``d5`` () =
        let result = Game.Update.makeMove Moves.d5 (Games.ExampleGame.getWhite1 ())
        Assert.Equal(Games.ExampleGame.getBlack1 (), result)

    [<Fact>]
    let ``xd5`` () =
        let result = Game.Update.makeMove Moves.xd5 (Games.ExampleGame.getBlack1 ())
        Assert.Equal(Games.ExampleGame.getWhite2 (), result)
        
    [<Fact>]
    let ``nf6`` () =
        let result = Game.Update.makeMove Moves.Nf6 (Games.ExampleGame.getWhite2 ())
        Assert.Equal(Games.ExampleGame.getBlack2 (), result)

module Castling =
    [<Fact>]
    let ``White Kingside`` () =
        let result = Game.Update.makeMove (Castling (Kingside, White)) (Games.Castling.PreWhite ())
        Assert.Equal(Games.Castling.PostWhiteKing (), result)

    [<Fact>]
    let ``White Queenside`` () =
        let result = Game.Update.makeMove (Castling (Queenside, White)) (Games.Castling.PreWhite ())
        Assert.Equal(Games.Castling.PostWhiteQueen (), result)

    [<Fact>]
    let ``Black Kingside`` () =
        let result = Game.Update.makeMove (Castling (Kingside, Black)) (Games.Castling.PreBlack ())
        Assert.Equal(Games.Castling.PostBlackKing (), result)

    [<Fact>]
    let ``Black Queenside`` () =
        let result = Game.Update.makeMove (Castling (Queenside, Black)) (Games.Castling.PreBlack ())
        Assert.Equal(Games.Castling.PostBlackQueen (), result)
        
module Enpassant =
    [<Fact>]
    let ``White`` () =
        let result = Game.Update.makeMove Moves.EnPassant.PostWhite (Games.Enpassant.PreWhite ())
        Assert.Equal(Games.Enpassant.PostWhite (), result)
    
    [<Fact>]
    let ``Black`` () =
        let result = Game.Update.makeMove Moves.EnPassant.PostBlack (Games.Enpassant.PreBlack ())
        Assert.Equal(Games.Enpassant.PostBlack (), result)
     
module Promotion =
    [<Fact>]
    let ``White`` () =
        let result = Game.Update.makeMove Moves.Promotion.White1 (Games.Promotion.PreWhite1 ())
        Assert.Equal(Games.Promotion.PostWhite1 (), result)
    
    [<Fact>]
    let ``White taking`` () =
        let result = Game.Update.makeMove Moves.Promotion.White2 (Games.Promotion.PreWhite2 ())
        Assert.Equal(Games.Promotion.PostWhite2 (), result)
     
module MovesFromPositions =
    [<Fact>]
    let ``Opening moves for White`` () =
        let moves = 
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
            |> GetPossibleMoves.all
        Assert.Equal<string list>([
            "Ph2 -> h4";
            "Ph2 -> h3";
            "Pg2 -> g4";
            "Pg2 -> g3";
            "Pf2 -> f4";
            "Pf2 -> f3";
            "Pe2 -> e4";
            "Pe2 -> e3";
            "Pd2 -> d4";
            "Pd2 -> d3";
            "Pc2 -> c4";
            "Pc2 -> c3";
            "Pb2 -> b4";
            "Pb2 -> b3";
            "Pa2 -> a4";
            "Pa2 -> a3";
            "Ng1 -> h3";
            "Ng1 -> f3";
            "Nb1 -> c3";
            "Nb1 -> a3";
        ], moves)
    
    [<Fact>]
    let ``Move options for Black`` () =
        let moves = 
            "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
            |> GetPossibleMoves.all
        Assert.Equal<string list>([
            "ng8 -> h6";
            "ng8 -> f6";
            "qd8 -> c7";
            "qd8 -> b6";
            "qd8 -> a5";
            "nb8 -> c6";
            "nb8 -> a6";
            "ph7 -> h6";
            "ph7 -> h5";
            "pg7 -> g6";
            "pg7 -> g5";
            "pf7 -> f6";
            "pf7 -> f5";
            "pe7 -> e6";
            "pe7 -> e5";
            "pd7 -> d6";
            "pd7 -> d5";
            "pb7 -> b6";
            "pb7 -> b5";
            "pa7 -> a6";
            "pa7 -> a5";
            "pc5 -> c4";
        ], moves)

    [<Fact>]
    let ``pd5 -> xe4`` () =
        let moves = 
            "rnbqkbnr/pp1ppppp/8/3p4/3PP3/8/PPP2PPP/RNBQKBNR b KQkq - 1 2"
            |> GetPossibleMoves.fromSquare "d5"
        Assert.StrictEqual<string list>([
            "pd5 -> xPe4";
        ], moves)
        
    [<Fact>]
    let ``King can't move into check`` () =
        let moves = 
            "2k5/8/8/8/8/1pb5/8/K7 w - - 1 2"
            |> GetPossibleMoves.all
        Assert.StrictEqual<string list>([
            "Ka1 -> b1"
        ], moves)
        