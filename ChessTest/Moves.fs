namespace Moves

open Xunit
open Chess
open ChessTest.Helpers.Functions
open ChessTest.Helpers.Data

module Normal =
    [<Fact>]
    let ``e4`` () =
        let newGame = Game.Create.newGame ()
        let result = UpdateWithMove.makeMove Moves.WhiteMove1 newGame
        Assert.Equal(Games.ExampleGame.White1, result)
        
    [<Fact>]
    let ``d5`` () =
        let result = UpdateWithMove.makeMove Moves.BlackMove1 Games.ExampleGame.White1
        Assert.Equal(Games.ExampleGame.Black1, result)

    [<Fact>]
    let ``xd5`` () =
        let result = UpdateWithMove.makeMove Moves.WhiteMove2 Games.ExampleGame.Black1
        Assert.Equal(Games.ExampleGame.White2, result)
        
    [<Fact>]
    let ``nf6`` () =
        let result = UpdateWithMove.makeMove Moves.BlackMove2 Games.ExampleGame.White2
        Assert.Equal(Games.ExampleGame.Black2, result)

module UndoNormal =
    [<Fact>]
    let ``e4`` () =
        let newGame = Game.Create.newGame ()
        let result = UpdateWithMove.undoMove Games.ExampleGame.White1
        Assert.Equal(newGame, result)
        
    [<Fact>]
    let ``d5`` () =
        let result = UpdateWithMove.undoMove Games.ExampleGame.Black1
        Assert.Equal(Games.ExampleGame.White1, result)

    [<Fact>]
    let ``xd5`` () =
        let result = UpdateWithMove.undoMove Games.ExampleGame.White2
        Assert.Equal(Games.ExampleGame.Black1, result)
        
    [<Fact>]
    let ``nf6`` () =
        let result = UpdateWithMove.undoMove Games.ExampleGame.Black2
        Assert.Equal(Games.ExampleGame.White2, result)

module Castling =
    [<Fact>]
    let ``White Kingside`` () =
        let result = UpdateWithMove.makeMove (Castling (Kingside, White)) Games.Castling.PreWhite
        Assert.Equal(Games.Castling.PostWhiteKing, result)

    [<Fact>]
    let ``White Queenside`` () =
        let result = UpdateWithMove.makeMove (Castling (Queenside, White)) Games.Castling.PreWhite
        Assert.Equal(Games.Castling.PostWhiteQueen, result)

    [<Fact>]
    let ``Black Kingside`` () =
        let result = UpdateWithMove.makeMove (Castling (Kingside, Black)) Games.Castling.PreBlack
        Assert.Equal(Games.Castling.PostBlackKing, result)

    [<Fact>]
    let ``Black Queenside`` () =
        let result = Game.Update.makeMove (Castling (Queenside, Black)) Games.Castling.PreBlack
        Assert.Equal(Games.Castling.PostBlackQueen, result)
        
module UndoCastling =
    [<Fact>]
    let ``White Kingside`` () =
        let result = UpdateWithMove.undoMove Games.Castling.PostWhiteKing
        Assert.Equal(Games.Castling.PreWhite, result)

    [<Fact>]
    let ``White Queenside`` () =
        let result = UpdateWithMove.undoMove Games.Castling.PostWhiteQueen
        Assert.Equal(Games.Castling.PreWhite, result)

    [<Fact>]
    let ``Black Kingside`` () =
        let result = UpdateWithMove.undoMove Games.Castling.PostBlackKing
        Assert.Equal(Games.Castling.PreBlack, result)

    [<Fact>]
    let ``Black Queenside`` () =
        let result = UpdateWithMove.undoMove Games.Castling.PostBlackQueen
        Assert.Equal(Games.Castling.PreBlack, result)

module Enpassant =
    [<Fact>]
    let ``White`` () =
        let result = UpdateWithMove.makeMove Moves.EnPassant.PostWhite Games.Enpassant.PreWhite
        Assert.Equal(Games.Enpassant.PostWhite, result)
    
    [<Fact>]
    let ``Black`` () =
        let result = UpdateWithMove.makeMove Moves.EnPassant.PostBlack Games.Enpassant.PreBlack
        Assert.Equal(Games.Enpassant.PostBlack, result)
        
module UndoEnpassant =
    [<Fact>]
    let ``White`` () =
        let result = UpdateWithMove.undoMove Games.Enpassant.PostWhite
        Assert.Equal(Games.Enpassant.PreWhite, result)
    
    [<Fact>]
    let ``Black`` () =
        let result = UpdateWithMove.undoMove Games.Enpassant.PostBlack
        Assert.Equal(Games.Enpassant.PreBlack, result)

module Promotion =
    [<Fact>]
    let ``White`` () =
        let result = UpdateWithMove.makeMove Moves.Promotion.White1 Games.Promotion.PreWhite1
        Assert.Equal(Games.Promotion.PostWhite1, result)
    
    [<Fact>]
    let ``White taking`` () =
        let result = UpdateWithMove.makeMove Moves.Promotion.White2 Games.Promotion.PreWhite2
        Assert.Equal(Games.Promotion.PostWhite2, result)
        
module UndoPromotion =
    [<Fact>]
    let ``White`` () =
        let result = UpdateWithMove.undoMove Games.Promotion.PostWhite1
        Assert.Equal(Games.Promotion.PreWhite1, result)
    
    [<Fact>]
    let ``White taking`` () =
        let result = UpdateWithMove.undoMove Games.Promotion.PostWhite2
        Assert.Equal(Games.Promotion.PreWhite2, result)

module MovesFromPositions =
    [<Fact>]
    let ``Opening moves for White`` () =
        let moves = 
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
            |> GetPossibleMoves.all
        Assert.StrictEqual<string list>([
            "Pa2 -> a3";
            "Pa2 -> a4";
            "Nb1 -> c3";
            "Nb1 -> a3";
            "Pb2 -> b3";
            "Pb2 -> b4";
            "Pc2 -> c3";
            "Pc2 -> c4";
            "Pd2 -> d3";
            "Pd2 -> d4";
            "Pe2 -> e3";
            "Pe2 -> e4";
            "Pf2 -> f3";
            "Pf2 -> f4";
            "Ng1 -> h3";
            "Ng1 -> f3";
            "Pg2 -> g3";
            "Pg2 -> g4";
            "Ph2 -> h3";
            "Ph2 -> h4";
        ], moves)
    
    [<Fact>]
    let ``Move options for Black`` () =
        let moves = 
            "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
            |> GetPossibleMoves.all
        Assert.StrictEqual<string list>([
            "pa7 -> a6";
            "pa7 -> a5";
            "pb7 -> b6";
            "pb7 -> b5";
            "nb8 -> c6";
            "nb8 -> a6";
            "pc5 -> c4";
            "pd7 -> d6";
            "pd7 -> d5";
            "qd8 -> c7";
            "qd8 -> b6";
            "qd8 -> a5";
            "pe7 -> e6";
            "pe7 -> e5";
            "pf7 -> f6";
            "pf7 -> f5";
            "pg7 -> g6";
            "pg7 -> g5";
            "ng8 -> h6";
            "ng8 -> f6";
            "ph7 -> h6";
            "ph7 -> h5";
        ], moves)

    [<Fact>]
    let ``pd5 -> xe4`` () =
        let moves = 
            "rnbqkbnr/pp1ppppp/8/3p4/3PP3/8/PPP2PPP/RNBQKBNR b KQkq - 1 2"
            |> GetPossibleMoves.fromSquare "d5"
        Assert.StrictEqual<string list>([
            "pd5 -> xPe4";
        ], moves)