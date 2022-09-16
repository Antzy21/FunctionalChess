namespace Moves

open Xunit
open Chess
open ChessTest.Helpers.Functions
open ChessTest.Helpers.Data

module Normal =
    [<Fact>]
    let ``e4`` () =
        let newGame = GameState.Create.newGame () |> GameState.toFen
        let result = UpdateWithMove.applyMove newGame Moves.WhiteMove1
        Assert.Equal(Fens.ExampleGame.White1, result)
        
    [<Fact>]
    let ``d5`` () =
        let result = UpdateWithMove.applyMove Fens.ExampleGame.White1 Moves.BlackMove1
        Assert.Equal(Fens.ExampleGame.Black1, result)

    [<Fact>]
    let ``xd5`` () =
        let result = UpdateWithMove.applyMove Fens.ExampleGame.Black1 Moves.WhiteMove2
        Assert.Equal(Fens.ExampleGame.White2, result)
        
    [<Fact>]
    let ``nf6`` () =
        let result = UpdateWithMove.applyMove Fens.ExampleGame.White2 Moves.BlackMove2
        Assert.Equal(Fens.ExampleGame.Black2, result)

module UndoNormal =
    [<Fact>]
    let ``e4`` () =
        let newGame = GameState.Create.newGame () |> GameState.toFen
        let result = UpdateWithMove.undoMove Fens.ExampleGame.White1 Moves.WhiteMove1
        Assert.Equal(newGame , result)
        
    [<Fact>]
    let ``d5`` () =
        let result = UpdateWithMove.undoMove Fens.ExampleGame.Black1 Moves.BlackMove1
        Assert.Equal(Fens.ExampleGame.White1, result)

    [<Fact>]
    let ``xd5`` () =
        let result = UpdateWithMove.undoMove Fens.ExampleGame.White2 Moves.WhiteMove2
        Assert.Equal(Fens.ExampleGame.Black1, result)
        
    [<Fact>]
    let ``nf6`` () =
        let result = UpdateWithMove.undoMove Fens.ExampleGame.Black2 Moves.BlackMove2
        Assert.Equal(Fens.ExampleGame.White2, result)

module Castling =
    [<Fact>]
    let ``White Kingside`` () =
        let result = UpdateWithMove.applyMove Fens.Castling.PreWhite (Castling (Kingside, White))
        Assert.Equal(Fens.Castling.PostWhiteKing, result)

    [<Fact>]
    let ``White Queenside`` () =
        let result = UpdateWithMove.applyMove Fens.Castling.PreWhite (Castling (Queenside, White))
        Assert.Equal(Fens.Castling.PostWhiteQueen, result)

    [<Fact>]
    let ``Black Kingside`` () =
        let result = UpdateWithMove.applyMove Fens.Castling.PreBlack (Castling (Kingside, Black))
        Assert.Equal(Fens.Castling.PostBlackKing, result)

    [<Fact>]
    let ``Black Queenside`` () =
        let result = UpdateWithMove.applyMove Fens.Castling.PreBlack (Castling (Queenside, Black))
        Assert.Equal(Fens.Castling.PostBlackQueen, result)
        
module UndoCastling =
    [<Fact>]
    let ``White Kingside`` () =
        let result = UpdateWithMove.undoMove Fens.Castling.PostWhiteKing (Castling (Kingside, White))
        Assert.Equal(Fens.Castling.PreWhite, result)

    [<Fact>]
    let ``White Queenside`` () =
        let result = UpdateWithMove.undoMove Fens.Castling.PostWhiteQueen (Castling (Queenside, White))
        Assert.Equal(Fens.Castling.PreWhite, result)

    [<Fact>]
    let ``Black Kingside`` () =
        let result = UpdateWithMove.undoMove Fens.Castling.PostBlackKing (Castling (Kingside, Black))
        Assert.Equal(Fens.Castling.PreBlack, result)

    [<Fact>]
    let ``Black Queenside`` () =
        let result = UpdateWithMove.undoMove Fens.Castling.PostBlackQueen (Castling (Queenside, Black))
        Assert.Equal(Fens.Castling.PreBlack, result)

module Enpassant =
    [<Fact>]
    let ``White`` () =
        let result = UpdateWithMove.applyMove Fens.Enpassant.PreWhite Moves.EnPassant.White
        Assert.Equal(Fens.Enpassant.PostWhite, result)
    
    [<Fact>]
    let ``Black`` () =
        let result = UpdateWithMove.applyMove Fens.Enpassant.PreBlack Moves.EnPassant.Black
        Assert.Equal(Fens.Enpassant.PostBlack, result)
        
module UndoEnpassant =
    [<Fact>]
    let ``White`` () =
        let result = UpdateWithMove.undoMove Fens.Enpassant.PostWhite Moves.EnPassant.White
        Assert.Equal(Fens.Enpassant.PreWhite, result)
    
    [<Fact>]
    let ``Black`` () =
        let result = UpdateWithMove.undoMove Fens.Enpassant.PostBlack Moves.EnPassant.Black
        Assert.Equal(Fens.Enpassant.PreBlack, result)

module Promotion =
    [<Fact>]
    let ``White`` () =
        let result = UpdateWithMove.applyMove Fens.Promotion.PreWhite1 Moves.Promotion.White1
        Assert.Equal(Fens.Promotion.PostWhite1, result)
    
    [<Fact>]
    let ``White taking`` () =
        let result = UpdateWithMove.applyMove Fens.Promotion.PreWhite2 Moves.Promotion.White2
        Assert.Equal(Fens.Promotion.PostWhite2, result)
        
module UndoPromotion =
    [<Fact>]
    let ``White`` () =
        let result = UpdateWithMove.undoMove Fens.Promotion.PostWhite1 Moves.Promotion.White1
        Assert.Equal(Fens.Promotion.PreWhite1, result)
    
    [<Fact>]
    let ``White taking`` () =
        let result = UpdateWithMove.undoMove Fens.Promotion.PostWhite2 Moves.Promotion.White2
        Assert.Equal(Fens.Promotion.PreWhite2, result)

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