namespace Board

open Xunit
open Checkerboard
open Chess
open ChessTest.Helpers.Data
open ChessTest.Helpers.Functions

module Construct =
    
    [<Fact>]
    let ``Starting position is correct`` () =
        let board = Board.constructStarting ()
        let fen = BoardParser.toFen board
        Assert.Equal("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR", fen)


module ContainsPiece =

    [<Fact>]
    let ``Piece exists`` () =
        let board : board = 
            (Games.ExampleGame.getWhite1 ()).gameState.board
            |> Board.Update.updateSquare Pieces.WhiteBishop (coordCntr 2 0)
        Assert.True(BitMap.isOnAtCoordinates (coordCntr 2 0) board.pieceMap)

    [<Fact>]
    let ``Piece does not exist`` () =
        let result = BitMap.isOnAtCoordinates (coordCntr 0 2) (Games.ExampleGame.getWhite1 ()).gameState.board.pieceMap
        Assert.False(result)
