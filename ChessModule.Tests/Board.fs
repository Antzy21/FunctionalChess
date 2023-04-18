namespace Board

open Xunit
open Checkerboard
open Chess
open ChessTest.Helpers.Data

module ContainsPiece =

    [<Fact>]
    let ``Piece exists check`` () =
        let board : board = 
            (Games.ExampleGame.getWhite1 ()).gameState.board
            |> Board.updateSquare (2,0) Pieces.WhiteBishop
        Assert.True(Board.containsPiece (2,0) board)

    [<Fact>]
    let ``Piece does not exist check`` () =
        let result = Board.containsPiece (0,2) (Games.ExampleGame.getWhite1 ()).gameState.board
        Assert.False(result)
