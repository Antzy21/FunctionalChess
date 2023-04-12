namespace Chess

open Checkerboard
open FSharp.Extensions

type normalMove = move<piece, int>

type move = 
    | NormalMove of normalMove
    | Castling of (side * colour)
    | Promotion of (normalMove * pieceType)
    | EnPassant of normalMove

module Move =

    let getMovedPieceType (move: normalMove) : pieceType =
        Move.getMovedPiece move
        |> fun piece -> piece.pieceType
    let getMovedPieceColour (move: normalMove) : colour =
        Move.getMovedPiece move
        |> fun piece -> piece.colour
    /// Get the difference between the starting and destination of the move.
    let getShift (move: normalMove) : int * int =
        Coordinates.getShiftBetweenCoordinates (fst move).coordinates (snd move).coordinates

    module Enpassant =
        /// Gets the optional coordinates that a pawn could be taken through an "en passant" move, that only comes by the previous move being a pawn moving two squares.
        let getEnPassantCoordinates (board: board<piece, int>) (move: normalMove) : coordinates option = 
            let pawnMovedTwoSquares = List.contains (getShift move) [(0,2); (0,-2)]
            let start = fst move
            let moveWasPawn = 
                start
                |> Square.getPieceType
                |> (=) (Some Pawn)
            if moveWasPawn && pawnMovedTwoSquares then
                let shift = 
                    match getMovedPieceColour move with                
                    | White -> (0,1)
                    | Black -> (0,-1)
                Some (Coordinates.getAfterShift shift start.coordinates)
            else
                None