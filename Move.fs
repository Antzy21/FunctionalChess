namespace Chess

open Checkerboard
open FSharp.Extensions

type normalMove = move<piece>

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

    module Enpassant =
        let getEnPassantCoordinates (move: normalMove) : coordinates option = 
            if getMovedPieceType move = Pawn && List.contains (Move.getShift move) [(0,2); (0,-2)] then
                let startingSquare = fst move
                let shift = 
                    match getMovedPieceColour move with                
                    | White -> (0,1)
                    | Black -> (0,-1)
                Some (Coordinates.afterShift shift startingSquare.coordinates)
            else
                None