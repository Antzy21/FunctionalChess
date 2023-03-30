namespace Chess

open Checkerboard
open FSharp.Extensions

type normalMove = move<piece, sbyte>

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
        let getEnPassantCoordinates (move: normalMove) : coordinates<sbyte> option = 
            if getMovedPieceType move = Pawn && List.contains (Move.getShift move) [(0y,2y); (0y,-2y)] then
                let startingSquare = fst move
                let shift = 
                    match getMovedPieceColour move with                
                    | White -> (0y,1y)
                    | Black -> (0y,-1y)
                Some (Coordinates.getAfterShift shift startingSquare.coordinates)
            else
                None