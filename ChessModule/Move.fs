namespace Chess

open Checkerboard
open FSharp.Extensions

[<Struct>]
type normalMove = {startingCoords: coordinates; destinationCoords: coordinates}

type move = 
    | NormalMove of normalMove
    | Castling of (side * colour)
    | Promotion of (normalMove * pieceType)
    | EnPassant of normalMove

module Move =

    let getShift (move: normalMove) : struct (int*int) =
        Coordinates.getShiftBetweenCoordinates move.startingCoords move.destinationCoords

    module Enpassant =
        /// Gets the optional coordinates that a pawn could be taken through an "en passant" move, that only comes by the previous move being a pawn moving two squares.
        let getEnPassantCoordinates (board: board) (move: normalMove) : coordinates option = 
            let pawnMovedTwoSquares = List.contains (getShift move) [(0,2); (0,-2)]
            let start = Board.getSquareFromCoordinates board move.startingCoords
            let moveWasPawn = 
                start
                |> Square.BitMap.containsPieceOfType Pawn
            if moveWasPawn && pawnMovedTwoSquares then
                let shift = 
                    match Square.getPieceColour start with                
                    | Some White -> struct (0,1)
                    | Some Black -> struct (0,-1)
                    | None -> failwith "No piece"
                Some (Coordinates.getAfterShift shift move.startingCoords)
            else
                None
