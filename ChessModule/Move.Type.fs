namespace Chess

open Checkerboard

[<Struct>]
type normalMove = {startingCoords: coordinates; destinationCoords: coordinates}

type move = 
    | NormalMove of normalMove
    | Castling of (side * colour)
    | Promotion of (normalMove * pieceType)
    | EnPassant of normalMove