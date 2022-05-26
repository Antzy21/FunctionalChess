namespace Chess

open Checkerboard

type square = square<piece>

module Square =
    let print (square: square) =
        match square.piece with
        | Some piece -> Piece.getLetter piece
        | None -> '.'