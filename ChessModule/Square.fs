namespace Chess

open Checkerboard
open FSharp.Extensions

type square = piece option

module Square =
    
    let toString (coords: coordinates) (square: square) =
        let pieceTypeLetter = 
            match square with
            | None -> ""
            | Some piece -> $"{Piece.getLetter piece}"
        $"{pieceTypeLetter}{(Coordinates.getName coords)}"
