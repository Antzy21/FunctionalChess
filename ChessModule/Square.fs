namespace Chess

open Checkerboard
open FSharp.Extensions

type square = square<piece, int>

module Square =
    let toString (square: square) =
        match square.piece with
        | Some piece -> Piece.getLetter piece
        | None -> '.'
    let getDescription (square: square) =
        let pieceTypeLetter = 
            match square.piece with
            | None -> ""
            | Some piece -> $"{Piece.getLetter piece}"
        $"{pieceTypeLetter}{(Square.getCoordinatesName square)}"
    let getPieceType (square: square) : pieceType option =
       square.piece
       |> Option.map (fun piece -> piece.pieceType)
    let getPieceColour (square: square) : colour option =
       square.piece
       |> Option.map (fun piece -> piece.colour)
