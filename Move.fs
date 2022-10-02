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
    
    let getMoveNotation (move: move) : string =
        match move with
        | Castling (Kingside, _) -> "0-0"
        | Castling (Queenside, _) -> "0-0-0"
        | Promotion (move, promotedPieceType) ->
            let timesSignIfTaken =
                if Move.getShift move |> fst <> 0 then "x"
                else ""
            $"{move |> fst |> Square.getDescription} -> " +
            timesSignIfTaken +
            $"{(move |> snd |> Square.getCoordinatesName)}" +
            " = " +
            $"{(PieceType.getLetter promotedPieceType)}"
        | EnPassant move ->
            $"{move |> fst |> Square.getDescription} -> " +
            $"x" +
            match (Move.getMovedPiece move).colour with 
            | White -> "p"
            | Black -> "P"
            + $"{(move |> snd |> Square.getCoordinatesName)}"
            + $" e.p."
        | NormalMove move ->
            match Move.getPieceAtDestination move with
            | Some takenPiece ->
                $"{move |> fst |> Square.getDescription} -> " +
                $"x{(Piece.getLetter takenPiece)}" +
                $"{(move |> snd |> Square.getCoordinatesName)}"
            | None -> 
                $"{move |> fst |> Square.getDescription} -> " +
                $"{(move |> snd |> Square.getCoordinatesName)}"

    //let getPgnMoveNotation (move: move) : string =
        

    let printMoveNotation (move: move) =
        printfn $"{getMoveNotation move}"
