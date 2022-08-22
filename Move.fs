namespace Chess

open Checkerboard
open FSharp.Extensions

type move = move<piece>

module Move =
    let getMovedPieceType (move: move) : pieceType =
        Move.getMovedPiece move
        |> fun piece -> piece.pieceType
    let isEnpassant (move: move) : bool =
        (Move.getMovedPiece move).pieceType = Pawn &&
        Move.getPieceAtDestination move |> Option.isNone &&
        Move.getShift move |> fun (i, j) -> (abs(i), abs(j)) = (1,1)
    let getEnPassantSquare (move: move) : square option = 
        if getMovedPieceType move = Pawn && Move.getShift move = (0,2) then
            let startingSquare = fst move
            let shift = 
                Move.getMovedPiece move
                |> fun piece -> piece.colour
                |> function
                | White -> (0,1)
                | Black -> (0,-1)
            Some {
                coordinates = Coordinates.afterShift shift startingSquare.coordinates;
                piece = None
            }
        else 
            None
    let isCastling (move: move) : bool =
        (Move.getMovedPiece move).pieceType = King &&
        Move.getShift move |> fun (i, j) -> (i, j) = (2,0) || (i, j) = (-2, 0)
    let isPromotion (move: move) (board: board<piece>) : bool =
        let promotionPiece = (snd move).piece
        let coordinates = (snd move).coordinates
        let endSquare = Board.GetSquare.fromCoordinates coordinates board
        endSquare.piece <> promotionPiece
    let getCastlingSide (move: move) : side =
        if not <| isCastling move then
            failwith $"Move is not castling"
        elif Move.getShift move = (-2,0) then
            Queenside
        elif Move.getShift move = (2,0) then
            Kingside
        else
            failwith $"Move is not castling"
    let getMoveNotation (move: move) : string =
        match Move.getPieceAtDestination move with
        | Some promotingPiece when promotingPiece.colour = (Move.getMovedPiece move).colour ->
            let timesSignIfTaken =
                if Move.getShift move |> fst <> 0 then "x"
                else ""
            $"{move |> fst |> Square.getDescription} -> " +
            timesSignIfTaken +
            $"{(move |> snd |> Square.getCoordinatesName)}" +
            " = " +
            $"{(PieceType.getLetter promotingPiece.pieceType)}"
        | Some takenPiece ->
            $"{move |> fst |> Square.getDescription} -> " +
            $"x{(PieceType.getLetter takenPiece.pieceType)}" +
            $"{(move |> snd |> Square.getCoordinatesName)}"
        | None when isEnpassant move -> 
            $"{move |> fst |> Square.getDescription} -> " +
            $"x" +
            match (Move.getMovedPiece move).colour with 
            | White -> "p"
            | Black -> "P"
            + $"{(move |> snd |> Square.getCoordinatesName)}"
        | None when isCastling move ->
            match getCastlingSide move with
            | Kingside -> "0-0"
            | Queenside -> "0-0-0"
        | None -> 
            $"{move |> fst |> Square.getDescription} -> " +
            $"{(move |> snd |> Square.getCoordinatesName)}"
    let printMoveNotation (move: move) =
        printfn $"{getMoveNotation move}"
