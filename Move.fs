namespace Chess

open Checkerboard
open FSharp.Extensions

type move = move<piece>

module Move =
    let isEnpassant (move: move) : bool =
        (Move.getMovedPiece move).pieceType = Pawn &&
        Move.getTakenPiece move |> Option.isNone &&
        Move.getShift move |> fun (i, j) -> (abs(i), abs(j)) = (1,1)
    let isCastling (move: move) : bool =
        (Move.getMovedPiece move).pieceType = King &&
        Move.getShift move |> fun (i, j) -> (i, j) = (2,0) || (i, j) = (-2, 0)
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
        match Move.getTakenPiece move with
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
