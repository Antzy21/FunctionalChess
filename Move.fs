namespace Chess

open Checkerboard
open FSharp.Extensions

type move = move<piece>

module Move =
    let isEnpassant (move: move) : bool =
        (Move.getMovedPiece move).pieceType = Pawn &&
        Move.getTakenPiece move |> Option.isNone &&
        Move.getShift move |> (fun (i, j) -> (i+j)%2=0)
    let getMoveNotation (move: move) : string =
        $"{move |> fst |> Square.getDescription}"
        + "->" +
        match Move.getTakenPiece move with
        | Some takenPiece -> $"x{(PieceType.getLetter takenPiece.pieceType)}"
        | None when isEnpassant move -> 
            $"x" +
            match (Move.getMovedPiece move).colour with 
            | White -> "p"
            | Black -> "P"
        | None -> ""
        + $"{(move |> snd |> Square.getName)}"