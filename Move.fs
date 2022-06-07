namespace Chess

open Checkerboard

type move = square * square

module Move =
    let getTakenPiece (move: move) : piece option =
        snd move
        |> fun square -> square.piece
    let getMovedPiece (move: move) : piece =
        fst move
        |> Square.getPiece
    let getMoveNotation (move: move) : string =
        $"{move |> fst |> Square.getDescription}"
        + "->" +
        match getTakenPiece move with
        | None -> ""
        | Some takenPiece -> $"x{(PieceType.getLetter takenPiece.pieceType)}"
        + $"{(move |> snd |> Square.getName)}"