namespace Chess

open Checkerboard

type move = square * square

module Move =
    let getShift (move: move) : int * int =
        let start = (fst move).coordinates
        let finish = (snd move).coordinates
        (fst start - fst finish, snd start - snd finish)
    let getTakenPiece (move: move) : piece option =
        snd move
        |> fun square -> square.piece
    let getMovedPiece (move: move) : piece =
        fst move
        |> Square.getPiece
    let isEnpassant (move: move) : bool =
        (getMovedPiece move).pieceType = Pawn &&
        getTakenPiece move |> Option.isNone &&
        getShift move |> (fun (i, j) -> (i+j)%2=0)
    let getMoveNotation (move: move) : string =
        $"{move |> fst |> Square.getDescription}"
        + "->" +
        match getTakenPiece move with
        | Some takenPiece -> $"x{(PieceType.getLetter takenPiece.pieceType)}"
        | None when isEnpassant move -> 
            $"x" +
            match (getMovedPiece move).colour with 
            | White -> "p"
            | Black -> "P"
        | None -> ""
        + $"{(move |> snd |> Square.getName)}"