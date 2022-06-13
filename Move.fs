namespace Chess

open Checkerboard
open FSharp.Extensions

type move = move<piece>

module Move =
    let isEnpassant (move: move) : bool =
        (Move.getMovedPiece move).pieceType = Pawn &&
        Move.getTakenPiece move |> Option.isNone &&
        Move.getShift move |> fun (i, j) -> (i+j)%2=0
    let isCastling (move: move) : bool =
        (Move.getMovedPiece move).pieceType = King &&
        Move.getShift move |> fun (i, _) -> i%2 = 0
    let getMoveNotation (move: move) : string =
        match Move.getTakenPiece move with
        | Some takenPiece ->
            $"{move |> fst |> Square.getDescription} ->" +
            $"x{(PieceType.getLetter takenPiece.pieceType)}" +
            $"{(move |> snd |> Square.getCoordinatesName)}"
        | None when isEnpassant move -> 
            $"{move |> fst |> Square.getDescription} ->" +
            $"x" +
            match (Move.getMovedPiece move).colour with 
            | White -> "p"
            | Black -> "P"
            + $"{(move |> snd |> Square.getCoordinatesName)}"
        | None when isCastling move ->
            if Move.getShift move = (-2,0) then
                "0-0"
            elif Move.getShift move = (2,0) then
                "0-0-0"
            else 
                failwith "Invalid castling"
        | None -> 
            $"{move |> fst |> Square.getDescription} ->" +
            $"{(move |> snd |> Square.getCoordinatesName)}"
    let getPossibleMoves (colour: colour) (board: board<piece>) : move list =
        Square.getFromBoardWithPiecesOfColour colour board
        |> List.map (fun oldSquare ->
            oldSquare
            |> Square.getMoves board
            |> List.map (fun newSquare ->
                (oldSquare, newSquare)
            )
        )
        |> List.concat