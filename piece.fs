namespace Chess

open Checkerboard
open FSharp.Extensions

type piece = {pieceType: pieceType; colour: colour}

module Piece =
    let getValue (piece: piece) : int option =
        PieceType.getValue piece.pieceType
    let getLetter (piece: piece) : char=
        let letter = PieceType.getLetter piece.pieceType
        if piece.colour = Black then
            System.Char.ToLower letter
        else
            letter
    let getFromLetter (letter: char) : piece =
        let colour = 
            if letter = System.Char.ToUpper letter then
                White
            else
                Black
        {pieceType = PieceType.fromLetter letter; colour = colour}

    let getPawnMoveFunction (start: square<piece>) (board: board<piece>) (piece: piece) =
        let direction, startingRow =
            match piece.colour with
            | White -> 1, 1
            | Black -> -1, 6

        let diagonalMoves =
            Board.getSquares.afterShifts start.coordinates board [(direction,-1); (direction,1)]
            |> List.filter (fun square -> Option.isSome square.piece)
                
        let forwardMoves = 
            Board.getSquares.afterShift (0,direction) start.coordinates board
            |> Option.failOnNone "Pawn shouldn't be at the end of the board"
            |> (fun square -> 
                match square.piece with
                | Some _ -> []
                | None ->
                    match start.coordinates with
                    | (_, row) when row = startingRow ->
                        Board.getSquares.afterShift (0,direction*2) start.coordinates board
                        |> Option.get
                        |> (fun square2 -> 
                            match square2.piece with
                            | Some _ -> [square]
                            | None -> [square; square2]
                        )
                    | _ -> [square]
            )

        List.append forwardMoves diagonalMoves