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

    module PawnMoves =
        let private getPawnMovementDirection (pieceColour: colour) =
            match pieceColour with
            | White -> 1
            | Black -> -1
        let private getPawnStartingRow (pieceColour: colour) =
            match pieceColour with
            | White -> 1
            | Black -> 6

        let private getPawnMovesFromStartingRow square direction start board =
            Board.GetSquare.afterShift (0,direction*2) start board
            |> Option.get
            |> (fun square2 -> 
                match square2.piece with
                | Some _ -> [square]
                | None -> [square; square2]
            )

        let private getPawnMovesForColour (start: coordinates) (board: board<piece>) (direction: int) (startingRow: int) =
            let diagonalMoves =
                Board.GetSquares.afterShifts start board [(-1,direction); (1,direction)]
                |> List.filter (fun square -> Option.isSome square.piece)
                
            let forwardMoves = 
                Board.GetSquare.afterShift (0,direction) start board
                |> Option.failOnNone "Pawn shouldn't be at the end of the board"
                |> (fun square -> 
                    match square.piece with
                    | Some _ -> []
                    | None ->
                        if snd start = startingRow then
                            getPawnMovesFromStartingRow square direction start board
                        else [square]
                )
            List.append forwardMoves diagonalMoves        

        let getPawnMoves (start: coordinates) (board: board<piece>) (pieceColour: colour) =
            let direction = getPawnMovementDirection pieceColour
            let startingRow = getPawnStartingRow pieceColour

            getPawnMovesForColour start board direction startingRow

        let getPawnFrom (start: coordinates) (pieceColour: colour) (board: board<piece>): square<piece> list =
            let direction = getPawnMovementDirection pieceColour
            let rowIfMovedTwo = getPawnStartingRow pieceColour + direction*2
            if snd start = rowIfMovedTwo then
                [(0, -direction); (-1, -direction); (1, -direction); (0,-direction*2)]
            else
                [(0, -direction); (-1, -direction); (1, -direction)]
            |> Board.GetSquares.afterShifts start board