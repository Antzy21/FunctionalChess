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
            | White -> 1y
            | Black -> -1y
        let private getPawnStartingRow (pieceColour: colour) =
            match pieceColour with
            | White -> 1y
            | Black -> 6y

        let private getPawnVisionFromStartingRow square direction start board =
            Board.GetSquare.afterShift (0y,direction*2y) start board
            |> Option.get
            |> (fun square2 -> 
                match square2.piece with
                | Some _ -> [square]
                | None -> [square; square2]
            )

        let private getPawnVisionForColour (start: coordinates<sbyte>) (board: board<piece, sbyte>) (direction: sbyte) (startingRow: sbyte) : square<piece, sbyte> list =
            let diagonalMoves =
                Board.GetSquares.afterShifts start board [(-1y,direction); (1y,direction)]
                |> List.filter (fun square -> Option.isSome square.piece)
                
            let forwardMoves = 
                Board.GetSquare.afterShift (0y,direction) start board
                |> Option.failOnNone "Pawn shouldn't be at the end of the board"
                |> (fun square -> 
                    match square.piece with
                    | Some _ -> []
                    | None ->
                        if snd start = startingRow then
                            getPawnVisionFromStartingRow square direction start board
                        else [square]
                )
            List.append forwardMoves diagonalMoves        

        let getPawnVision (start: coordinates<sbyte>) (board: board<piece, sbyte>) (pieceColour: colour) : square<piece, sbyte> list =
            let direction = getPawnMovementDirection pieceColour
            let startingRow = getPawnStartingRow pieceColour
            getPawnVisionForColour start board direction startingRow

        let getPawnFrom (start: coordinates<sbyte>) (pieceColour: colour) (board: board<piece, sbyte>): square<piece, sbyte> list =
            let direction = getPawnMovementDirection pieceColour
            let rowIfMovedTwo = getPawnStartingRow pieceColour + direction*2y
            let pieceAtStart = Board.GetPiece.fromCoordinates start board
            if Option.isSome pieceAtStart then
                [(-1y, -direction); (1y, -direction)]
            else
                if snd start = rowIfMovedTwo then
                    [(0y, -direction); (0y,-direction*2y)]
                else                    
                    Board.GetSquare.afterShift (0y, -direction) start board
                    |> Option.map (fun square ->
                        match square.piece with
                        | Some piece when piece.colour = pieceColour ->
                            [(0y, -direction)]
                        | _ ->
                            [(-1y, -direction); (1y, -direction)]
                    )
                    |> Option.get
            |> Board.GetSquares.afterShifts start board