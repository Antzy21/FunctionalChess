namespace Chess

open Checkerboard
open FSharp.Extensions

type piece = {pieceType: pieceType; colour: colour}

type coordinates = coordinates<int>

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

        let private getPawnVisionFromStartingRow square direction start board =
            Board.GetSquare.afterShift (0,direction*2) start board
            |> Option.get
            |> (fun square2 -> 
                match square2.piece with
                | Some _ -> [square]
                | None -> [square; square2]
            )

        let private getPawnVisionForColour (start: coordinates) (board: board<piece, int>) (direction: int) (startingRow: int) : square<piece, int> list =
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
                            getPawnVisionFromStartingRow square direction start board
                        else [square]
                )
            List.append forwardMoves diagonalMoves        

        let getPawnVision (start: coordinates) (board: board<piece, int>) (pieceColour: colour) : square<piece, int> list =
            let direction = getPawnMovementDirection pieceColour
            let startingRow = getPawnStartingRow pieceColour
            getPawnVisionForColour start board direction startingRow

        let getPawnFrom (start: coordinates) (pieceColour: colour) (board: board<piece, int>): square<piece, int> list =
            let direction = getPawnMovementDirection pieceColour
            let rowIfMovedTwo = getPawnStartingRow pieceColour + direction*2
            let pieceAtStart = Board.GetPiece.fromCoordinates start board
            if Option.isSome pieceAtStart then
                [(-1, -direction); (1, -direction)]
            else
                if snd start = rowIfMovedTwo then
                    [(0, -direction); (0,-direction*2)]
                else                    
                    Board.GetSquare.afterShift (0, -direction) start board
                    |> Option.map (fun square ->
                        match square.piece with
                        | Some piece when piece.colour = pieceColour ->
                            [(0, -direction)]
                        | _ ->
                            [(-1, -direction); (1, -direction)]
                    )
                    |> Option.get
            |> Board.GetSquares.afterShifts start board