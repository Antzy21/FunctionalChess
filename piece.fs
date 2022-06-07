namespace Chess

open Checkerboard
open FSharp.Extensions

type pieceType =
    | Pawn
    | Rook
    | Knight
    | Bishop
    | King
    | Queen

module PieceType =
    let getValue (piece: pieceType) : int option =
        match piece with
        | Pawn -> Some 1
        | Rook -> Some 5
        | Knight -> Some 3
        | Bishop -> Some 3
        | King -> None
        | Queen -> Some 9
    let getLetter (piece: pieceType) : char =
        match piece with
        | Pawn -> 'P'
        | Rook -> 'R'
        | Knight -> 'N'
        | Bishop -> 'B'
        | King -> 'K'
        | Queen -> 'Q'
    
type colour = 
    | White
    | Black

module Colour =
    let opposite (colour: colour) : colour =
        match colour with
        | White -> Black
        | Black -> White

type piece = {pieceType: pieceType; colour: colour}

type pieceMoveGenerator = (int * int) -> (int * int) list

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
        match letter with
        | 'P' -> {pieceType = Pawn; colour = White}
        | 'p' -> {pieceType = Pawn; colour = Black}
        | 'R' -> {pieceType = Rook; colour = White}
        | 'r' -> {pieceType = Rook; colour = Black}
        | 'N' -> {pieceType = Knight; colour = White}
        | 'n' -> {pieceType = Knight; colour = Black}
        | 'B' -> {pieceType = Bishop; colour = White}
        | 'b' -> {pieceType = Bishop; colour = Black}
        | 'K' -> {pieceType = King; colour = White}
        | 'k' -> {pieceType = King; colour = Black}
        | 'Q' -> {pieceType = Queen; colour = White}
        | 'q' -> {pieceType = Queen; colour = Black}
        | c -> failwith (sprintf "No piece matches letter %c" c)

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