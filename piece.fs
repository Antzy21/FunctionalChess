namespace Chess

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