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