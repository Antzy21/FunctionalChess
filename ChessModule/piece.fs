namespace Chess

open Checkerboard
open FSharp.Extensions

[<Struct>]
type piece = {pieceType: pieceType; colour: colour}

type coordinates = coordinates<int>
 

module Piece =

    let private pieceUnicodeMap : Map<piece, char> =
        [
            ({pieceType = Pawn; colour = White }, '\u265F');
            ({pieceType = Rook; colour = White }, '\u265C');
            ({pieceType = Knight; colour = White }, '\u265E');
            ({pieceType = Bishop; colour = White }, '\u265D');
            ({pieceType = King; colour = White }, '\u265A');
            ({pieceType = Queen; colour = White }, '\u265B');
            ({pieceType = Pawn; colour = Black }, '\u2659');
            ({pieceType = Rook; colour = Black }, '\u2656');
            ({pieceType = Knight; colour = Black }, '\u2658');
            ({pieceType = Bishop; colour = Black }, '\u2657');
            ({pieceType = King; colour = Black }, '\u2654');
            ({pieceType = Queen; colour = Black }, '\u2655');
        ]
        |> Map.ofList

    let getValue (piece: piece) : int option =
        PieceType.getValue piece.pieceType

    let getLetter (piece: piece) : char =
        let letter = PieceType.getLetter piece.pieceType
        if piece.colour = Black then
            System.Char.ToLower letter
        else
            letter

    let getUnicodeChar (piece: piece) : char =
        Map.find piece pieceUnicodeMap

    let getFromLetter (letter: char) : piece =
        let colour = 
            if letter = System.Char.ToUpper letter then
                White
            else
                Black
        {pieceType = PieceType.fromLetter letter; colour = colour}
