namespace Chess

open Checkerboard
open FSharp.Extensions

[<Struct>]
type piece = {pieceType: pieceType; colour: colour}

type coordinates = coordinates<int>

module internal PieceBitMap =
    let containsPiece (pieceBitMap: squareBitMap) : bool =
        List.head pieceBitMap
    let containsPieceOfColour (colour: colour) (squareBitMap: squareBitMap) : bool =
        match squareBitMap with
        | optionMap :: colourMap :: _ ->
            optionMap && colourMap = Colour.toBool colour
        | _ -> failwith "Error parsing pieceBitMap. Not enough maps for optionality and colour."
            

module Piece =

    module internal Parser =
        let fromBitMaps (boolList: squareBitMap) : piece =
            let parser = Parsers.addLayerToParser Colour.fromBool PieceType.Parser.fromBitMaps
            let colour, pieceType = parser boolList
            {pieceType = pieceType; colour = colour}
        let toBitMaps (piece: piece) : squareBitMap =
            let colour = Colour.toBool piece.colour
            let pieceType = PieceType.Parser.toBitMaps piece.pieceType
            List.append [colour] pieceType

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
