namespace Chess

open Checkerboard
open FSharp.Extensions

type square = piece option

module Square =
    
    module Parser =
        let fromBitMaps (boolList: squareBitMap) : square =
            let parser = Parsers.addOptionalLayerToParser Piece.Parser.fromBitMaps
            parser boolList
        let toBitMaps (square: square) : squareBitMap =
            match square with
            | Some piece ->
                true :: Piece.Parser.toBitMaps piece
            | None ->
                [false; false; false; false; false]

    let toString (coords: coordinates) (square: squareBitMap) =
        let pieceTypeLetter = 
            match Parser.fromBitMaps square with
            | None -> ""
            | Some piece -> $"{Piece.getLetter piece}"
        $"{pieceTypeLetter}{(Coordinates.getName coords)}"

    let getPieceType (square: squareBitMap) : pieceType option =
        square
        |> Parser.fromBitMaps
        |> Option.map (fun piece -> piece.pieceType)

    let getPieceColour (square: squareBitMap) : colour option =
        match square with
        | [isSomePiece; colourBool; _; _; _] ->
            if not isSomePiece then
                None
            else
                Some (Colour.fromBool colourBool)
        | _ -> failwith $"squareBitMap {square} is not correct length"

    module BitMap =
        let isColour (colour: colour) (square: squareBitMap) : bool =
            match square with
            | [isSomePiece; colourBool; _; _; _] -> isSomePiece && colourBool = Colour.toBool colour
            | _ -> failwith $"squareBitMap {square} is not correct length"
        let containsPiece (square: squareBitMap) : bool =
            square.Head
        let containsColouredPiece (colour: colour) (square: squareBitMap) : bool =
            match square with
            | [true; colourBool; _; _; _] -> colourBool = Colour.toBool colour
            | _ -> false
        let contains (piece: piece) (square: squareBitMap) : bool =
            square
            |> Parser.fromBitMaps
            |> Option.map ((=) piece)
            |> Option.defaultValue false
        let containsPieceOfType (pieceType: pieceType) (square: squareBitMap) : bool =
            square
            |> Parser.fromBitMaps
            |> Option.map (fun piece -> piece.pieceType = pieceType)
            |> Option.defaultValue false
