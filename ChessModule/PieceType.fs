namespace Chess

type pieceType =
    | Pawn
    | Rook
    | Knight
    | Bishop
    | King
    | Queen

module PieceType =
    let private pieceTypeLetterMap : Map<pieceType, char> =
        [
            (Pawn, 'P');
            (Rook, 'R');
            (Knight, 'N');
            (Bishop, 'B');
            (King, 'K');
            (Queen, 'Q');
        ]
        |> Map.ofList
    module Parser =
        let fromBitMaps (bitmap: bool list) =
            if bitmap.Length <> 3 then
                failwith "bitmaps for Chess piece should have length 3"
            else
                match bitmap[0], bitmap[1], bitmap[2] with
                | true, true, _ -> Pawn
                | true, false, _ -> King
                | false, true, true -> Knight
                | false, true, false -> Bishop
                | false, false, true -> Queen
                | false, false, false -> Rook
        let toBitMaps (pieceType: pieceType) =
            match pieceType with
            | Pawn -> [true; true; true]
            | King -> [true; false; false]
            | Knight -> [false; true; true]
            | Bishop -> [false; true; false]
            | Queen -> [false; false; true]
            | Rook -> [false; false; false]
    let getValue (piece: pieceType) : int option =
        match piece with
        | Pawn -> Some 1
        | Rook -> Some 5
        | Knight -> Some 3
        | Bishop -> Some 3
        | King -> None
        | Queen -> Some 9
    let getLetter (piece: pieceType) : char =
        Map.find piece pieceTypeLetterMap
    let tryParse (letter: char) : pieceType option =
        Map.tryFindKey (fun _ l -> System.Char.ToUpper letter = l) pieceTypeLetterMap
    let fromLetter : char -> pieceType = tryParse >> Option.get 
