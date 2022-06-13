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
    let fromLetter (letter: char) : pieceType =
        Map.findKey (fun _ l -> System.Char.ToUpper letter = l) pieceTypeLetterMap
