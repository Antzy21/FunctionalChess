namespace Chess

open Checkerboard

[<Struct>]
type castlingAllowance = {whiteKingside: bool; whiteQueenside: bool; blackKingside: bool; blackQueenside: bool;}

module internal CastlingAllowance =

    /// Parse the part of the fen string that represents castling allowance into a castlingAllowance record
    let internal fromFenPart (fen: string) : castlingAllowance =
        {
            whiteKingside = fen.Contains('K');
            whiteQueenside = fen.Contains('Q');
            blackKingside = fen.Contains('k');
            blackQueenside = fen.Contains('q')
        }

    let internal toFenPart (castling: castlingAllowance) : string =
        let allowances = 
            if castling.whiteKingside then
                "K"
            else
                ""
            +
            if castling.whiteQueenside then
                "Q"
            else
                ""
            +
            if castling.blackKingside then
                "k"
            else
                ""
            +
            if castling.blackQueenside then
                "q"
            else
                ""
        if allowances = "" then
            "-"
        else
            allowances

    let private modifyRights (setting: bool) (side: side) (colour: colour) (ca: castlingAllowance) : castlingAllowance =
        match colour, side with
        | White, Kingside -> {whiteKingside = setting; whiteQueenside = ca.whiteQueenside; blackKingside = ca.blackKingside; blackQueenside = ca.blackQueenside}
        | White, Queenside -> {whiteKingside = ca.whiteKingside; whiteQueenside = setting; blackKingside = ca.blackKingside; blackQueenside = ca.blackQueenside}
        | Black, Kingside -> {whiteKingside = ca.whiteKingside; whiteQueenside = ca.whiteQueenside; blackKingside = setting; blackQueenside = ca.blackQueenside}
        | Black, Queenside -> {whiteKingside = ca.whiteKingside; whiteQueenside = ca.whiteQueenside; blackKingside = ca.blackKingside; blackQueenside = setting}

    let private removeRights = modifyRights false

    /// Remove castling allowance if the current move is castling, or the king or one of the rooks move.
    let internal removeBasedOnMove (colour: colour) (ca: castlingAllowance) (board: board) (move: move) =
        match move with
        | Castling (_, colour) -> 
            ca
            |> removeRights Kingside colour
            |> removeRights Queenside colour
        | NormalMove normalMove ->
            let movedPieceType =
                Board.getSquareFromCoordinates board normalMove.startingCoords
                |> Square.Parser.fromBitMaps
                |> fun sqr -> sqr.Value.pieceType
            match movedPieceType with
            | King ->
                ca
                |> removeRights Kingside colour
                |> removeRights Queenside colour
            | Rook ->
                let rank =
                    match colour with
                    | White -> 0
                    | Black -> 7
                match normalMove.startingCoords with
                | (0, r) when r = rank -> removeRights Queenside colour ca
                | (7, r) when r = rank -> removeRights Kingside colour ca
                | _ -> ca
            | _ -> ca
        | _ -> ca

    let internal toString (castling: castlingAllowance) : string =
        if castling.whiteKingside then
            "W 0-0, "
        else
            "-, "
        +
        if castling.whiteQueenside then
            "W 0-0-0, "
        else
            "-, "
        +
        if castling.blackKingside then
            "B 0-0, "
        else
            "-, "
        +
        if castling.blackQueenside then
            "B 0-0-0"
        else
            "-"