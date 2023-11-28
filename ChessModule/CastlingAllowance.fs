namespace Chess

open Checkerboard
open FSharp.Extensions

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
                |> fun sqr -> sqr.Value.pieceType
            match movedPieceType with
            | King ->
                ca
                |> removeRights Kingside colour
                |> removeRights Queenside colour
            | Rook ->
                match colour with
                | White -> 0
                | Black -> 7
                |> fun rank ->
                if Coordinates.construct rank 0 |> Result.failOnError = normalMove.startingCoords then
                    removeRights Queenside colour ca
                elif Coordinates.construct rank 7 |> Result.failOnError = normalMove.startingCoords then
                    removeRights Kingside colour ca
                else
                    ca
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