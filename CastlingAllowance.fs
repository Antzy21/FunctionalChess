namespace Chess

open Checkerboard

type castlingAllowance = {whiteKingside: bool; whiteQueenside: bool; blackKingside: bool; blackQueenside: bool;}

module CastlingAllowance =
    let fromFen (fen: string) : castlingAllowance =
        {
            whiteKingside = fen.Contains('K');
            whiteQueenside = fen.Contains('Q');
            blackKingside = fen.Contains('k');
            blackQueenside = fen.Contains('q')
        }
    let toFen (castling: castlingAllowance) : string =
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
    let removeRights = modifyRights false
    let addRights = modifyRights true        
    let print (castling: castlingAllowance) : string=
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