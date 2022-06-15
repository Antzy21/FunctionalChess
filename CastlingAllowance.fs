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
    let removeRights (colour: colour) (side: side) (ca: castlingAllowance) : castlingAllowance =
        match colour, side with
        | White, Kingside -> {whiteKingside = false; whiteQueenside = ca.whiteQueenside; blackKingside = ca.blackKingside; blackQueenside = ca.blackQueenside}
        | White, Queenside -> {whiteKingside = ca.whiteKingside; whiteQueenside = false; blackKingside = ca.blackKingside; blackQueenside = ca.blackQueenside}
        | Black, Kingside -> {whiteKingside = ca.whiteKingside; whiteQueenside = ca.whiteQueenside; blackKingside = false; blackQueenside = ca.blackQueenside}
        | Black, Queenside -> {whiteKingside = ca.whiteKingside; whiteQueenside = ca.whiteQueenside; blackKingside = ca.blackKingside; blackQueenside = false}
    let removeBasedOnMove (move: move) (ca: castlingAllowance) : castlingAllowance =
        removeRights (Move.getMovedPiece move |> fun p -> p.colour) (Move.getCastlingSide move) ca
    let print (castling: castlingAllowance) : string=
        if castling.whiteKingside then
            "White 0-0\n"
        else
            ""
        +
        if castling.whiteQueenside then
            "White 0-0-0\n"
        else
            ""
        +
        if castling.blackKingside then
            "Black 0-0\n"
        else
            ""
        +
        if castling.blackQueenside then
            "Black 0-0-0\n"
        else
            ""