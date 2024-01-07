namespace Chess

open Checkerboard

[<Struct>]
type board = 
    {
        /// A map for the pieces that are White
        ColourBitmap: bitMap

        /// A map for determining if piece of type King, Queen or Pawn
        /// (Not a Rook, Knight or Bishop)
        KQPmap: bitMap

        /// A map for determining if piece of type King, Queen, Bishop or Rook
        KQBRmap: bitMap

        /// A map for determining if piece of type King, Pawn, Knight or Rook
        KPNRmap: bitMap
    }
    member this.pieceMap =
        this.KQBRmap ||| this.KPNRmap
    member this.whitePieces =
        this.pieceMap &&& this.ColourBitmap
    member this.blackPieces =
        this.pieceMap &&& (~~~this.ColourBitmap)
    member this.queenMap =
        this.KQBRmap &&& this.KQPmap &&& (~~~ this.KPNRmap) 
    member this.whiteQueenMap =
        this.ColourBitmap &&& this.queenMap
    member this.blackQueenMap =
        (~~~ this.ColourBitmap) &&& this.queenMap
    member this.kingMap =
        this.KQBRmap &&& this.KQPmap &&& this.KPNRmap
    member this.whiteKingMap =
        this.ColourBitmap &&& this.kingMap
    member this.blackKingMap =
        (~~~ this.ColourBitmap) &&& this.kingMap
    member this.pawnMap =
        (~~~ this.KQBRmap) &&& this.KQPmap &&& this.KPNRmap
    member this.whitePawnMap =
        this.ColourBitmap &&& this.pawnMap
    member this.blackPawnMap =
        (~~~ this.ColourBitmap) &&& this.pawnMap
    member this.rookMap =
        this.KQBRmap &&& (~~~ this.KQPmap) &&& this.KPNRmap
    member this.whiteRookMap =
        this.ColourBitmap &&& this.rookMap
    member this.blackRookMap =
        (~~~ this.ColourBitmap) &&& this.rookMap
    member this.knightMap =
        (~~~ this.KQBRmap) &&& (~~~ this.KQPmap) &&& this.KPNRmap
    member this.whiteKnightMap =
        this.ColourBitmap &&& this.knightMap
    member this.blackKnightMap =
        (~~~ this.ColourBitmap) &&& this.knightMap
    member this.bishopMap =
        this.KQBRmap &&& (~~~ this.KQPmap) &&& (~~~ this.KPNRmap)
    member this.whiteBishopMap =
        this.ColourBitmap &&& this.bishopMap
    member this.blackBishopMap =
        (~~~ this.ColourBitmap) &&& this.bishopMap
