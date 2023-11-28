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
    member this.whiteQueenMap =
        this.ColourBitmap &&& this.KQBRmap &&& this.KQPmap &&& (~~~ this.KPNRmap) 
    member this.blackQueenMap =
        (~~~ this.ColourBitmap) &&& this.KQBRmap &&& this.KQPmap &&& (~~~ this.KPNRmap)
    member this.whiteKingMap =
        this.ColourBitmap &&& this.KQBRmap &&& this.KQPmap &&& this.KPNRmap
    member this.blackKingMap =
        (~~~ this.ColourBitmap) &&& this.KQBRmap &&& this.KQPmap &&& this.KPNRmap
    member this.whitePawnMap =
        this.ColourBitmap &&& (~~~ this.KQBRmap) &&& this.KQPmap &&& this.KPNRmap
    member this.blackPawnMap =
        (~~~ this.ColourBitmap) &&& (~~~ this.KQBRmap) &&& this.KQPmap &&& this.KPNRmap
    member this.whiteRookMap =
        this.ColourBitmap &&& this.KQBRmap &&& (~~~ this.KQPmap) &&& this.KPNRmap
    member this.blackRookMap =
        (~~~ this.ColourBitmap) &&& this.KQBRmap &&& (~~~ this.KQPmap) &&& this.KPNRmap
    member this.whiteKnightMap =
        this.ColourBitmap &&& (~~~ this.KQBRmap) &&& (~~~ this.KQPmap) &&& this.KPNRmap
    member this.blackKnightMap =
        (~~~ this.ColourBitmap) &&& (~~~ this.KQBRmap) &&& (~~~ this.KQPmap) &&& this.KPNRmap
    member this.whiteBishopMap =
        this.ColourBitmap &&& this.KQBRmap &&& (~~~ this.KQPmap) &&& (~~~ this.KPNRmap)
    member this.blackBishopMap =
        (~~~ this.ColourBitmap) &&& this.KQBRmap &&& (~~~ this.KQPmap) &&& (~~~ this.KPNRmap)
