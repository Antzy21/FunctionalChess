namespace Chess

open Checkerboard
open FSharp.Extensions

type normalMove = move<piece, int>

type move = 
    | NormalMove of normalMove
    | Castling of (side * colour)
    | Promotion of (normalMove * pieceType)
    | EnPassant of normalMove

module Move =

    let getMovedPieceType (move: normalMove) : pieceType =
        Move.getMovedPiece move
        |> fun piece -> piece.pieceType

    let getMovedPieceColour (move: normalMove) : colour =
        Move.getMovedPiece move
        |> fun piece -> piece.colour

    /// Get the difference between the starting and destination of the move.
    let getShift (move: normalMove) : int * int =
        Coordinates.getShiftBetweenCoordinates (fst move).coordinates (snd move).coordinates

    module Enpassant =

        /// Gets the optional coordinates that a pawn could be taken through an "en passant" move, that only comes by the previous move being a pawn moving two squares.
        let getEnPassantCoordinates (board: board<piece, int>) (move: normalMove) : coordinates option = 
            let pawnMovedTwoSquares = List.contains (getShift move) [(0,2); (0,-2)]
            let start = fst move
            let moveWasPawn = 
                start
                |> Square.getPieceType
                |> (=) (Some Pawn)
            if moveWasPawn && pawnMovedTwoSquares then
                let shift = 
                    match getMovedPieceColour move with                
                    | White -> (0,1)
                    | Black -> (0,-1)
                Some (Coordinates.getAfterShift shift start.coordinates)
            else
                None


    module PawnMoves =

        let private getPawnMovementDirection (pieceColour: colour) =
            match pieceColour with
            | White -> 1
            | Black -> -1

        let private getPawnStartingRow (pieceColour: colour) =
            match pieceColour with
            | White -> 1
            | Black -> 6

        let private getPawnVisionFromStartingRow direction start board : coordinates list =
            let pawn1SquareJumpCoords = Coordinates.getAfterShift (0,direction*1) start
            let pawn2SquareJumpCoords = Coordinates.getAfterShift (0,direction*2) start
            Board.GetSquare.fromCoordinatesResult board pawn2SquareJumpCoords
            |> (fun pawn2SquareJump -> 
                Board.GetSquare.fromCoordinates board pawn2SquareJumpCoords
                |> fun sqr -> 
                    if sqr.piece.IsSome then
                        [pawn1SquareJumpCoords]
                    else
                        [pawn1SquareJumpCoords; pawn2SquareJumpCoords]
            )

        let private getPawnVisionForColour (start: coordinates) (board: board<piece, int>) (direction: int) (startingRow: int) : coordinates list =
            let diagonalMoves =
                [(-1,direction); (1,direction)]
                |> List.map (Coordinates.getAfterShift start)
                |> List.filter (fun coords ->
                    match Board.GetSquare.fromCoordinatesOption board coords with
                    | None -> false
                    | Some square -> square.piece.IsSome
                )

            let forwardMoves = 
                let coords = Coordinates.getAfterShift (0,direction) start
                Board.GetSquare.fromCoordinatesOption board coords
                |> Option.failOnNone "Pawn shouldn't be at the end of the board"
                |> (fun square ->
                    if square.piece.IsSome then
                        []
                    else
                        if snd start = startingRow then
                            getPawnVisionFromStartingRow direction start board
                        else [coords]
                )
            List.append forwardMoves diagonalMoves        

        let getPawnVision (start: coordinates) (board: board<piece, int>) (pieceColour: colour) : coordinates list =
            let direction = getPawnMovementDirection pieceColour
            let startingRow = getPawnStartingRow pieceColour
            getPawnVisionForColour start board direction startingRow

        let getPawnOriginPossibilitiesFromDestination (destination: coordinates) (pieceColour: colour) (board: board<piece, int>) : coordinates list =
            let direction = getPawnMovementDirection pieceColour
            let rowIfMovedTwo = getPawnStartingRow pieceColour + direction*2
            let pieceAtStart = Board.GetPiece.fromCoordinates destination board
            if Option.isSome pieceAtStart then
                [(-1, -direction); (1, -direction)]
            else
                if snd destination = rowIfMovedTwo then
                    [(0, -direction); (0,-direction*2)]
                else                    
                    Board.GetSquare.afterShift (0, -direction) destination board
                    |> Option.get
                    |> fun square ->
                        if square.piece.Value.colour = pieceColour then
                            [(0, -direction)]
                        else
                            [(-1, -direction); (1, -direction)]
            |> Board.GetCoordinates.afterShifts destination board
