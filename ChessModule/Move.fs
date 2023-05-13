namespace Chess

open Checkerboard
open FSharp.Extensions

[<Struct>]
type normalMove = {startingCoords: coordinates; destinationCoords: coordinates}

type move = 
    | NormalMove of normalMove
    | Castling of (side * colour)
    | Promotion of (normalMove * pieceType)
    | EnPassant of normalMove

module Move =

    let getShift (move: normalMove) : struct (int*int) =
        Coordinates.getShiftBetweenCoordinates move.startingCoords move.destinationCoords

    module Enpassant =
        /// Gets the optional coordinates that a pawn could be taken through an "en passant" move, that only comes by the previous move being a pawn moving two squares.
        let getEnPassantCoordinates (board: board) (move: normalMove) : coordinates option = 
            let pawnMovedTwoSquares = List.contains (getShift move) [(0,2); (0,-2)]
            let start = Board.GetSquare.fromCoordinates board move.startingCoords
            let moveWasPawn = 
                start
                |> Square.BitMap.containsPieceOfType Pawn
            if moveWasPawn && pawnMovedTwoSquares then
                let shift = 
                    match Square.getPieceColour start with                
                    | Some White -> struct (0,1)
                    | Some Black -> struct (0,-1)
                    | None -> failwith "No piece"
                Some (Coordinates.getAfterShift shift move.startingCoords)
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
                |> Square.Parser.fromBitMaps
                |> fun sqr -> 
                    if sqr.IsSome then
                        [pawn1SquareJumpCoords]
                    else
                        [pawn1SquareJumpCoords; pawn2SquareJumpCoords]
            )

        let private getPawnVisionForColour (start: coordinates) (board: board) (direction: int) (startingRow: int) : coordinates list =
            let diagonalMoves =
                [struct (-1,direction); struct (1,direction)]
                |> List.map (Coordinates.getAfterShift start)
                |> List.filter (fun coords ->
                    match Board.GetSquare.fromCoordinatesOption board coords with
                    | None -> false
                    | Some bitmap -> PieceBitMap.containsPiece bitmap
                )
                
            let forwardMoves = 
                let coords = Coordinates.getAfterShift (0,direction) start
                Board.GetSquare.fromCoordinatesOption board coords
                |> Option.failOnNone "Pawn shouldn't be at the end of the board"
                |> (fun squareBitMap ->
                    if PieceBitMap.containsPiece squareBitMap then
                        []
                    else
                        if (start |> fun (struct (x,y)) -> y = startingRow) then
                            getPawnVisionFromStartingRow direction start board
                        else [coords]
                )
            List.append forwardMoves diagonalMoves        

        let getPawnVision (start: coordinates) (board: board) (pieceColour: colour) : coordinates list =
            let direction = getPawnMovementDirection pieceColour
            let startingRow = getPawnStartingRow pieceColour
            getPawnVisionForColour start board direction startingRow

        let getPawnOriginPossibilitiesFromDestination (destination: coordinates) (pieceColour: colour) (board: board): coordinates list =
            let direction = getPawnMovementDirection pieceColour
            let rowIfMovedTwo = getPawnStartingRow pieceColour + direction*2
            let containsPieceAtStart = Board.GetSquare.fromCoordinates board destination |> List.head // Should be Square.BitMaps.containsPiece function here.
            if containsPieceAtStart then
                [(-1, -direction); (1, -direction)]
            else
                if (destination |> fun (struct (x,y)) -> y = rowIfMovedTwo) then
                    [(0, -direction); (0,-direction*2)]
                else
                    Coordinates.getAfterShift (0, -direction) destination
                    |> Board.GetSquare.fromCoordinatesOption board
                    |> Option.map (fun square ->
                        if PieceBitMap.containsPieceOfColour pieceColour square then
                            [struct (0, -direction)]
                        else
                            [struct (-1, -direction); struct (1, -direction)]
                    )
                    |> Option.get