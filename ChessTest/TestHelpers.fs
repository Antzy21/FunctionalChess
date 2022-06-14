module internal TestHelpers

open Chess
open Checkerboard

let getPossibleMovesForPieceType (pieceType: pieceType) (fen: string) : string list =
    fen
    |> GameState.fromFen
    |> GameState.getMovesForPlayer
    |> List.filter (Move.getMovedPiece >> fun p -> p.pieceType = pieceType)
    |> List.map Move.getMoveNotation

let getPossibleCastlingMoveNotations (fen: string) : string list =
    fen
    |> GameState.fromFen
    |> GameState.getMovesForPlayer
    |> List.filter Move.isCastling
    |> List.map Move.getMoveNotation

let gameStateIsInCheck (fen: string) : bool =
    fen
    |> GameState.fromFen
    |> fun gs -> gs.board |> Board.isInCheck gs.playerTurn