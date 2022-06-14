module internal TestHelpers

open Chess

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