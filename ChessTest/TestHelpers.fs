module TestHelpers

open Chess

let internal getPossibleCastlingMoveNotations (fen: string) =
    fen
    |> GameState.fromFen
    |> GameState.getMovesForPlayer
    |> List.filter Move.isCastling
    |> List.map Move.getMoveNotation