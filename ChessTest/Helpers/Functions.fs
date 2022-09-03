module internal ChessTest.Helpers.Functions

open Chess
open Checkerboard

module GetPossibleMoves = 

    let private getPossibleMovesWithFilter (filter: move -> bool) (fen: string) : string list =
        GameState.fromFen fen
        |> GameState.getMovesForPlayer
        |> List.filter filter
        |> List.map Move.getMoveNotation
    
    let all (fen: string) : string list =
        getPossibleMovesWithFilter (fun _ -> true) fen

    let forPieceType (pieceType: pieceType) (fen: string) : string list =
        getPossibleMovesWithFilter (fun move ->
            match move with
            | Move move -> Move.getMovedPieceType move |> (=) pieceType
            | Promotion (move, _) -> Move.getMovedPieceType move |> (=) pieceType
            | Castling _ -> false
        ) fen
    
    let fromSquare (squareName: string) (fen: string) : string list =
        getPossibleMovesWithFilter (fun move ->
            match move with
            | Move move -> fst move |> Square.getCoordinatesName |> (=) squareName
            | _ -> false
        ) fen

    let castling (fen: string) : string list =
        getPossibleMovesWithFilter (fun move ->
            match move with
            | Castling _ -> true
            | _ -> false
        ) fen

let gameStateIsInCheck (fen: string) : bool =
    GameState.fromFen fen
    |> fun gs -> gs.board |> Board.isInCheck gs.playerTurn

let moveNotationFromNotationParser (game: gameState) (notation: string) : string = 
    NotationParser.parse game.playerTurn game.board notation
    |> Move.getMoveNotation

let fromFenToFenIsInverse (fen: string) : bool =
    fen
    |> GameState.fromFen
    |> GameState.toFen
    |> (=) fen