module internal ChessTest.Helpers.Functions

open Chess
open Checkerboard

module GetPossibleMoves = 

    let private getPossibleMovesWithFilter (filter: move -> bool) (fen: string) : string list =
        GameState.Create.fromFen fen
        |> GameState.getMoves
        |> List.filter filter
        |> List.map Move.getFullNotation
    
    let all (fen: string) : string list =
        getPossibleMovesWithFilter (fun _ -> true) fen

    let forPieceType (pieceType: pieceType) (fen: string) : string list =
        getPossibleMovesWithFilter (fun move ->
            match move with
            | NormalMove move -> Move.getMovedPieceType move |> (=) pieceType
            | EnPassant move -> Move.getMovedPieceType move |> (=) pieceType
            | Promotion (move, _) -> Move.getMovedPieceType move |> (=) pieceType
            | Castling _ -> false
        ) fen
    
    let fromSquare (squareName: string) (fen: string) : string list =
        getPossibleMovesWithFilter (fun move ->
            match move with
            | NormalMove move -> fst move |> Square.getCoordinatesName |> (=) squareName
            | _ -> false
        ) fen

    let castling (fen: string) : string list =
        getPossibleMovesWithFilter (fun move ->
            match move with
            | Castling _ -> true
            | _ -> false
        ) fen

let gameStateIsInCheck (fen: string) : bool =
    GameState.Create.fromFen fen
    |> fun gs -> gs.board |> Board.isInCheck gs.playerTurn

let moveNotationFromNotationParser (game: gameState) (notation: string) : string = 
    NotationParser.parse game.playerTurn game.board notation
    |> Move.getFullNotation
    
module UpdateWithMove =
    let applyMove (fen: string) (move: move) : string =
        let gs = GameState.Create.fromFen fen
        GameState.Update.makeMove move gs
        |> GameState.toFen
    
    let parseMoveAndApplyIt (fen: string) (notation: string) : string =
        let gs = GameState.Create.fromFen fen
        let move = NotationParser.parse (Colour.opposite gs.playerTurn) gs.board notation
        GameState.Update.makeMove move gs
        |> GameState.toFen
    
    let undoMove (fen: string) (move: move) : string =
        let gs = GameState.Create.fromFen fen
        GameState.Update.undoMove move gs
        |> GameState.toFen    

    let parseMoveAndUndoIt (fen: string) (notation: string) : string =
        let gs = GameState.Create.fromFen fen
        let move = NotationParser.parse (Colour.opposite gs.playerTurn) gs.board notation
        GameState.Update.undoMove move gs
        |> GameState.toFen
    
let fromFenToFenIsInverseAfterMove (fen: string) (notation: string) : bool =
    let gs = GameState.Create.fromFen fen
    let move = NotationParser.parse gs.playerTurn gs.board notation
    GameState.Update.makeMove move gs
    |> GameState.Update.undoMove move
    |> GameState.toFen
    |> (=) fen

let fromFenToFenIsInverse (fen: string) : bool =
    fen
    |> GameState.Create.fromFen
    |> GameState.toFen
    |> (=) fen