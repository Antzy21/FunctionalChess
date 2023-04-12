module internal ChessTest.Helpers.Functions

open Chess
open Checkerboard
open Xunit

module GetPossibleMoves = 

    let private getPossibleMovesWithFilter (filter: board -> move -> bool) (fen: string) : string list =
        let gs = GameState.Create.fromFen fen
        gs
        |> GameState.getMoves
        |> List.filter (filter gs.board)
        |> List.map MoveParser.FullNotation.toString
    
    let all (fen: string) : string list =
        getPossibleMovesWithFilter (fun _ _ -> true) fen

    let forPieceType (pieceType: pieceType) (fen: string) : string list =
        getPossibleMovesWithFilter (fun board move ->
            match move with
            | NormalMove move -> Some move
            | EnPassant move -> Some move
            | Promotion (move, _) -> Some move
            | Castling _ -> None
            |> Option.map (fun move -> Board.GetSquare.fromCoordinates board (fst move).coordinates |> fun sqr -> sqr.piece.Value.pieceType = pieceType)
            |> Option.defaultValue false
        ) fen
    
    let fromSquare (squareName: string) (fen: string) : string list =
        getPossibleMovesWithFilter (fun board move ->
            match move with
            | NormalMove move -> (fst move).coordinates |> Coordinates.getName |> (=) squareName
            | _ -> false
        ) fen

    let castling (fen: string) : string list =
        getPossibleMovesWithFilter (fun board move ->
            match move with
            | Castling _ -> true
            | _ -> false
        ) fen

let gameStateIsInCheck (fen: string) : bool =
    GameState.Create.fromFen fen
    |> fun gs -> gs.board |> Board.isInCheck gs.playerTurn

let moveNotationFromMoveParser (game: gameState) (notation: string) : string = 
    MoveParser.parse game.playerTurn game.board notation
    |> MoveParser.FullNotation.toString

module UpdateWithMove =

    let parseMoveAndApplyIt (fen: string) (notation: string) : string =
        let gs = GameState.Create.fromFen fen
        let move = MoveParser.parse (Colour.opposite gs.playerTurn) gs.board notation
        GameState.Update.makeMove move gs
        |> GameState.toFen

let fromFenToFenIsInverse (fen: string) =
    let actual =
        fen
        |> GameState.Create.fromFen
        |> GameState.toFen
    Assert.Equal(fen, actual)