namespace Chess

open System.Collections.Generic

type fens = Map<string, int>

type game = {
    gameState: gameState;
    fens: fens ;
    moves: move list
}

module Game =

    module Create =
        let newGame () : game =
            let newGameState = GameState.Create.newGame ()
            {
                moves = [];
                fens = fens[];
                gameState = newGameState
            }
        let fromFen (fen: string) : game =
            {
                moves = [];
                fens = fens[];
                gameState = GameState.Create.fromFen fen
            }

    let toString (game: game) =
        List.fold (fun s move ->
            s + $"\n{MoveParser.FullNotation.toString game.gameState.board move}"
        ) $"{GameState.toString game.gameState}" game.moves
    let print = toString >> printfn "%s"

    module Update = 
        let private updateFens (fens: fens) (newFen: string) =
            match Map.tryFind newFen fens with
            | Some count ->
                count + 1
            | None ->
                1
            |> fun value -> Map.add newFen value fens
        let makeMove (move: move) (game: game) : game =
            let newGameState = GameState.Update.makeMove move game.gameState
            {
                moves = move :: game.moves;
                fens = updateFens game.fens (GameState.toFen newGameState)
                gameState = newGameState
            }
        let makeMoveFromNotation (move: string) (game: game) : game =
            let parsedMove = MoveParser.parse game.gameState.playerTurn game.gameState.board move
            makeMove parsedMove game

    let private threeMovesRepeated (game: game) : bool =
        game.fens
        |> Map.exists (fun fen count ->
            count >= 3
        )

    let isGameOver (game: game) : bool =
        GameState.checkmateOrStatemate game.gameState || threeMovesRepeated game

    let pgn (game: game) : string =
        let tempBoard = Board.Create.starting ()
        game.moves
        |> List.rev
        |> List.fold (fun (board, i, pgn) move ->
            let pgn = 
                pgn +
                if i%2 = 0 then
                    $"{(i/2)+1}."
                else ""
                + $"{(MoveParser.AlgebraicNotation.toString move board)} "
            let board = Board.Update.applyMove move board
            Board.print board
            (board, i+1, pgn)
        ) (tempBoard, 0, "")
        |> fun (b, i, pgn) -> pgn.Trim()
