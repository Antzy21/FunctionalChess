namespace Chess

open FSharp.Extensions

type boardOccurenceCounter = Map<board, int>

type game = {
    gameState: gameState;
    previousBoardOccurrences: boardOccurenceCounter ;
    moves: string list
}

module Game =

    module Create =
        let newGame () : game =
            let newGameState = GameState.Create.newGame ()
            {
                moves = [];
                previousBoardOccurrences = boardOccurenceCounter[];
                gameState = newGameState
            }
        let fromFen (fen: string) : game =
            {
                moves = [];
                previousBoardOccurrences = boardOccurenceCounter[];
                gameState = GameState.Create.fromFen fen
            }

    let toString (game: game) =
        $"{GameState.toString game.gameState}" +
        String.concat "\n" game.moves
    let print = toString >> printfn "%s"

    module Update = 
        let private updatePreviousGameStates (gameStateOccurenceCounter: boardOccurenceCounter) newGameState =
            match Map.tryFind newGameState gameStateOccurenceCounter with
            | Some count ->
                count + 1
            | None ->
                1
            |> fun value -> Map.add newGameState value gameStateOccurenceCounter
        let makeMove (move: move) (game: game) : game =
            let newGameState = GameState.Update.makeMove move game.gameState
            {
                moves = MoveParser.AlgebraicNotation.toString move game.gameState.board :: game.moves;
                previousBoardOccurrences = updatePreviousGameStates game.previousBoardOccurrences newGameState.board
                gameState = newGameState
            }
        let makeMoveFromNotation (move: string) (game: game) : game result =
            MoveParser.tryParse game.gameState.playerTurn game.gameState.board move
            |> Result.map (fun parsedMove ->
                makeMove parsedMove game
            )

    let private threeMovesRepeated (game: game) : bool =
        game.previousBoardOccurrences
        |> Map.exists (fun _ count ->
            count >= 3
        )

    let isGameOver (game: game) : bool =
        // No moves mean no legal moves, either checkmate or stalemate 
        GameState.getMoves game.gameState |> List.isEmpty
        || threeMovesRepeated game

    let pgn (game: game) : string =
        game.moves
        |> List.rev
        |> List.fold (fun (i, pgn) move ->
            let pgn = 
                pgn +
                if i%2 = 0 then
                    $"{(i/2)+1}."
                else ""
                + $"{move} "
            (i+1, pgn)
        ) (0, "")
        |> fun (_, pgn) -> pgn.Trim()
