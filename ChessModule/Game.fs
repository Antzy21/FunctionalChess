namespace Chess

open FSharp.Extensions

type fens = Map<string, int>

type game = {
    gameState: gameState;
    fens: fens ;
    moves: string list
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
        $"{GameState.toString game.gameState}" +
        String.concat "\n" game.moves
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
                moves = MoveParser.AlgebraicNotation.toString move game.gameState.board :: game.moves;
                fens = updateFens game.fens (GameState.toFen newGameState)
                gameState = newGameState
            }
        let makeMoveFromNotation (move: string) (game: game) : game result =
            MoveParser.tryParse game.gameState.playerTurn game.gameState.board move
            |> Result.map (fun parsedMove ->
                makeMove parsedMove game
            )

    let private threeMovesRepeated (game: game) : bool =
        game.fens
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
