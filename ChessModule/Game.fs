namespace Chess

type game = {
    gameState: gameState;
    moves: move list
}

module Game =

    module Create =
        let newGame () : game =
            {
                moves = [];
                gameState = GameState.Create.newGame ()
            }
        let fromFen (fen: string) : game =
            {
                moves = [];
                gameState = GameState.Create.fromFen fen
            }

    let toString (game: game) =
        List.fold (fun s move ->
            s + $"\n{MoveParser.FullNotation.toString game.gameState.board move}"
        ) $"{GameState.toString game.gameState}" game.moves
    let print = toString >> printfn "%s"

    module Update = 
        let makeMove (move: move) (game: game) : game =
            {
                moves = move :: game.moves;
                gameState = GameState.Update.makeMove move game.gameState
            }
        let makeMoveFromNotation (move: string) (game: game) : game =
            let parsedMove = MoveParser.parse game.gameState.playerTurn game.gameState.board move
            makeMove parsedMove game

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
