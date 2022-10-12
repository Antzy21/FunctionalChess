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
        let deepCopy (game: game) : game =
            {
                moves = game.moves;
                gameState = GameState.Create.deepCopy game.gameState
            }

    let toString (game: game) =
        List.fold (fun s move ->
            s + $"\n{MoveParser.FullNotation.toString move}"
        ) $"{GameState.toString game.gameState}" game.moves
    let print (game: game) =
        toString >> printf "%s"

    module Update = 
        let makeMove (move: move) (game: game) : game =
            {
                moves = move :: game.moves;
                gameState = GameState.Update.makeMove move game.gameState
            }
        let undoMove (game: game) : game =
            let moveToUndo, moves =
                match game.moves with
                | [] -> failwith "No moves to undo"
                | moveToUndo :: moves -> moveToUndo, moves 
            let gameState = 
                match moves with
                | EnPassant epMove :: _ -> 
                    let enpassantCoordinates = Some (snd epMove).coordinates
                    GameState.Update.undoMoveSetEnpassantSquare enpassantCoordinates moveToUndo game.gameState
                | _ ->
                    GameState.Update.undoMove moveToUndo game.gameState
            {
                moves = moves;
                gameState = gameState
            }
        let makeMoveFromNotation (move: string) (game: game) : game =
            let parsedMove = MoveParser.parse game.gameState.playerTurn game.gameState.board move
            makeMove parsedMove game

    let pgn (game: game) : string =
        game.moves
        |> List.rev
        |> List.mapi (fun i move -> 
            if i%2 = 0 then
                $"{(i/2)+1}. "
            else ""
            + $"{(MoveParser.AlgebraicNotation.toString move)} "
        )
        |> List.reduce (+)
