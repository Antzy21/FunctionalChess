namespace Chess

type gameState = {
    board: board;
    playerTurn: colour;
    castlingAllowance: castlingAllowance;
    enpassantCoordinates: Checkerboard.coordinates option;
    halfMoveClock: int;
    fullMoveClock: int;
    }

module GameState =

    module Create =
        let fromFen (fen: string) : gameState =
            let parts = fen.Split(' ')
            let board = Board.Create.fromFen(parts[0])
            let playerTurn =
                match parts[1] with
                | "w" -> White
                | "b" -> Black
                | c -> failwith $"Error in FEN: Cannot determine player turn from {c}" 
            let castlingAllowance = CastlingAllowance.fromFen parts[2]
            let enpassantCoordinates = 
                match parts[3] with
                | "-" -> None
                | name -> Some (Checkerboard.Coordinates.fromName name)
            let halfMoveClock = int(parts[4])
            let fullMoveClock = int(parts[5])
            {
                board = board;
                playerTurn = playerTurn;
                castlingAllowance = castlingAllowance;
                enpassantCoordinates = enpassantCoordinates;
                halfMoveClock = halfMoveClock;
                fullMoveClock = fullMoveClock;
            }
        let newGame () : gameState =
            fromFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"

    let toFen (game: gameState) : string =
        let enpassant =
            game.enpassantCoordinates
            |> Option.map (Checkerboard.Coordinates.getName)
            |> Option.defaultValue "-"
        let castling = CastlingAllowance.toFen game.castlingAllowance
        let playerTurn = 
            match game.playerTurn with
            | White -> "w"
            | Black -> "b"
        $"{Board.toFen game.board} "
        + $"{playerTurn} "
        + $"{castling} "
        + $"{enpassant} "
        + $"{game.halfMoveClock} {game.fullMoveClock}"
    let print (game: gameState) =
        Board.print game.board
        printfn $"\nPlayer Turn: {game.playerTurn}"
        printfn $"Castling Allowed: \n{CastlingAllowance.print game.castlingAllowance}"
        Option.iter (fun enpasSqr -> printfn $"EnpassantSquare: {enpasSqr}") game.enpassantCoordinates
        printfn $"Turn: {game.fullMoveClock}, Half Turn: {game.halfMoveClock}"
        
    let getMoves (gameState: gameState) : move list =
        let board = gameState.board
        Board.GetMoves.normal gameState.playerTurn board
        |> Board.GetMoves.promotion board
        |> List.append <| Board.GetMoves.enpassant gameState.playerTurn gameState.enpassantCoordinates board
        |> List.append <| Board.GetMoves.castling gameState.playerTurn gameState.castlingAllowance board
    
    module Update = 
        let makeMove (move: move) (gameState: gameState) : gameState =
            Board.Update.applyMove move gameState.board
            {
                board = gameState.board
                playerTurn = Colour.opposite gameState.playerTurn
                castlingAllowance = 
                    match move with
                    | Castling (_, colour) -> 
                        gameState.castlingAllowance
                        |> CastlingAllowance.removeRights Kingside colour
                        |> CastlingAllowance.removeRights Queenside colour
                    | NormalMove normalMove ->
                        match Move.getMovedPieceType normalMove with
                        | King ->
                            gameState.castlingAllowance
                            |> CastlingAllowance.removeRights Kingside gameState.playerTurn
                            |> CastlingAllowance.removeRights Queenside gameState.playerTurn
                        | Rook ->
                            let rank =
                                match gameState.playerTurn with
                                | White -> 0
                                | Black -> 7
                            match (fst normalMove).coordinates with
                            | (0, r) when r = rank -> CastlingAllowance.removeRights Queenside gameState.playerTurn gameState.castlingAllowance
                            | (7, r) when r = rank -> CastlingAllowance.removeRights Kingside gameState.playerTurn gameState.castlingAllowance
                            | _ -> gameState.castlingAllowance
                        | _ -> gameState.castlingAllowance
                    | _ -> gameState.castlingAllowance
                enpassantCoordinates = 
                    match move with
                    | NormalMove move -> Move.Enpassant.getEnPassantCoordinates move
                    | _ -> None
                halfMoveClock = gameState.halfMoveClock + 1
                fullMoveClock = 
                    match gameState.playerTurn with 
                    | White -> gameState.fullMoveClock + 1
                    | Black -> gameState.fullMoveClock
            }
        let undoMoveSetEnpassantSquare (enpassantCoordinates: Checkerboard.coordinates option) (move: move) (gameState: gameState) =
            Board.Update.undoMove move gameState.board
            {
                board = gameState.board
                playerTurn = Colour.opposite gameState.playerTurn
                castlingAllowance = 
                    match move with
                    | Castling (side, colour) -> CastlingAllowance.addRights side colour gameState.castlingAllowance
                    | _ -> gameState.castlingAllowance
                enpassantCoordinates = enpassantCoordinates
                halfMoveClock = gameState.halfMoveClock - 1
                fullMoveClock = 
                    match gameState.playerTurn with 
                    | White -> gameState.fullMoveClock
                    | Black -> gameState.fullMoveClock - 1
            }
        let undoMove = undoMoveSetEnpassantSquare None
        let makeMoveFromNotation (move: string) (game: gameState) : gameState =
            let parsedMove = MoveParser.parse game.playerTurn game.board move
            makeMove parsedMove game

    let isGameOver (game: gameState) : bool =
        getMoves game |> List.isEmpty
