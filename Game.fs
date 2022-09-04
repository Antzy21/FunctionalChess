namespace Chess

type gameState = {
    board: board;
    playerTurn: colour;
    castlingAllowance: castlingAllowance;
    enpassantSquare: square option;
    halfMoveClock: int;
    fullMoveClock: int;
    }

module GameState =
    let fromFen (fen: string) : gameState =
        let parts = fen.Split(' ')
        let board = Board.Create.fromFen(parts[0])
        let playerTurn =
            match parts[1] with
            | "w" -> White
            | "b" -> Black
            | c -> failwith $"Error in FEN: Cannot determine player turn from {c}" 
        let castlingAllowance = CastlingAllowance.fromFen parts[2]
        let enpassantSquare = 
            match parts[3] with
            | "-" -> None
            | name -> Some (Checkerboard.Board.GetSquare.fromCoordinatesName name board)
        let halfMoveClock = int(parts[4])
        let fullMoveClock = int(parts[5])
        {
            board = board;
            playerTurn = playerTurn;
            castlingAllowance = castlingAllowance;
            enpassantSquare = enpassantSquare;
            halfMoveClock = halfMoveClock;
            fullMoveClock = fullMoveClock;
        }
    let toFen (game: gameState) : string =
        let enpassant = game.enpassantSquare |> Option.map Square.getDescription |> Option.defaultValue "-"
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
        
    let newGame () : gameState =
        fromFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
    let getMovesForPlayer (game: gameState) : move list =
        let board = game.board
        Board.GetMoves.normal game.playerTurn board
        |> List.append <| Board.GetMoves.enpassant game.playerTurn game.enpassantSquare board
        |> Board.GetMoves.promotion board
        |> List.append <| Board.GetMoves.castling game.playerTurn game.castlingAllowance board
    let makeMove (move: move) (game: gameState) : gameState =
        Board.Update.applyMove move game.board
        {
            board = game.board
            playerTurn = Colour.opposite game.playerTurn
            castlingAllowance = 
                match move with
                | Castling (side, colour) -> CastlingAllowance.removeRights colour side game.castlingAllowance
                | _ -> game.castlingAllowance
            enpassantSquare = 
                match move with
                | Move move -> Move.getEnPassantSquare move                  
                | _ -> game.enpassantSquare
            halfMoveClock = game.halfMoveClock + 1
            fullMoveClock = 
                match game.playerTurn with 
                | White -> game.fullMoveClock + 1
                | Black -> game.fullMoveClock
        }
    let makeMoveFromNotation (move: string) (game: gameState) : gameState =
        let parsedMove = NotationParser.parse game.playerTurn game.board move
        makeMove parsedMove game
    let print (game: gameState) =
        Board.print game.board
        printfn $"\nPlayer Turn: {game.playerTurn}"
        printf $"Castling Allowed: \n{CastlingAllowance.print game.castlingAllowance}"
        Option.iter (fun enpasSqr -> printfn $"EnpassantSquare: {enpasSqr}") game.enpassantSquare 
        printfn $"Turn: {game.fullMoveClock}, Half Turn: {game.halfMoveClock}"
    let isGameOver (game: gameState) : bool =
        getMovesForPlayer game |> List.isEmpty
