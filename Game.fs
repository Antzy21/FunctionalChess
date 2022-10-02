namespace Chess

type gameState = {
    board: board;
    playerTurn: colour;
    castlingAllowance: castlingAllowance;
    enpassantCoordinates: (Checkerboard.coordinates option) list;
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
                | "-" -> [None]
                | name -> [Some (Checkerboard.Coordinates.fromName name)]
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
            (List.head game.enpassantCoordinates)
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
        Option.iter (fun enpasSqr -> printfn $"EnpassantSquare: {enpasSqr}") (List.head game.enpassantCoordinates)
        printfn $"Turn: {game.fullMoveClock}, Half Turn: {game.halfMoveClock}"
        
    let getMoves (game: gameState) : move list =
        let board = game.board
        Board.GetMoves.normal game.playerTurn board
        |> Board.GetMoves.promotion board
        |> List.append <| Board.GetMoves.enpassant game.playerTurn (List.head game.enpassantCoordinates) board
        |> List.append <| Board.GetMoves.castling game.playerTurn game.castlingAllowance board
    
    module Update = 
        let makeMove (move: move) (game: gameState) : gameState =
            Board.Update.applyMove move game.board
            {
                board = game.board
                playerTurn = Colour.opposite game.playerTurn
                castlingAllowance = 
                    match move with
                    | Castling (side, colour) -> CastlingAllowance.removeRights side colour game.castlingAllowance
                    | _ -> game.castlingAllowance
                enpassantCoordinates = 
                    match move with
                    | NormalMove move -> Move.Enpassant.getEnPassantCoordinates move
                    | _ -> None
                    :: game.enpassantCoordinates
                halfMoveClock = game.halfMoveClock + 1
                fullMoveClock = 
                    match game.playerTurn with 
                    | White -> game.fullMoveClock + 1
                    | Black -> game.fullMoveClock
            }
        let undoMove (move: move) (game: gameState) : gameState =
            Board.Update.undoMove move game.board
            {
                board = game.board
                playerTurn = Colour.opposite game.playerTurn
                castlingAllowance = 
                    match move with
                    | Castling (side, colour) -> CastlingAllowance.addRights side colour game.castlingAllowance
                    | _ -> game.castlingAllowance
                enpassantCoordinates = 
                    match game.enpassantCoordinates with
                    | [] | _::[] -> [None]
                    | _::t -> t
                halfMoveClock = game.halfMoveClock - 1
                fullMoveClock = 
                    match game.playerTurn with 
                    | White -> game.fullMoveClock
                    | Black -> game.fullMoveClock - 1
            }
        let makeMoveFromNotation (move: string) (game: gameState) : gameState =
            let parsedMove = NotationParser.parse game.playerTurn game.board move
            makeMove parsedMove game

    let isGameOver (game: gameState) : bool =
        getMoves game |> List.isEmpty
