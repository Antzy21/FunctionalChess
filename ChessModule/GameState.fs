namespace Chess

open FSharp.Extensions

type gameState = {
    board: Checkerboard.board;
    playerTurn: colour;
    castlingAllowance: castlingAllowance;
    enpassantCoordinates: coordinates option;
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
                | name -> Some (Checkerboard.Coordinates.parse name |> Result.failOnError)
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
    let toString (game: gameState) : string =
        $"{Board.print game.board}\n" +
        $"\nPlayer Turn: {game.playerTurn}" +
        $"\nCastling Allowed: \n{CastlingAllowance.print game.castlingAllowance}" +
        (
            Option.map (fun enpasSqr -> $"\nEnpassantSquare: {enpasSqr}") game.enpassantCoordinates
            |> Option.defaultValue ""
        ) +
        $"\nTurn: {game.fullMoveClock}, Half Turn: {game.halfMoveClock}"
    let print =
        toString >> printfn "%s"
    let getMoves (gameState: gameState) : move list =
        let board = gameState.board
        Board.GetMoves.normal gameState.playerTurn board
        |> Board.GetMoves.promotion board
        |> List.append <| Board.GetMoves.enpassant gameState.playerTurn gameState.enpassantCoordinates board
        |> List.append <| Board.GetMoves.castling gameState.playerTurn gameState.castlingAllowance board
    
    module Update = 
        let makeMove (move: move) (gameState: gameState) : gameState =
            {
                board = Board.Update.applyMove move gameState.board
                playerTurn = Colour.opposite gameState.playerTurn
                castlingAllowance = CastlingAllowance.removeBasedOnMove gameState.playerTurn gameState.castlingAllowance gameState.board move
                enpassantCoordinates = 
                    match move with
                    | NormalMove move -> Move.Enpassant.getEnPassantCoordinates gameState.board move
                    | _ -> None
                halfMoveClock = gameState.halfMoveClock + 1
                fullMoveClock = 
                    match gameState.playerTurn with 
                    | White -> gameState.fullMoveClock + 1
                    | Black -> gameState.fullMoveClock
            }
        let makeMoveFromNotation (move: string) (game: gameState) : gameState =
            let parsedMove = MoveParser.parse game.playerTurn game.board move
            makeMove parsedMove game

    let checkmateOrStatemate (game: gameState) : bool =
        getMoves game |> List.isEmpty
