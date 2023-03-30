﻿namespace Chess

open FSharp.Extensions

type gameState = {
    board: board;
    playerTurn: colour;
    castlingAllowance: castlingAllowance;
    enpassantCoordinates: FSharp.Extensions.coordinates<sbyte> option;
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
                | name -> Some (Checkerboard.Coordinates.parse name)
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
        let deepCopy (gameState: gameState) : gameState =
            {
                board = Array2D.copy gameState.board;
                playerTurn = gameState.playerTurn
                castlingAllowance = gameState.castlingAllowance;
                enpassantCoordinates = gameState.enpassantCoordinates;
                halfMoveClock = gameState.halfMoveClock;
                fullMoveClock = gameState.fullMoveClock;
            }

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
            Board.Update.applyMove move gameState.board
            {
                board = gameState.board
                playerTurn = Colour.opposite gameState.playerTurn
                castlingAllowance = CastlingAllowance.removeBasedOnMove gameState.playerTurn gameState.castlingAllowance move
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
        let undoMoveSetEnpassantSquare (enpassantCoordinates: FSharp.Extensions.coordinates<sbyte> option) (move: move) (gameState: gameState) =
            Board.Update.undoMove move gameState.board
            {
                board = gameState.board
                playerTurn = Colour.opposite gameState.playerTurn
                castlingAllowance = 
                    match move with
                    | Castling (_, colour) -> 
                        gameState.castlingAllowance
                        |> CastlingAllowance.addRights Kingside colour
                        |> CastlingAllowance.addRights Queenside colour
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
