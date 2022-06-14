namespace Chess

open Checkerboard

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
            | name -> Some (Board.getSquareFromCoordinatesName name board)
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
    let getMovesForPlayer (game: gameState) : move list =
        let board = game.board
        Board.getLegalMoves game.playerTurn board
        |> List.append <| Board.getEnpassantMoves game.playerTurn game.enpassantSquare board
        |> List.append <| Board.getCastlingMoves game.playerTurn game.castlingAllowance board
    let makeMove (move: move) (game: gameState) : gameState =
        {
            board = Board.makeMove move game.board
            playerTurn = Colour.opposite game.playerTurn
            castlingAllowance = 
                if Move.isCastling move then
                    CastlingAllowance.removeBasedOnMove move game.castlingAllowance
                else
                    game.castlingAllowance
            enpassantSquare = None;
            halfMoveClock = game.halfMoveClock + 1
            fullMoveClock = 
                match game.playerTurn with 
                | White -> game.fullMoveClock + 1
                | Black -> game.fullMoveClock
        }