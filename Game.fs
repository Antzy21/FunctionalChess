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
        let castlingAllowance = 
            {
                whiteKingside = parts[2].Contains('K');
                whiteQueenside = parts[2].Contains('Q');
                blackKingside = parts[2].Contains('k');
                blackQueenside = parts[2].Contains('q')
            }
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
    let getMovesForPlayer (player: colour) (game: gameState) : move list =
        let board = game.board
        Board.getLegalMoves player board
        |> List.append <| Board.getEnpassantMoves player game.enpassantSquare board
        |> List.append <| Board.getCastlingMoves player game.castlingAllowance board