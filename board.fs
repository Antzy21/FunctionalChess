namespace Chess

open System
open FSharp.Extensions
open Checkerboard

type board = board<piece>

module Board =
    module Create =
        let fromFen (fen: string) : board =
            let board = Board.Create.empty 8
            fen.Split('/')
            |> Array.rev
            |> Array.iteri (fun (j: int) (row: string) ->
                row
                |> Seq.map (fun (c: char) ->
                    if (System.Char.IsNumber c) then
                        Seq.init (int $"{c}") (fun _ -> None)
                    else
                        seq { Some c }
                )
                |> Seq.concat
                |> Seq.iteri (fun (i: int) (c: char option) ->
                    if Option.isSome c then
                        let piece = c |> Option.get |> Piece.getFromLetter
                        board.[i,j] <- Square.updateWithPiece piece board.[i,j]
                )
            )
            board
        let starting () : board =
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
            |> fromFen
    let print (board : board) : unit =
        printfn "   ________________________"
        printfn "  /                        \\"
        [0..7]
        |> List.rev
        |> List.iter (fun j ->
            printf $"{j+1} |"
            [0..7]
            |> List.iter (fun i ->
                printf " %c " <| Square.print board.[i,j]
            )
            printfn "|"
        )
        printfn "  \\________________________/"
        printfn "    a  b  c  d  e  f  g  h"
    let getPossibleMoves (colour: colour) (board: board) : move list =
        board |> Array2D.filter (fun (square: square) ->
            match square.piece with
            | Some piece when piece.colour = colour -> true
            | _ -> false
        )
        |> List.ofArray
        |> List.map (fun oldSquare ->
            oldSquare
            |> Square.getMoves board
            |> List.map (fun newSquare ->
                (oldSquare, newSquare)
            )
        )
        |> List.concat
    let isInCheck (colour: colour) (board: board) : bool =
        let opponentColour = Colour.opposite colour
        let moves = getPossibleMoves opponentColour board
        moves
        |> List.exists (fun move ->
            match Move.getTakenPiece move with
            | Some piece when piece.pieceType = King -> true
            | _ -> false
        )
    let internal getEnpassantMoves (colour: colour) (enpassantSquareOption: square option) (board: board) : move list =
        match enpassantSquareOption with
        | None -> []
        | Some enpassantSquare -> 
            let direction =
                match colour with
                | White -> -1
                | Black -> 1
            let pos = enpassantSquare.coordinates
            Board.getSquares.afterShifts pos board [(-1, direction);(+1, direction);]
            |> List.filter (fun square -> 
                match square.piece with
                | Some piece when piece.pieceType = Pawn && piece.colour = colour -> true
                | _ -> false
            )
            |> List.map (fun square ->
                (square, board.[fst pos, snd pos])
            )
    let private enpassantMove (move: move) (board: board) : board =
        let board = Board.movePiece (fst move) (snd move) board
        let i, j = (snd move).coordinates |> fst, (fst move).coordinates |> snd
        board[i,j] <- Square.removePiece board[i,j]
        board
    let makeMove (move: move) (board: board) : board = 
        if Move.isEnpassant move then
            enpassantMove move board
        else
            Board.movePiece (fst move) (snd move) board
    let getLegalMoves (colour: colour) (board: board) : move list =
        getPossibleMoves colour board
        |> List.filter (fun move ->
            let newBoardState = Board.movePiece (fst move) (snd move) board
            not <| isInCheck colour newBoardState
        )