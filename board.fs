namespace Chess

open System
open FSharp.Extensions
open Checkerboard

type board = board<piece>

module Board =
    module Create =
        let starting () : board =
            let board = Board.Create.empty 8
            board.[0,1] <- Square.updateWithPiece {pieceType = Pawn; colour = White} board.[0,1]
            board.[1,1] <- Square.updateWithPiece {pieceType = Pawn; colour = White} board.[1,1]
            board.[2,1] <- Square.updateWithPiece {pieceType = Pawn; colour = White} board.[2,1]
            board.[3,1] <- Square.updateWithPiece {pieceType = Pawn; colour = White} board.[3,1]
            board.[4,1] <- Square.updateWithPiece {pieceType = Pawn; colour = White} board.[4,1]
            board.[5,1] <- Square.updateWithPiece {pieceType = Pawn; colour = White} board.[5,1]
            board.[6,1] <- Square.updateWithPiece {pieceType = Pawn; colour = White} board.[6,1]
            board.[7,1] <- Square.updateWithPiece {pieceType = Pawn; colour = White} board.[7,1]
            board.[0,0] <- Square.updateWithPiece {pieceType = Rook; colour = White} board.[0,0]
            board.[1,0] <- Square.updateWithPiece {pieceType = Knight; colour = White} board.[1,0]
            board.[2,0] <- Square.updateWithPiece {pieceType = Bishop; colour = White} board.[2,0]
            board.[3,0] <- Square.updateWithPiece {pieceType = Queen; colour = White} board.[3,0]
            board.[4,0] <- Square.updateWithPiece {pieceType = King; colour = White} board.[4,0]
            board.[5,0] <- Square.updateWithPiece {pieceType = Bishop; colour = White} board.[5,0]
            board.[6,0] <- Square.updateWithPiece {pieceType = Knight; colour = White} board.[6,0]
            board.[7,0] <- Square.updateWithPiece {pieceType = Rook; colour = White} board.[7,0]
            
            board.[0,6] <- Square.updateWithPiece {pieceType = Pawn; colour = Black} board.[0,6]
            board.[1,6] <- Square.updateWithPiece {pieceType = Pawn; colour = Black} board.[1,6]
            board.[2,6] <- Square.updateWithPiece {pieceType = Pawn; colour = Black} board.[2,6]
            board.[3,6] <- Square.updateWithPiece {pieceType = Pawn; colour = Black} board.[3,6]
            board.[4,6] <- Square.updateWithPiece {pieceType = Pawn; colour = Black} board.[4,6]
            board.[5,6] <- Square.updateWithPiece {pieceType = Pawn; colour = Black} board.[5,6]
            board.[6,6] <- Square.updateWithPiece {pieceType = Pawn; colour = Black} board.[6,6]
            board.[7,6] <- Square.updateWithPiece {pieceType = Pawn; colour = Black} board.[7,6]
            board.[0,7] <- Square.updateWithPiece {pieceType = Rook; colour = Black} board.[0,7]
            board.[1,7] <- Square.updateWithPiece {pieceType = Knight; colour = Black} board.[1,7]
            board.[2,7] <- Square.updateWithPiece {pieceType = Bishop; colour = Black} board.[2,7]
            board.[3,7] <- Square.updateWithPiece {pieceType = Queen; colour = Black} board.[3,7]
            board.[4,7] <- Square.updateWithPiece {pieceType = King; colour = Black} board.[4,7]
            board.[5,7] <- Square.updateWithPiece {pieceType = Bishop; colour = Black} board.[5,7]
            board.[6,7] <- Square.updateWithPiece {pieceType = Knight; colour = Black} board.[6,7]
            board.[7,7] <- Square.updateWithPiece {pieceType = Rook; colour = Black} board.[7,7]

            board
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
    let getLegalMoves (colour: colour) (board: board) : move list =
        getPossibleMoves colour board
        |> List.filter (fun move ->
            let newBoardState = Board.movePiece (fst move) (snd move) board
            not <| isInCheck colour newBoardState
        )