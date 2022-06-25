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
    let toFen (board: board) : string =
        board
        |> Array2D.foldj (fun i j fen square ->
            match square.piece with
            | None -> 
                if Seq.last fen |> Char.IsNumber then
                    let nef = Seq.rev fen
                    let addOne = 
                        Seq.head nef
                        |> Char.GetNumericValue
                        |> int |> (+) 1 |> string
                    (Seq.tail nef |> Seq.rev |> String.Concat) + addOne
                else
                    fen + "1"
            | Some piece -> fen + (Piece.getLetter piece |> string)
            +
            if i = Array2D.length1 board - 1 && j <> Array2D.length2 board - 1 then
                "/"
            else
                ""
        ) ""
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
    let isInCheck (colour: colour) (board: board) : bool =
        board
        |> Array2D.tryFind (fun square -> square.piece = Some {pieceType = King; colour = colour})
        |> Option.failOnNone "No king found on the board"
        |> Square.playerHasVisionOnSquare (Colour.opposite colour) board
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
    let internal getCastlingMoves (colour: colour) (castlingOptions: castlingAllowance) (board: board) : move list =
        let row, kingSide, queenSide = 
            match colour with
            | White -> 1, castlingOptions.whiteKingside, castlingOptions.whiteQueenside
            | Black -> 8, castlingOptions.blackKingside, castlingOptions.blackQueenside
        let castlingChecks
            squaresToInspectForCastlingThroughCheck
            squaresThatMustBeEmpty
            squareThatNeedsRook
            : bool =
            let castlingThroughCheck =
                squaresToInspectForCastlingThroughCheck
                |> List.map (fun name -> Board.getSquareFromCoordinatesName name board)
                |> List.fold (fun passingThroughCheck sqr ->
                    passingThroughCheck || 
                    Square.playerHasVisionOnSquare (Colour.opposite colour) board sqr
                ) false
            let squaresAreEmpty =
                squaresThatMustBeEmpty
                |> List.exists (fun name -> 
                    let square = Board.getSquareFromCoordinatesName name board
                    Option.isSome square.piece
                ) |> not
            let squareHadRook = 
                Board.getSquareFromCoordinatesName squareThatNeedsRook board
                |> fun square -> square.piece
                |> (=) <| Some {pieceType = Rook; colour = colour}
                
            (not castlingThroughCheck) && squaresAreEmpty && squareHadRook
        let kingSideCastling : move option = 
            if kingSide && (castlingChecks [$"e{row}"; $"f{row}"; $"g{row}"] [$"f{row}"; $"g{row}"] ($"h{row}")) then
                Some (
                    (Board.getSquareFromCoordinatesName $"e{row}" board),
                    (Board.getSquareFromCoordinatesName $"g{row}" board)
                )
            else
                None
        let queenSideCastling : move option =
            if queenSide && (castlingChecks [$"e{row}"; $"d{row}"; $"c{row}"] [$"d{row}"; $"c{row}"; $"b{row}"] ($"h{row}")) then
                let endSquareForKing = $"c{row}"
                Some (
                    (Board.getSquareFromCoordinatesName $"e{row}" board),
                    (Board.getSquareFromCoordinatesName endSquareForKing board)
                )
            else
                None
        let squareKingShouldBeOn =
            Board.getSquare (Coordinates.fromName $"e{row}") board
        if squareKingShouldBeOn.piece <> Some {pieceType = King; colour = colour} then
            failwith "King not on starting square! Error"

        [kingSideCastling; queenSideCastling]
        |> List.filter Option.isSome
        |> List.map Option.get
    let private enpassantMove (move: move) (board: board) : board =
        let board = Board.movePiece move board
        let i, j = (snd move).coordinates |> fst, (fst move).coordinates |> snd
        board[i,j] <- Square.removePiece board[i,j]
        board
    let private castlingMove (move: move) (board: board) : board =
        let i, j = snd move |> Square.getCoordinates
        let board = Board.movePiece move board
        if j = 0 && i = 2 then
            Board.removePiece (0,0) board
            |> Board.updateWithPiece (0,2) {pieceType = Rook; colour = White}
        elif j = 0 && i = 6 then
            Board.removePiece (0,7) board
            |> Board.updateWithPiece (0,6) {pieceType = Rook; colour = White}
        elif j = 7 && i = 2 then
            Board.removePiece (7,0) board
            |> Board.updateWithPiece (7,2) {pieceType = Rook; colour = Black}
        elif j = 7 && i = 6 then
            Board.removePiece (7,7) board
            |> Board.updateWithPiece (7,6) {pieceType = Rook; colour = Black}
        else
            failwith $"Invalid Castling attempted with move {Move.getMoveNotation move}"
        |> Board.removePiece (i,j)
    let makeMove (move: move) (board: board) : board = 
        if Move.isEnpassant move then
            enpassantMove move board
        elif Move.isCastling move then
            castlingMove move board
        else
            Board.movePiece move board
    let getLegalMoves (colour: colour) (board: board) : move list =
        Move.getPossibleMoves colour board
        |> List.filter (fun move ->
            let newBoardState = Board.movePiece move board
            not <| isInCheck colour newBoardState
        )