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
        |> Array2D.foldjbacki (fun i j fen square ->
            match square.piece with
            | None -> 
                if fen = "" then
                    "1"
                else if Seq.last fen |> Char.IsNumber then
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
            if i = Array2D.length1 board - 1 && j <> 0 then
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
    
    module GetSquares =
        let private stopAt = Some (fun (otherPiece: piece) -> true)
        let private blockSelfTaking (square: square) (board: board) (newSquare: square) : bool =
            let piece = Square.getPiece square
            let x,y = newSquare.coordinates
            match board.[x,y].piece with
            | Some oldPiece -> oldPiece.colour <> piece.colour
            | None -> true
        let private knightDirection (coordinates: coordinates) (board: board) : square list =
            Board.GetSquares.afterAllShiftDirections coordinates (1,2) board
        let private bishopDirection (coordinates: coordinates) (board: board) : square list =
            Board.GetSquares.onDiagonals coordinates stopAt board
        let private rookDirection (coordinates: coordinates) (board: board) : square list =
            Board.GetSquares.onRowAndFile coordinates stopAt board
        let private queenDirection (coordinates: coordinates) (board: board) : square list =
            Board.GetSquares.onRowFileAndDiagonals coordinates stopAt board
        let private kingDirection (coordinates: coordinates) (board: board) : square list =
            Board.GetSquares.adjacent coordinates board
        let private pieceDirection (piece: piece) (coordinates: coordinates) (board: board) : square list =
            match piece.pieceType with
                | Knight -> knightDirection coordinates board
                | Bishop -> bishopDirection coordinates board
                | Rook -> rookDirection coordinates board
                | Queen -> queenDirection coordinates board
                | King -> kingDirection coordinates board
                | Pawn -> Piece.PawnMoves.getPawnMoves coordinates board piece.colour
        let possibleToMoveToForPieceOnSquare (board: board) (square: square) : square list =
            let piece = square |> Square.getPiece
            pieceDirection piece square.coordinates board
            |> List.filter (blockSelfTaking square board)
        let reverseEngineerPieceLocations (piece: piece) (coordinates: coordinates) (board: board) : square list =
            match piece.pieceType with
                | Knight -> knightDirection coordinates board
                | Bishop -> bishopDirection coordinates board
                | Rook -> rookDirection coordinates board
                | Queen -> queenDirection coordinates board
                | King -> kingDirection coordinates board
                | Pawn -> Piece.PawnMoves.getPawnFrom coordinates piece.colour board
            |> List.filter (fun square ->
                square.piece
                |> Option.filter ((=) piece)
                |> Option.isSome
            )

    module Square =
        let getFromBoardWithPiecesOfColour (colour: colour) (board: board) : square list =
            board |> Array2D.filter (fun (square: square) ->
                match square.piece with
                | Some piece when piece.colour = colour -> true
                | _ -> false
            )
            |> List.ofArray
        let playerVision (colour: colour) (board: board) : square list =
            getFromBoardWithPiecesOfColour colour board
            |> List.map (fun oldSquare ->
                oldSquare
                |> GetSquares.possibleToMoveToForPieceOnSquare board
            )
            |> List.concat
        let isVisibleByPlayer (colour: colour) (board: board) (square: square) : bool =
            playerVision colour board
            |> List.contains square

    module Move =
        let getPossibleMoves (colour: colour) (board: board) : move list =
            Square.getFromBoardWithPiecesOfColour colour board
            |> List.map (fun oldSquare ->
                oldSquare
                |> GetSquares.possibleToMoveToForPieceOnSquare board
                |> List.map (fun newSquare ->
                    (oldSquare, newSquare)
                )
            )
            |> List.concat

    let isInCheck (colour: colour) (board: board) : bool =
        board
        |> Array2D.tryFind (fun square -> square.piece = Some {pieceType = King; colour = colour})
        |> Option.failOnNone "No king found on the board"
        |> Square.isVisibleByPlayer (Colour.opposite colour) board
    let internal getEnpassantMoves (colour: colour) (enpassantSquareOption: square option) (board: board) : move list =
        match enpassantSquareOption with
        | None -> []
        | Some enpassantSquare -> 
            let direction =
                match colour with
                | White -> -1
                | Black -> 1
            let pos = enpassantSquare.coordinates
            Board.GetSquares.afterShifts pos board [(-1, direction);(+1, direction);]
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
            squareThatNeedsKing
            : bool =
            let castlingThroughCheck =
                squaresToInspectForCastlingThroughCheck
                |> List.map (fun name -> Board.GetSquare.fromCoordinatesName name board)
                |> List.fold (fun passingThroughCheck sqr ->
                    passingThroughCheck || 
                    Square.isVisibleByPlayer (Colour.opposite colour) board sqr
                ) false
            let squaresAreEmpty =
                squaresThatMustBeEmpty
                |> List.exists (fun name -> 
                    let square = Board.GetSquare.fromCoordinatesName name board
                    Option.isSome square.piece
                ) |> not
            let rookInPosition = Board.hasPieceOnSquare squareThatNeedsRook {pieceType = Rook; colour = colour} board
            let kingInPosition = Board.hasPieceOnSquare squareThatNeedsKing {pieceType = King; colour = colour} board
                
            let squareKingShouldBeOn =
                Board.GetSquare.fromCoordinatesName $"e{row}" board
            let squareKingShouldBeOn =
                Board.GetSquare.fromCoordinatesName $"e{row}" board
            (not castlingThroughCheck) && squaresAreEmpty && rookInPosition && kingInPosition
        let kingSideCastling : move option = 
            if kingSide && (castlingChecks [$"e{row}"; $"f{row}"; $"g{row}"] [$"f{row}"; $"g{row}"] ($"h{row}") ($"e{row}")) then
                Some (
                    (Board.GetSquare.fromCoordinatesName $"e{row}" board),
                    (Board.GetSquare.fromCoordinatesName $"g{row}" board)
                )
            else
                None
        let queenSideCastling : move option =
            if queenSide && (castlingChecks [$"e{row}"; $"d{row}"; $"c{row}"] [$"d{row}"; $"c{row}"; $"b{row}"] ($"a{row}") ($"e{row}")) then
                let endSquareForKing = $"c{row}"
                Some (
                    (Board.GetSquare.fromCoordinatesName $"e{row}" board),
                    (Board.GetSquare.fromCoordinatesName endSquareForKing board)
                )
            else
                None

        [kingSideCastling; queenSideCastling]
        |> List.filter Option.isSome
        |> List.map Option.get
    let private enpassantMove (move: move) (board: board) : board =
        let board = Board.Update.applyMove move board
        let i, j = (snd move).coordinates |> fst, (fst move).coordinates |> snd
        board[i,j] <- Square.removePiece board[i,j]
        board
    let private castlingMove (move: move) (board: board) : board =
        let i, j = snd move |> Square.getCoordinates
        let board = Board.Update.applyMove move board
        if j = 0 && i = 2 then
            Board.Update.Square.removePiece (0,0) board
            |> Board.Update.Square.withPiece (0,2) {pieceType = Rook; colour = White}
        elif j = 0 && i = 6 then
            Board.Update.Square.removePiece (0,7) board
            |> Board.Update.Square.withPiece (0,6) {pieceType = Rook; colour = White}
        elif j = 7 && i = 2 then
            Board.Update.Square.removePiece (7,0) board
            |> Board.Update.Square.withPiece (7,2) {pieceType = Rook; colour = Black}
        elif j = 7 && i = 6 then
            Board.Update.Square.removePiece (7,7) board
            |> Board.Update.Square.withPiece (7,6) {pieceType = Rook; colour = Black}
        else
            failwith $"Invalid Castling attempted with move {Move.getMoveNotation move}"
        |> Board.Update.Square.removePiece (i,j)
    let makeMove (move: move) (board: board) : board = 
        if Move.isEnpassant move then
            enpassantMove move board
        elif Move.isCastling move then
            castlingMove move board
        else
            Board.Update.applyMove move board
    let getLegalMoves (colour: colour) (board: board) : move list =
        Move.getPossibleMoves colour board
        |> List.filter (fun move ->
            let newBoardState = Board.Update.applyMove move board
            not <| isInCheck colour newBoardState
        )