namespace Chess

open System
open FSharp.Extensions
open Checkerboard

type board = board<piece, int>

module Board =
    
    module Create =
        let private replaceNumbersWithReplicatedOnes =
            Seq.fold (fun acc (c: char) ->
                if (System.Char.IsNumber c) then
                    acc + (String.replicate (int $"{c}") "1")
                else
                    acc + $"{c}"
            ) ""
        let private updateBoardFromFenChar (fenChar: char) (coords: coordinates) (board: board) =
            if fenChar <> '1' then
                let piece = Piece.getFromLetter fenChar |> Some
                Board.Update.Square.withPieceOption coords piece board
        let fromFen (fen: string) : board =
            let board = Board.init 8
            fen
            |> replaceNumbersWithReplicatedOnes
            |> fun fen -> fen.Split('/')
            |> Array.rev
            |> Array.iteri (fun (j: int) (row: string) ->
                row
                |> Seq.iteri (fun (i: int) (c: char) ->
                    updateBoardFromFenChar c (i,j) board
                )
            )
            board
        let starting () : board =
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
            |> fromFen
    
    let private incrementIntegerAtEndOfString (str : string) : string =
        let revStr = Seq.rev str
        let addOne = 
            Seq.head revStr
            |> Char.GetNumericValue
            |> int |> (+) 1 |> string
        (Seq.tail revStr |> Seq.rev |> String.Concat) + addOne

    let private addOrIncrementIntegerAtEndOfString (str: string) =
        if str = "" then
            "1"
        else if Seq.last str |> Char.IsNumber then
            incrementIntegerAtEndOfString str
        else
            str + "1"

    let private addSlashIfEndOfLine ((i, j) : coordinates) (fen: string) : string =
        if i = 7 && j <> 0 then
            fen + "/"
        else
            fen

    /// Converts a chess board setup into a FEN notation string
    let toFen (board: board) : string =
        board
        |> Array2D.foldjiback (fun coords fen square ->
            match square.piece with
            | Some piece ->
                fen + (Piece.getLetter piece |> string)
            | None -> 
                addOrIncrementIntegerAtEndOfString fen
            |> addSlashIfEndOfLine coords
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
                printf " %c " <| Square.toString board.[i,j]
            )
            printfn "|"
        )
        printfn "  \\________________________/"
        printfn "    a  b  c  d  e  f  g  h"
    
    module GetSquares =
        let private stopAt = Some (fun (otherPiece: piece) -> true)
        let private knightVision (coordinates: coordinates) (board: board) : square list =
            Board.GetSquares.afterAllShiftDirections coordinates (1,2) board
        let private bishopVision (coordinates: coordinates) (board: board) : square list =
            Board.GetSquares.afterRepeatedShift (1,1) coordinates stopAt board
            |> List.append <| Board.GetSquares.afterRepeatedShift (1,-1) coordinates stopAt board
            |> List.append <| Board.GetSquares.afterRepeatedShift (-1,1) coordinates stopAt board
            |> List.append <| Board.GetSquares.afterRepeatedShift (-1,-1) coordinates stopAt board
        let private rookVision (coordinates: coordinates) (board: board) : square list =
            Board.GetSquares.afterRepeatedShift (1,0) coordinates stopAt board
            |> List.append <| Board.GetSquares.afterRepeatedShift (-1,0) coordinates stopAt board
            |> List.append <| Board.GetSquares.afterRepeatedShift (0,1) coordinates stopAt board
            |> List.append <| Board.GetSquares.afterRepeatedShift (0,-1) coordinates stopAt board
        let private queenVision (coordinates: coordinates) (board: board) : square list =
            rookVision coordinates board
            |> List.append <| bishopVision coordinates board
        let private kingVision (coordinates: coordinates) (board: board) : square list =
            Board.GetSquares.afterAllShiftDirections coordinates (1,0) board
            |> List.append <| Board.GetSquares.afterAllShiftDirections coordinates (1,1) board
        let pieceVision (square: square) (board: board) : square list =
            let piece, coordinates = Square.getPiece square, square.coordinates
            match piece.pieceType with
                | Knight -> knightVision coordinates board
                | Bishop -> bishopVision coordinates board
                | Rook -> rookVision coordinates board
                | Queen -> queenVision coordinates board
                | King -> kingVision coordinates board
                | Pawn -> Move.PawnMoves.getPawnVision coordinates board piece.colour
        let reverseEngineerPieceLocations (piece: piece) (coordinates: coordinates) (board: board) : square list =
            match piece.pieceType with
                | Knight -> knightVision coordinates board
                | Bishop -> bishopVision coordinates board
                | Rook -> rookVision coordinates board
                | Queen -> queenVision coordinates board
                | King -> kingVision coordinates board
                | Pawn -> Move.PawnMoves.getPawnFrom coordinates piece.colour board
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
            |> Seq.toList
        let playerVision (colour: colour) (board: board) : square list =
            getFromBoardWithPiecesOfColour colour board
            |> List.map (fun oldSquare ->
                GetSquares.pieceVision oldSquare board
            )
            |> List.concat
        let isVisibleByPlayer (colour: colour) (board: board) (square: square) : bool =
            playerVision colour board
            |> List.contains square
        let isAtEndsOfBoard (square: square) : bool =
            List.contains (snd square.coordinates) [0; 7]

    module Move =
        let private blockSelfTaking (square: square) (board: board) (newSquare: square) : bool =
            match Board.GetPiece.fromCoordinates newSquare.coordinates board with
            | Some newPiece -> 
                let piece = Square.getPiece square
                newPiece.colour <> piece.colour
            | None -> true
        let getNormalMoves (colour: colour) (board: board) : normalMove list =
            Square.getFromBoardWithPiecesOfColour colour board
            |> List.map (fun (oldSquare : square<piece, int>) ->
                GetSquares.pieceVision oldSquare board
                |> List.filter (blockSelfTaking oldSquare board)
                |> List.map (fun newSquare -> oldSquare, newSquare)
            )
            |> List.concat

    let isInCheck (colour: colour) (board: board) : bool =
        board
        |> Array2D.tryFind (fun square -> square.piece = Some {pieceType = King; colour = colour})
        |> Option.failOnNone "No king found on the board"
        |> Square.isVisibleByPlayer (Colour.opposite colour) board

    module GetMoves =
        let internal enpassant (colour: colour) (enpassantSquareOption: coordinates option) (board: board) : move list =
            match enpassantSquareOption with
            | None -> []
            | Some enpassantCoordinates -> 
                let direction =
                    match colour with
                    | White -> -1
                    | Black -> 1
                let pos = enpassantCoordinates
                Board.GetSquares.afterShifts pos board [(-1, direction);(+1, direction);]
                |> List.filter (fun square -> 
                    match square.piece with
                    | Some piece when piece.pieceType = Pawn && piece.colour = colour -> true
                    | _ -> false
                )
                |> List.map (fun square ->
                    EnPassant (square, Board.GetSquare.fromCoordinates board pos)
                )
        let internal castling (colour: colour) (castlingOptions: castlingAllowance) (board: board) : move list =
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
                    Some <| Castling (Kingside, colour)
                else
                    None
            let queenSideCastling : move option =
                if queenSide && (castlingChecks [$"e{row}"; $"d{row}"; $"c{row}"] [$"d{row}"; $"c{row}"; $"b{row}"] ($"a{row}") ($"e{row}")) then
                    Some <| Castling (Queenside, colour)
                else
                    None

            [kingSideCastling; queenSideCastling]
            |> List.filter Option.isSome
            |> List.map Option.get
        let internal promotion (board: board) =
            List.map (fun normalMove ->
                if Move.getMovedPieceType normalMove = Pawn && Square.isAtEndsOfBoard (snd normalMove) then
                    [ Queen; Rook; Bishop; Knight ]
                    |> List.map (fun pieceType ->
                        Promotion (normalMove, pieceType)
                    )
                else
                    [NormalMove normalMove]
            ) >> List.concat
        let normal (colour: colour) (board: board) : normalMove list =
            Move.getNormalMoves colour board
            |> List.filter (fun move ->
                Board.applyMove move board
                let isLegalMove = isInCheck colour board |> not
                Board.undoMove move board
                isLegalMove
            )

    module Update =
        let applyEnpassant (move: normalMove) (board: board) =
            Board.applyMove move board
            let coordinates = (snd move).coordinates |> fst, (fst move).coordinates |> snd
            Board.Update.Square.removePiece coordinates board
        let undoEnpassant (move: normalMove) (board: board) =
            Board.undoMove move board
            let coordinates = (snd move).coordinates |> fst, (fst move).coordinates |> snd
            let pawn = {pieceType = Pawn; colour = Move.getMovedPieceColour move |> Colour.opposite}
            Board.Update.Square.withPiece coordinates pawn board
        let applyPromotion (move: normalMove) (promotedPieceType: pieceType) (board: board) =
            Board.applyMove move board
            let promotionCoordinates = (snd move).coordinates
            let colour = Move.getMovedPiece move |> fun piece -> piece.colour
            let promotedPiece = {pieceType = promotedPieceType; colour = colour}
            Board.Update.Square.withPiece promotionCoordinates promotedPiece board
        let private castlingMove (side: side) (colour: colour) (board: board) : normalMove * normalMove =
            let rank = 
                match colour with
                | White -> 0
                | Black -> 7
            let kingStart, kingEnd, rookStart, rookEnd = 
                match side with
                | Kingside -> (4, rank), (6, rank), (7, rank), (5, rank)
                | Queenside -> (4, rank), (2, rank), (0, rank), (3, rank)
            (
                {piece = Some {pieceType = King; colour = colour}; coordinates = kingStart},
                {piece = None; coordinates = kingEnd}
            ),
            (
                {piece = Some {pieceType = Rook; colour = colour}; coordinates = rookStart},
                {piece = None; coordinates = rookEnd}
            )
        let applyCastling (side: side) (colour: colour) (board: board) =
            let kingMove, rookMove = castlingMove side colour board
            Board.applyMove kingMove board
            Board.applyMove rookMove board
        let undoCastling (side: side) (colour: colour) (board: board) =
            let kingMove, rookMove = castlingMove side colour board
            Board.undoMove kingMove board
            Board.undoMove rookMove board
        let applyMove (move: move) (board: board) = 
            match move with
            | Castling (side, colour) -> 
                applyCastling side colour board
            | Promotion (move, promotedPiece) ->
                applyPromotion move promotedPiece board
            | EnPassant move ->
                applyEnpassant move board
            | NormalMove move ->
                Board.applyMove move board
        let undoMove (move: move) (board: board) = 
            match move with
            | Castling (side, colour) -> 
                undoCastling side colour board
            | Promotion (move, _) ->
                Board.undoMove move board
            | EnPassant move ->
                undoEnpassant move board
            | NormalMove move ->
                Board.undoMove move board