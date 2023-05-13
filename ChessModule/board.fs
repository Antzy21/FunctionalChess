namespace Chess

open System
open FSharp.Extensions
open Checkerboard

module Board =
    
    module Create =
        let private replaceNumbersWithReplicatedOnes =
            Seq.fold (fun acc (c: char) ->
                if (System.Char.IsNumber c) then
                    acc + (String.replicate (int $"{c}") "1")
                else
                    acc + $"{c}"
            ) ""
        let private updateBoardFromFenChar (fenChar: char) (coords: coordinates) (board: board) : board =
            if fenChar <> '1' then
                let piece = Piece.getFromLetter fenChar |> Some |> Square.Parser.toBitMaps
                Board.updateSquare coords piece board
            else
                board
        let fromFen (fen: string) : board =
            let board = Board.init 5
            fen
            |> replaceNumbersWithReplicatedOnes
            |> fun fen -> fen.Split('/')
            |> Array.rev
            |> Array.fold (fun (j, board) (row: string) ->
                row
                |> Seq.fold (fun (((i,j), b): coordinates * board) (c: char) ->
                    (i+1,j), (updateBoardFromFenChar c (i,j) b)
                ) ((0,j), board)
                |> fun ((_,j), board) -> (j+1, board)
            ) (0, board)
            |> snd
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
        |> Board.foldjiback (fun coords fen squareBitMap ->
            match Square.Parser.fromBitMaps squareBitMap with
            | Some piece ->
                fen + (Piece.getLetter piece |> string)
            | None -> 
                addOrIncrementIntegerAtEndOfString fen
            |> addSlashIfEndOfLine coords
        ) ""

    let print (board : board) : unit =
        printfn "   ________________________"
        printfn "  /                        \\"

        Board.foldjiback (fun struct (i,j) acc sqr ->
            if i = 0 then
                printf $"{j+1} |"
            Square.Parser.fromBitMaps sqr
            |> Square.toString
            |> printf " %c "
            if i = 7 then
                printfn "|"
            acc
        ) "" board
        |> ignore

        printfn "  \\________________________/"
        printfn "    a  b  c  d  e  f  g  h"
    
    module GetSquares =
        let private stopAt = PieceBitMap.containsPiece
        let private knightVision (coordinates: coordinates) (board: board): coordinates list =
            Board.GetCoordinates.getAfterShiftInAllDirections (1,2) coordinates board
        let private bishopVision (coordinates: coordinates) (board: board) : coordinates list =
            Board.GetCoordinates.afterRepeatedShiftWithStopper (1,1) coordinates stopAt board
            |> List.append <| Board.GetCoordinates.afterRepeatedShiftWithStopper (1,-1) coordinates stopAt board
            |> List.append <| Board.GetCoordinates.afterRepeatedShiftWithStopper (-1,1) coordinates stopAt board
            |> List.append <| Board.GetCoordinates.afterRepeatedShiftWithStopper (-1,-1) coordinates stopAt board
        let private rookVision (coordinates: coordinates) (board: board) : coordinates list =
            Board.GetCoordinates.afterRepeatedShiftWithStopper (1,0) coordinates stopAt board
            |> List.append <| Board.GetCoordinates.afterRepeatedShiftWithStopper (-1,0) coordinates stopAt board
            |> List.append <| Board.GetCoordinates.afterRepeatedShiftWithStopper (0,1) coordinates stopAt board
            |> List.append <| Board.GetCoordinates.afterRepeatedShiftWithStopper (0,-1) coordinates stopAt board
        let private queenVision (coordinates: coordinates) (board: board) : coordinates list =
            rookVision coordinates board
            |> List.append <| bishopVision coordinates board
        let private kingVision (coordinates: coordinates) (board: board) : coordinates list =
            Board.GetCoordinates.getAfterShiftInAllDirections (1,1) coordinates board
            |> List.append <| Board.GetCoordinates.getAfterShiftInAllDirections (1,0) coordinates board
        let pieceVisionResult (board: board) ((i,j): coordinates) : coordinates list result =
            let square =
                Board.getSquareFromCoordinates board (i,j)
                |> Square.Parser.fromBitMaps
            match square with
            | None -> Error $"No Piece to get vision for at ({i}, {j})"
            | Some piece ->
                match piece.pieceType with
                | Knight -> knightVision (i,j) board
                | Bishop -> bishopVision (i,j) board
                | Rook -> rookVision (i,j) board
                | Queen -> queenVision (i,j) board
                | King -> kingVision (i,j) board
                | Pawn -> Move.PawnMoves.getPawnVision (i,j) board piece.colour
                |> Ok
        let pieceVision (board: board) (coords: coordinates) : coordinates list =
            pieceVisionResult board coords |> Result.failOnError

        /// Gets the locations that a piece could have come from given some destination coordinates.
        /// Filters the coordinates list based on if the piece type is on the board at the calculated starting coordinates.
        let reverseEngineerPieceLocations (piece: piece) (coordinates: coordinates) (board: board) =
            match piece.pieceType with
                | Knight -> knightVision coordinates board
                | Bishop -> bishopVision coordinates board
                | Rook -> rookVision coordinates board
                | Queen -> queenVision coordinates board
                | King -> kingVision coordinates board
                | Pawn -> Move.PawnMoves.getPawnOriginPossibilitiesFromDestination coordinates piece.colour board
            |> List.filter (fun coords ->
                Board.getSquareFromCoordinates board coords
                |> Square.Parser.fromBitMaps
                |> (=) (Some piece)
            )

    module Square =
        let internal playerVision (colour: colour) (board: board) : coordinates list =
            board
            |> Board.filterCoordinates (fun sqr ->
                Square.BitMap.isColour colour sqr
            )
            |> List.map (GetSquares.pieceVision board)
            |> List.concat
        let internal isVisibleByPlayer (colour: colour) (board: board) (coords: coordinates) : bool =
            playerVision colour board
            |> List.contains coords
        let isAtEndsOfBoard (coords: coordinates) : bool =
            List.contains (coords |> fun struct (x,y) -> y) [0; 7]

    module Move =
        let private filterOutSameColouredPieces (pieceColour: colour) (board: board) (coordsList: coordinates list) : coordinates list =
            coordsList
            |> List.filter (fun coords -> 
                Board.getSquareFromCoordinates board coords
                |> Square.BitMap.containsColouredPiece pieceColour
                |> not
            )
        let getNormalMoves (colour: colour) (board: board) : normalMove list =
            board
            |> Board.filterCoordinates (Square.BitMap.containsColouredPiece colour)
            |> List.map (fun oldCoords ->
                GetSquares.pieceVision board oldCoords
                |> filterOutSameColouredPieces colour board
                |> List.map (fun newCoords -> {startingCoords = oldCoords; destinationCoords = newCoords})
            )
            |> List.concat

    let isInCheck (colour: colour) (board: board) : bool =
        board
        |> Board.tryFindCoordinates (fun squareBitMaps -> squareBitMaps = (Some {pieceType = King; colour = colour} |> Square.Parser.toBitMaps))
        |> Option.failOnNone "No king found on the board"
        |> Square.isVisibleByPlayer (Colour.opposite colour) board

    let containsPieceResult (coords: coordinates) (board: board) : bool result =
        Board.getSquareFromCoordinatesResult board coords
        |> Result.map Square.BitMap.containsPiece
    let containsPieceOption (coords: coordinates) (board: board) : bool option =
        containsPieceResult coords board |> Result.toOption
    let containsPiece (coords: coordinates) (board: board) : bool =
        containsPieceResult coords board |> Result.failOnError

    module Update =
        let removePiece (coords: coordinates) (board: board) : board =
            Board.updateSquare coords (Square.Parser.toBitMaps None) board
        let internal applyNormalMove (move: normalMove) (board: board) : board =
            let square = Board.getSquareFromCoordinates board move.startingCoords
            Board.updateSquare move.destinationCoords square board
            |> removePiece move.startingCoords
        let private applyEnpassant (move: normalMove) (board: board) : board =
            let coordinatesOfPawnToBeRemoved = 
                move.destinationCoords |> fun struct (x,y) -> x, move.startingCoords |> fun struct (x,y) -> y
                |> (fun (x,y) -> (struct (x,y)))
            applyNormalMove move board
            |> removePiece coordinatesOfPawnToBeRemoved
        let private applyPromotion (move: normalMove) (promotedPieceType: pieceType) (board: board) =
            let colour = 
                Board.getSquareFromCoordinates board move.startingCoords
                |> Square.getPieceColour
                |> Option.get
            let promotedPiece = {pieceType = promotedPieceType; colour = colour}
            applyNormalMove move board
            |> Board.updateSquare move.destinationCoords (Square.Parser.toBitMaps (Some promotedPiece))
        let private getMovesForCastling (side: side) (colour: colour) : normalMove * normalMove =
            let rank = 
                match colour with
                | White -> 0
                | Black -> 7
            let kingStart, kingEnd, rookStart, rookEnd = 
                match side with
                | Kingside -> struct (4, rank), struct (6, rank), struct (7, rank), struct (5, rank)
                | Queenside -> struct (4, rank), struct (2, rank), struct (0, rank), struct (3, rank)
            {startingCoords = kingStart; destinationCoords = kingEnd},
            {startingCoords = rookStart; destinationCoords = rookEnd}
        let private applyCastling (side: side) (colour: colour) (board: board) : board =
            let kingMove, rookMove = getMovesForCastling side colour
            board
            |> applyNormalMove kingMove
            |> applyNormalMove rookMove
        let applyMove (move: move) (board: board) = 
            match move with
            | Castling (side, colour) -> 
                applyCastling side colour board
            | Promotion (move, promotedPiece) ->
                applyPromotion move promotedPiece board
            | EnPassant move ->
                applyEnpassant move board
            | NormalMove move ->
                applyNormalMove move board
    
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
                Board.GetCoordinates.afterShifts pos board [(-1, direction);(+1, direction);]
                |> List.filter (fun coords ->
                    Board.getSquareFromCoordinates board coords
                    |> Square.BitMap.contains {pieceType = Pawn; colour = colour}
                )
                |> List.map (fun coordsOfPawnDoingEnPassant ->
                    EnPassant {startingCoords = coordsOfPawnDoingEnPassant; destinationCoords = enpassantCoordinates}
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
                    |> List.map (fun name -> (Coordinates.tryParse name).Value)
                    |> List.fold (fun passingThroughCheck coords ->
                        passingThroughCheck || 
                        Square.isVisibleByPlayer (Colour.opposite colour) board coords
                    ) false
                let squaresAreEmpty =
                    squaresThatMustBeEmpty
                    |> List.forall (fun name -> 
                        let coords = (Coordinates.tryParse name).Value
                        Board.getSquareFromCoordinates board coords
                        |> Square.BitMap.containsPiece
                        |> not
                    )
                let rookInPosition =
                    Coordinates.parse squareThatNeedsRook
                    |> Result.bind (Board.getSquareFromCoordinatesResult board)
                    |> Result.failOnError
                    |> (=) (Square.Parser.toBitMaps <| Some {pieceType = Rook; colour = colour})
                let kingInPosition =
                    Coordinates.parse squareThatNeedsKing
                    |> Result.bind (Board.getSquareFromCoordinatesResult board)
                    |> Result.failOnError
                    |> (=) (Square.Parser.toBitMaps <| Some {pieceType = King; colour = colour})
                
                //let squareKingShouldBeOn =
                //    Board.GetSquare.fromCoordinatesName $"e{row}" board
                //let squareKingShouldBeOn =
                //    Board.GetSquare.fromCoordinatesName $"e{row}" board
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
                let movedPieceIsPawn =
                    Board.getSquareFromCoordinates board normalMove.startingCoords
                    |> Square.BitMap.containsPieceOfType Pawn
                if movedPieceIsPawn && Square.isAtEndsOfBoard normalMove.destinationCoords then
                    [ Queen; Rook; Bishop; Knight ]
                    |> List.map (fun pieceType ->
                        Promotion (normalMove, pieceType)
                    )
                else
                    [NormalMove normalMove]
            ) >> List.concat
        let internal normal (colour: colour) (board: board) : normalMove list =
            Move.getNormalMoves colour board
            |> List.filter (fun move ->
                Update.applyNormalMove move board
                |> isInCheck colour
                |> not
            )
