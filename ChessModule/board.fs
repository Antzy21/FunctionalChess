namespace Chess

open FSharp.Extensions
open Checkerboard

module Board =

    /// Initialise a chess board with the starting position
    let createStarting () : board =
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
        |> BoardParser.fromFen

    /// Print an image of the board to the console
    let internal print (board : board) : unit =
        printfn "   ________________________"
        printfn "  /                        \\"

        Board.foldjiback (fun struct (i,j) acc sqr ->
            if i = 0 then
                printf $"{j+1} |"
            match Square.Parser.fromBitMaps sqr with
            | Some piece -> Piece.getLetter piece
            | None -> '.'
            |> printf " %c "
            if i = 7 then
                printfn "|"
            acc
        ) "" board
        |> ignore

        printfn "  \\________________________/"
        printfn "    a  b  c  d  e  f  g  h"
    
    let getSquareFromCoordinates (board: board) (c: coordinates) : piece option =
        let pieceType = 
            if board.KPNRmap &&& c.value > 0UL then
                if board.KQBRmap &&& c.value > 0UL  then
                    if board.KQPmap &&& c.value > 0UL  then
                        Some King
                    else
                        Some Rook
                else
                    if board.KQPmap &&& c.value > 0UL  then
                        Some Pawn
                    else
                        Some Knight
            else
                if board.KQBRmap &&& c.value > 0UL  then
                    if board.KQPmap &&& c.value > 0UL  then
                        Some Queen
                    else
                        Some Bishop
                else
                    None
        pieceType
        |> Option.map (fun pieceType ->
            if board.ColourBitmap &&& c.value > 0UL then
                {pieceType = pieceType; colour = White}
            else
                {pieceType = pieceType; colour = White}
        )
            
    /// Functions for getting the list of coordinates on the board that are visible to the piece on some given coordinates.
    module Vision =
        let private stopAt = PieceBitMap.containsPiece
        let private ofKnight (coordinates: coordinates) (board: board): coordinates list =
            Board.getCoordinatesAfterShiftInAllDirections (1,2) coordinates board
        let private ofBishop (coordinates: coordinates) (board: board) : coordinates list =
            Board.getCoordinatesAfterRepeatedShiftWithStopper (1,1) coordinates stopAt board
            |> List.append <| Board.getCoordinatesAfterRepeatedShiftWithStopper (1,-1) coordinates stopAt board
            |> List.append <| Board.getCoordinatesAfterRepeatedShiftWithStopper (-1,1) coordinates stopAt board
            |> List.append <| Board.getCoordinatesAfterRepeatedShiftWithStopper (-1,-1) coordinates stopAt board
        let private ofRook (coordinates: coordinates) (board: board) : coordinates list =
            Board.getCoordinatesAfterRepeatedShiftWithStopper (1,0) coordinates stopAt board
            |> List.append <| Board.getCoordinatesAfterRepeatedShiftWithStopper (-1,0) coordinates stopAt board
            |> List.append <| Board.getCoordinatesAfterRepeatedShiftWithStopper (0,1) coordinates stopAt board
            |> List.append <| Board.getCoordinatesAfterRepeatedShiftWithStopper (0,-1) coordinates stopAt board
        let private ofQueen (coordinates: coordinates) (board: board) : coordinates list =
            ofRook coordinates board
            |> List.append <| ofBishop coordinates board
        let private ofKing (coordinates: coordinates) (board: board) : coordinates list =
            Board.getCoordinatesAfterShiftInAllDirections (1,1) coordinates board
            |> List.append <| Board.getCoordinatesAfterShiftInAllDirections (1,0) coordinates board
        /// Get a list of coordinates visible from a given coordinates on a board.
        let ofPieceAtCoordsResult (board: board) ((i,j): coordinates) : coordinates list result =
            let square =
                Board.getSquareFromCoordinates board (i,j)
                |> Square.Parser.fromBitMaps
            match square with
            | None -> Error $"No Piece to get vision for at ({i}, {j})"
            | Some piece ->
                match piece.pieceType with
                | Knight -> ofKnight (i,j) board
                | Bishop -> ofBishop (i,j) board
                | Rook -> ofRook (i,j) board
                | Queen -> ofQueen (i,j) board
                | King -> ofKing (i,j) board
                | Pawn ->
                    Move.PawnMoves.getPawnVision (i,j) board piece.colour
                    |> Result.failOnError
                |> Ok
        /// Get a list of coordinates visible from a given coordinates on a board.
        let ofPieceAtCoords (board: board) (coords: coordinates) : coordinates list =
            ofPieceAtCoordsResult board coords |> Result.failOnError

        /// Gets the locations that a piece could have come from given some destination coordinates.
        /// Filters the coordinates list based on if the piece type is on the board at the calculated starting coordinates.
        let internal reverseEngineerPieceLocations (piece: piece) (coordinates: coordinates) (board: board) =
            match piece.pieceType with
                | Knight -> ofKnight coordinates board
                | Bishop -> ofBishop coordinates board
                | Rook -> ofRook coordinates board
                | Queen -> ofQueen coordinates board
                | King -> ofKing coordinates board
                | Pawn -> Move.PawnMoves.getPawnOriginPossibilitiesFromDestination coordinates piece.colour board
            |> List.filter (fun coords ->
                Board.getSquareFromCoordinates board coords
                |> Square.Parser.fromBitMaps
                |> (=) (Some piece)
            )

        /// Is the King visible from the opponent? 
        let internal existsOfKing (oppColour: colour) (board: board) (coordsOfKing: coordinates) : bool =
            ofRook coordsOfKing board
            |> List.exists (fun coords -> 
                let bitMaps = Board.getSquareFromCoordinates board coords
                Square.Parser.fromBitMaps bitMaps
                |> (=) <| Some {pieceType = Rook; colour = oppColour} ||
                Square.Parser.fromBitMaps bitMaps
                |> (=) <| Some {pieceType = Queen; colour = oppColour}
            ) ||
            ofBishop coordsOfKing board
            |> List.exists (fun coords -> 
                let bitMaps = Board.getSquareFromCoordinates board coords
                Square.Parser.fromBitMaps bitMaps
                |> (=) <| Some {pieceType = Bishop; colour = oppColour} ||
                Square.Parser.fromBitMaps bitMaps
                |> (=) <| Some {pieceType = Queen; colour = oppColour}
            ) ||
            ofKnight coordsOfKing board
            |> List.exists (fun coords -> 
                Board.getSquareFromCoordinates board coords
                |> Square.Parser.fromBitMaps
                |> (=) <| Some {pieceType = Knight; colour = oppColour}
            ) ||
            Move.PawnMoves.getPawnVision coordsOfKing board oppColour
            |> Result.map (
                List.exists (fun coords -> 
                    Board.getSquareFromCoordinates board coords
                    |> Square.Parser.fromBitMaps
                    |> (=) <| Some {pieceType = Pawn; colour = oppColour}
                )
            ) |> Result.defaultValue false

    let private playerVision (colour: colour) (board: board) : coordinates list =
        board
        |> Board.filterCoordinates (fun sqr ->
            Square.BitMap.isColour colour sqr
        )
        |> List.map (Vision.ofPieceAtCoords board)
        |> List.concat
    let internal isVisibleByPlayer (colour: colour) (board: board) (coords: coordinates) : bool =
        playerVision colour board
        |> List.contains coords

    /// See if the coloured player is in check on the board
    let isInCheck (colour: colour) (board: board) : bool =
        board
        |> Board.tryFindCoordinates (fun squareBitMaps -> squareBitMaps = (Some {pieceType = King; colour = colour} |> Square.Parser.toBitMaps))
        |> Option.failOnNone "No king found on the board"
        |> Vision.existsOfKing (Colour.opposite colour) board

    let containsPieceResult (coords: coordinates) (board: board) : bool result =
        Board.getSquareFromCoordinatesResult board coords
        |> Result.map Square.BitMap.containsPiece
    let containsPiece (coords: coordinates) (board: board) : bool =
        containsPieceResult coords board |> Result.failOnError

    module Update =
        let internal removePiece (coords: coordinates) (board: board) : board =
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
        let internal applyMove (move: move) (board: board) = 
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
        let private filterOutSameColouredPieces (pieceColour: colour) (board: board) (coordsList: coordinates list) : coordinates list =
            coordsList
            |> List.filter (fun coords -> 
                Board.getSquareFromCoordinates board coords
                |> Square.BitMap.containsColouredPiece pieceColour
                |> not
            )
        let private pseudoLegal (colour: colour) (board: board) : normalMove list =
            board
            |> Board.filterCoordinates (Square.BitMap.containsColouredPiece colour)
            |> List.map (fun oldCoords ->
                Vision.ofPieceAtCoords board oldCoords
                |> filterOutSameColouredPieces colour board
                |> List.map (fun newCoords -> {startingCoords = oldCoords; destinationCoords = newCoords})
            )
            |> List.concat
        let internal enpassant (colour: colour) (enpassantSquareOption: coordinates option) (board: board) : move list =
            match enpassantSquareOption with
            | None -> []
            | Some enpassantCoordinates -> 
                let direction =
                    match colour with
                    | White -> -1
                    | Black -> 1
                let pos = enpassantCoordinates
                Board.getCoordinatesAfterShifts pos board [(-1, direction);(+1, direction);]
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
                        isVisibleByPlayer (Colour.opposite colour) board coords
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
                let isAtEndOfBoard = 
                    List.contains (normalMove.destinationCoords |> fun struct (x,y) -> y) [0; 7]
                if movedPieceIsPawn && isAtEndOfBoard then
                    [ Queen; Rook; Bishop; Knight ]
                    |> List.map (fun pieceType ->
                        Promotion (normalMove, pieceType)
                    )
                else
                    [NormalMove normalMove]
            ) >> List.concat
        let internal normal (colour: colour) (board: board) : normalMove list =
            pseudoLegal colour board
            |> List.filter (fun move ->
                Update.applyNormalMove move board
                |> isInCheck colour
                |> not
            )
        let internal asyncNormal (colour: colour) (board: board) : normalMove list =
            [
                for move in List.toSeq (pseudoLegal colour board) do
                    async {
                        let inCheck =
                            Update.applyNormalMove move board
                            |> isInCheck colour
                        if inCheck then
                            return None
                        else
                            return (Some move)
                    }
            ]
            |> fun comp -> Async.Parallel(comp, 8)
            |> Async.StartAsTask
            |> (fun task ->
                task.Wait()
                task.Result
            )
            |> Seq.filterSome
            |> List.ofSeq
