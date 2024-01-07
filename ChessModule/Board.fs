namespace Chess

open Checkerboard
open FSharp.Extensions

module Board =

    /// Initialise a chess board with the starting position
    let constructStarting () : board =
        { ColourBitmap = 65535UL; KQPmap = 1801158375971553048UL; KQBRmap = 13618885273168380093UL; KPNRmap = 15275928461064077267UL }

    /// Initialise an empty chess board
    let construct () : board =
        {ColourBitmap = 0UL; KPNRmap = 0UL; KQBRmap = 0UL; KQPmap = 0UL }

    let getSquareFromCoordinates (board: board) (c: coordinates) : piece option =
        let pieceType = 
            if BitMap.getValueAtCoordinates c board.KPNRmap then
                if BitMap.getValueAtCoordinates c board.KQBRmap then
                    if BitMap.getValueAtCoordinates c board.KQPmap then
                        Some King
                    else
                        Some Rook
                else
                    if BitMap.getValueAtCoordinates c board.KQPmap then
                        Some Pawn
                    else
                        Some Knight
            else
                if BitMap.getValueAtCoordinates c board.KQBRmap then
                    if BitMap.getValueAtCoordinates c board.KQPmap then
                        Some Queen
                    else
                        Some Bishop
                else
                    None
        pieceType
        |> Option.map (fun pieceType ->
            if BitMap.getValueAtCoordinates c board.ColourBitmap then
                {pieceType = pieceType; colour = White}
            else
                {pieceType = pieceType; colour = Black}
        )

    /// Builds a board of the one given piece at given coordinates
    let constructForPieceAtCoords (c : coordinates) (piece: piece) : board =
        let colourBitMap = 
            match piece.colour with
            | White -> c
            | Black -> 0UL
        match piece.pieceType with
        | King -> 
            {
                ColourBitmap = colourBitMap;
                KPNRmap = c;
                KQBRmap = c;
                KQPmap = c;
            }
        | Queen -> 
            {
                ColourBitmap = colourBitMap;
                KPNRmap = 0UL;
                KQBRmap = c;
                KQPmap = c;
            }
        | Pawn -> 
            {
                ColourBitmap = colourBitMap;
                KPNRmap = c;
                KQBRmap = 0UL;
                KQPmap = c;
            }
        | Bishop -> 
            {
                ColourBitmap = colourBitMap;
                KPNRmap = 0UL;
                KQBRmap = c;
                KQPmap = 0UL;
            }
        | Knight -> 
            {
                ColourBitmap = colourBitMap;
                KPNRmap = c;
                KQBRmap = 0UL;
                KQPmap = 0UL;
            }
        | Rook -> 
            {
                ColourBitmap = colourBitMap;
                KPNRmap = c;
                KQBRmap = c;
                KQPmap = 0UL;
            }

    /// Folds the array, starting in the top right and moving down.
    let foldjiback (folder: coordinates -> 'S -> square -> 'S) (state: 'S) (board: board)=
        [0..7] |> List.rev
        |> List.fold (fun accRow j ->
            [0..7]
            |> List.fold (fun acc i ->
                let c = Coordinates.construct i j |> Result.failOnError
                getSquareFromCoordinates board c
                |> folder c acc
            ) accRow
        ) state

    /// Print an image of the board to the console
    let internal toString (board : board) : string =
        "   ________________________\n" +
        "  /                        \\\n" +
        foldjiback (fun c acc sqr ->
            acc +
            if Coordinates.getFile c = 0 then
                $"{Coordinates.getRow c + 1} |"
            else ""
            +
            match sqr with
            | Some piece -> $" {Piece.getUnicodeChar piece} "
            | None -> " . "
            +
            if Coordinates.getFile c = 7 then
                "|\n"
            else ""
        ) "" board
        +
        "  \\________________________/" +
        "\n    a  b  c  d  e  f  g  h"
        
    /// Functions for getting the list of coordinates on the board that are visible to the piece on some given coordinates.
    module Vision =
        let private getPawnMovementDirection (pieceColour: colour) =
            match pieceColour with
            | White -> 1
            | Black -> -1
        let private getPawnStartingRow (pieceColour: colour) =
            match pieceColour with
            | White -> 1
            | Black -> 6

        let internal ofPawnDiagonal (start: coordinates) (board: board) (pieceColour: colour) (direction: int) =
            CoordinatesCollection.construct ()
            |> CoordinatesCollection.appendResult (Coordinates.shift start 1 direction)
            |> CoordinatesCollection.appendResult (Coordinates.shift start -1 direction)
        let private ofPawn (start: coordinates) (board: board) (pieceColour: colour) : coordinatesCollection =
            let direction = getPawnMovementDirection pieceColour
            let startingRow = getPawnStartingRow pieceColour
            let diagonalMoves =
                ofPawnDiagonal start board pieceColour direction
                |> fun coordinatesCollection ->
                    match pieceColour with
                    | White -> coordinatesCollection &&& board.blackPieces
                    | Black -> coordinatesCollection &&& board.whitePieces
            let forwardMoves = 
                // Pawns will never be at the end of the board so this should not fail
                let pawn1ForwardCoords = Coordinates.shift start 0 direction |> Result.failOnError
                // If square in front is occupied, no moves forward are possible
                if BitMap.getValueAtCoordinates pawn1ForwardCoords board.pieceMap then
                    CoordinatesCollection.construct ()
                // Else, if at the starting square then two moves forward are possible
                elif (startingRow = Coordinates.getRow start) then
                    // This should not error as coordinates are on the starting row
                    let pawn2ForwardCoords = Coordinates.shift start 0 (direction*2) |> Result.failOnError
                    // Check the second square is empty, and possible to move to
                    if BitMap.getValueAtCoordinates pawn2ForwardCoords board.pieceMap |> not then
                        pawn1ForwardCoords ||| pawn2ForwardCoords
                    else
                        pawn1ForwardCoords
                // Otherwise just the one space forward is possible.
                else
                    pawn1ForwardCoords
            diagonalMoves ||| forwardMoves

        // Get Pawn origin possibilities from destination
        let internal reverseOfPawn (destination: coordinates) (pieceColour: colour) (board: board): coordinatesCollection =
            let direction = getPawnMovementDirection pieceColour
            CoordinatesCollection.construct ()
            |> CoordinatesCollection.appendResult (Coordinates.shift destination -1 (-direction))
            |> CoordinatesCollection.appendResult (Coordinates.shift destination  1 (-direction))
            |> CoordinatesCollection.appendResult (Coordinates.shift destination  0 (-direction))
            |> CoordinatesCollection.appendResult (Coordinates.shift destination  0 (-direction*2))

        /// Get all coordinates after repeating a shift, up to and including when a piece is on the next coordinates.
        let rec private afterRepeatedShift (xShift: int) (yShift: int) (start: coordinates) (board: board) : coordinatesCollection =
            Coordinates.shift start xShift yShift 
            |> Result.map (fun coords ->
                if BitMap.getValueAtCoordinates coords board.pieceMap then
                    coords
                else
                    coords ||| afterRepeatedShift xShift yShift coords board
            )
            |> Result.defaultValue (CoordinatesCollection.construct ())

        let private ofKnight (c: coordinates) (board: board) : coordinatesCollection =
            let i = Coordinates.getFile c
            let j = Coordinates.getRow c
            CoordinatesCollection.construct ()
            |> CoordinatesCollection.appendResult (Coordinates.construct (i+1) (j+2))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i-1) (j-2))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i-1) (j+2))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i+1) (j-2))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i+2) (j+1))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i-2) (j-1))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i-2) (j+1))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i+2) (j-1))
        let private ofBishop (c: coordinates) (board: board) : coordinatesCollection =
            afterRepeatedShift 1 1 c board |||
            afterRepeatedShift 1 -1 c board |||
            afterRepeatedShift -1  1 c board |||
            afterRepeatedShift -1 -1 c board
        let private ofRook (c: coordinates) (board: board) : coordinatesCollection =
            afterRepeatedShift 1 0 c board |||
            afterRepeatedShift -1 0 c board |||
            afterRepeatedShift 0 1 c board |||
            afterRepeatedShift 0 -1 c board
        let private ofQueen (c: coordinates) (board: board) : coordinatesCollection =
            ofRook c board ||| ofBishop c board
        let private ofKing (c: coordinates) (board: board) : coordinatesCollection =
            let i = Coordinates.getFile c
            let j = Coordinates.getRow c
            CoordinatesCollection.construct ()
            |> CoordinatesCollection.appendResult (Coordinates.construct (i+1) (j+1))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i+1) (j+0))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i+1) (j-1))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i+0) (j+1))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i+0) (j-1))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i-1) (j+1))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i-1) (j+0))
            |> CoordinatesCollection.appendResult (Coordinates.construct (i-1) (j-1))
        /// Get a list of coordinates visible from a given coordinates on a board.
        let ofPieceAtCoords (board: board) (c: coordinates) : coordinatesCollection result =
            match getSquareFromCoordinates board c with
            | None -> Error $"No piece at position {Coordinates.getName c}"
            | Some piece ->
                match piece.pieceType with
                | Knight -> ofKnight c board
                | Bishop -> ofBishop c board
                | Rook -> ofRook c board
                | Queen -> ofQueen c board
                | King -> ofKing c board
                | Pawn -> ofPawn c board piece.colour
                |> Ok

        /// Gets the locations that a piece could have come from given some destination coordinates.
        /// Filters the coordinates list based on if the piece type is on the board at the calculated starting coordinates.
        let internal reverseEngineerPieceLocations (piece: piece) (coordinates: coordinates) (board: board) =
            let colouredPieceMap =
                match piece.colour with
                | White -> board.whitePieces
                | Black -> board.blackPieces
            match piece.pieceType with
                | Knight -> ofKnight coordinates board |> (&&&) board.knightMap
                | Bishop -> ofBishop coordinates board |> (&&&) board.bishopMap
                | Rook -> ofRook coordinates board |> (&&&) board.rookMap
                | Queen -> ofQueen coordinates board |> (&&&) board.queenMap
                | King -> ofKing coordinates board |> (&&&) board.kingMap
                | Pawn -> reverseOfPawn coordinates piece.colour board |> (&&&) board.pawnMap
            &&& colouredPieceMap

        /// Is the King visible from the opponent? 
        let internal playerHasVisionOfKing (player: colour) (board: board) (coordsOfKing: coordinates) : bool =
            let normalPieceCanSeeKing =
                match player with
                | White -> 
                    ofRook coordsOfKing board &&& (board.whiteQueenMap ||| board.whiteRookMap) > 0UL ||
                    ofBishop coordsOfKing board &&& (board.whiteQueenMap ||| board.whiteBishopMap) > 0UL ||
                    ofKnight coordsOfKing board &&& board.whiteKnightMap > 0UL ||
                    ofKing coordsOfKing  board &&& board.whiteKingMap > 0UL
                | Black -> 
                    ofRook coordsOfKing board &&& (board.blackQueenMap ||| board.blackRookMap) > 0UL ||
                    ofBishop coordsOfKing board &&& (board.blackQueenMap ||| board.blackBishopMap) > 0UL ||
                    ofKnight coordsOfKing board &&& board.blackKnightMap > 0UL ||
                    ofKing coordsOfKing  board &&& board.blackKingMap > 0UL
            
            let pawnPieceCanSeeKing =
                let direction = getPawnMovementDirection player
                ofPawnDiagonal coordsOfKing board player -direction
                |> fun coordinatesCollection ->
                    match player with
                    | White -> coordinatesCollection &&& board.whitePawnMap
                    | Black -> coordinatesCollection &&& board.blackPawnMap
                > 0UL
            
            normalPieceCanSeeKing
            ||
            pawnPieceCanSeeKing

    let private playerVision (colour: colour) (board: board) : coordinatesCollection =
        match colour with
        | White -> board.whitePieces
        | Black -> board.blackPieces
        |> BitMap.isolateValues
        // IsolateValues returns a bitMap, not coordinates explicitly. So a quick conversion is required.
        |> List.fold (fun acc c ->
            Vision.ofPieceAtCoords board c 
            |> Result.failOnError
            ||| acc
        ) (CoordinatesCollection.construct ())
    let internal isVisibleByPlayer (colour: colour) (board: board) (coords: coordinates) : bool =
        playerVision colour board &&& coords > 0UL

    /// See if the coloured player is in check on the board
    let isInCheck (colour: colour) (board: board) : bool =
        match colour with
        | White -> board.whiteKingMap
        | Black-> board.blackKingMap
        |> Vision.playerHasVisionOfKing (Colour.opposite colour) board

    /// Gets the optional coordinates that a pawn could be taken through an "en passant" move, that only comes by the previous move being a pawn moving two squares.
    let internal getEnPassantCoordinates (board: board) (move: normalMove) : coordinates option = 
        let pieceMovedTwoSquaresVertically =
            abs(Coordinates.getRow move.startingCoords - Coordinates.getRow move.destinationCoords) = 2
        // Option.get because if there is no piece at the start of the move, there is an error.
        let pieceAtStartOfMove = getSquareFromCoordinates board move.startingCoords |> Option.get
        let movedPieceWasPawn = pieceAtStartOfMove.pieceType = Pawn
        if movedPieceWasPawn && pieceMovedTwoSquaresVertically then
            match pieceAtStartOfMove.colour with
            | White ->
                Coordinates.shift move.startingCoords 0 1
            | Black -> 
                Coordinates.shift move.startingCoords 0 -1
            // This coordinates shift should never fail
            |> Result.failOnError
            |> Some
        else
            None

    module Update =
        let private removePiece (coords: coordinates) (board: board) : board =
            {
                ColourBitmap = BitMap.setValueAtCoordinates false coords board.ColourBitmap;
                KPNRmap = BitMap.setValueAtCoordinates false coords board.KPNRmap;
                KQBRmap = BitMap.setValueAtCoordinates false coords board.KQBRmap;
                KQPmap = BitMap.setValueAtCoordinates false coords board.KQPmap;
            }
        let updateSquare (piece: piece) (c: coordinates) (board: board) : board =
            let boardOfNewSquare = constructForPieceAtCoords c piece 
            board 
            |> removePiece c
            |> fun b ->
                {
                    ColourBitmap = boardOfNewSquare.ColourBitmap ||| b.ColourBitmap;
                    KPNRmap = boardOfNewSquare.KPNRmap ||| b.KPNRmap;
                    KQBRmap = boardOfNewSquare.KQBRmap ||| b.KQBRmap;
                    KQPmap = boardOfNewSquare.KQPmap ||| b.KQPmap;
                }
        let internal applyNormalMove (move: normalMove) (board: board) : board result =
            getSquareFromCoordinates board move.startingCoords
            |> Option.map (fun piece -> 
                updateSquare piece move.destinationCoords board
                |> removePiece move.startingCoords
            )
            |> Result.fromOption "No piece at the starting square"
        let private applyEnpassant (move: normalMove) (board: board) : board result =
            let coordinatesOfPawnToBeRemoved = 
                (move.destinationCoords |> Coordinates.getFile,
                move.startingCoords |> Coordinates.getRow)
                ||> Coordinates.construct |> Result.failOnError

            applyNormalMove move board
            |> Result.map (removePiece coordinatesOfPawnToBeRemoved)
        let private applyPromotion (move: normalMove) (promotedPieceType: pieceType) (board: board) =
            let colour = 
                getSquareFromCoordinates board move.startingCoords
                |> Option.get
                |> fun piece -> piece.colour
            let promotedPiece = {pieceType = promotedPieceType; colour = colour}
            applyNormalMove move board
            |> Result.map (updateSquare promotedPiece move.destinationCoords)
        let private getMovesForCastling (side: side) (colour: colour) : normalMove * normalMove =
            let (kingStart, kingEnd, rookStart, rookEnd) = 
                match colour with
                | White ->
                    match side with
                    | Kingside -> ((4, 0), (6, 0), (7, 0), (5, 0))
                    | Queenside -> ((4, 0), (2, 0), (0, 0), (3, 0))
                | Black ->
                    match side with
                    | Kingside -> ((4, 7), (6, 7), (7, 7), (5, 7))
                    | Queenside -> ((4, 7), (2, 7), (0, 7), (3, 7))
                |> fun ((x1,y1), (x2,y2), (x3,y3), (x4,y4)) -> 
                    Coordinates.construct x1 y1 |> Result.failOnError,
                    Coordinates.construct x2 y2 |> Result.failOnError,
                    Coordinates.construct x3 y3 |> Result.failOnError,
                    Coordinates.construct x4 y4 |> Result.failOnError
            {startingCoords = kingStart; destinationCoords = kingEnd},
            {startingCoords = rookStart; destinationCoords = rookEnd}
        let private applyCastling (side: side) (colour: colour) (board: board) =
            let kingMove, rookMove = getMovesForCastling side colour
            board
            |> applyNormalMove kingMove
            |> Result.bind (applyNormalMove rookMove)
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
        let private filterOutSameColouredPieces (pieceColour: colour) (board: board) (coordinatesCollection: coordinatesCollection) : coordinatesCollection =
            match pieceColour with
            | White -> board.whitePieces
            | Black -> board.blackPieces
            |> (~~~)
            |> (&&&) coordinatesCollection
        let private pseudoLegal (colour: colour) (board: board) : normalMove list =
            match colour with
            | White -> board.ColourBitmap
            | Black -> ~~~ board.ColourBitmap
            |> (&&&) board.pieceMap
            |> BitMap.isolateValues
            // Construct coordinates from the bitmap
            |> List.map (fun oldCoords ->                
                // This result shouldn't fail, as we have got only coords with pieces on.
                Vision.ofPieceAtCoords board oldCoords |> Result.failOnError
                |> filterOutSameColouredPieces colour board
                |> BitMap.isolateValues
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
                CoordinatesCollection.construct ()
                |> CoordinatesCollection.appendResult (Coordinates.shift enpassantCoordinates -1 direction)
                |> CoordinatesCollection.appendResult (Coordinates.shift enpassantCoordinates  1 direction)
                |> (&&&) <|
                    match colour with
                    | White -> board.whitePawnMap
                    | Black -> board.blackPawnMap
                |> BitMap.isolateValues
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
                    |> List.map (fun name -> Coordinates.parse name |> Result.failOnError)
                    |> List.exists (fun coords ->
                        isVisibleByPlayer (Colour.opposite colour) board coords
                    )
                let squaresAreEmpty =
                    squaresThatMustBeEmpty
                    |> List.forall (fun name -> 
                        let coords = Coordinates.parse name |> Result.failOnError
                        getSquareFromCoordinates board coords
                        |> Option.isNone
                    )
                let rookInPosition =
                    Coordinates.parse squareThatNeedsRook
                    |> Result.map (getSquareFromCoordinates board)
                    |> Result.failOnError
                    |> (=) (Some {pieceType = Rook; colour = colour})
                let kingInPosition =
                    Coordinates.parse squareThatNeedsKing
                    |> Result.map (getSquareFromCoordinates board)
                    |> Result.failOnError
                    |> (=) (Some {pieceType = King; colour = colour})
                
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
                    getSquareFromCoordinates board normalMove.startingCoords
                    |> fun piece -> piece.Value.pieceType = Pawn
                let isAtEndOfBoard = 
                    let row = normalMove.destinationCoords |> Coordinates.getRow
                    row = 0 || row = 7
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
                |> Result.failOnError
                |> isInCheck colour
                |> not
            )
        let internal asyncNormal (colour: colour) (board: board) : normalMove list =
            [
                for move in List.toSeq (pseudoLegal colour board) do
                    async {
                        let inCheck =
                            Update.applyNormalMove move board
                            |> Result.failOnError
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
