namespace Chess

open Checkerboard
open FSharp.Extensions

module MoveParser =

    module FullNotation =
        let toString (board: board) (move: move) : string =
            match move with
            | Castling (Kingside, _) -> "0-0"
            | Castling (Queenside, _) -> "0-0-0"
            | Promotion (move, promotedPieceType) ->
                let timesSignIfTaken =
                    if (Move.getShift move |> fun (struct (x,y)) -> x <> 0) then "x"
                    else ""
                $"{Square.toString move.startingCoords (Board.getSquareFromCoordinates board move.startingCoords)} -> " +
                timesSignIfTaken +
                $"{Coordinates.getName move.destinationCoords}" +
                " = " +
                $"{(PieceType.getLetter promotedPieceType)}"
            | EnPassant move ->
                $"{Square.toString move.startingCoords (Board.getSquareFromCoordinates board move.startingCoords)} -> " +
                $"x" +
                match Board.getSquareFromCoordinates board move.startingCoords |> Square.getPieceColour |> Option.get with 
                | White -> "p"
                | Black -> "P"
                + $"{Coordinates.getName move.destinationCoords}"
                + $" e.p."
            | NormalMove move ->
                match Board.getSquareFromCoordinates board move.destinationCoords |> Square.Parser.fromBitMaps with
                | Some takenPiece ->
                    $"{Square.toString move.startingCoords (Board.getSquareFromCoordinates board move.startingCoords)} -> " +
                    $"x{(Piece.getLetter takenPiece)}" +
                    $"{Coordinates.getName move.destinationCoords}"
                | None -> 
                    $"{Square.toString move.startingCoords (Board.getSquareFromCoordinates board move.startingCoords)} -> " +
                    $"{Coordinates.getName move.destinationCoords}"

        let private tryParseSquare (square: string) : coordinates option =
            let square =
                match List.ofSeq square with
                | 'x'::sqr -> sqr
                | sqr -> sqr
            if square.Length > 3 then
                None
            else
                match square with
                | pieceLetter::coordinates when square.Length = 3 ->
                    String.ofSeq coordinates
                    |> Coordinates.tryParse
                | coordinates ->
                    String.ofSeq coordinates
                    |> Coordinates.tryParse

        let tryParse (move: string) : normalMove result =
            match move.Split(' ') with
            | [|fstSquare; _; sndSquare |] ->
                (tryParseSquare fstSquare, tryParseSquare sndSquare)
                ||> Option.map2 (fun fs ss -> {startingCoords = fs; destinationCoords= ss})
                |> Result.fromOption $"Error parsing squares {fstSquare}, {sndSquare}"
            | _ -> Error "Move is an unexpected length of chars"

    module AlgebraicNotation =
        let private getNewSquareNotationForPiece (piece: piece) (move: normalMove) (board: board) =
            match Board.Vision.reverseEngineerPieceLocations piece move.destinationCoords board with
            | _ :: [] -> ""
            | others ->
                let piecesOnRow = 
                    List.filter (fun square ->
                        Coordinates.getRow square = Coordinates.getRow move.destinationCoords
                    ) others
                match piecesOnRow with
                    | _ :: [] -> Coordinates.getRow move.startingCoords
                    | _ -> Coordinates.getFile move.startingCoords 
            + Coordinates.getName move.destinationCoords

        let toString (move: move) (board: board) : string =
            match move with
            | Castling (Kingside, _) -> "0-0"
            | Castling (Queenside, _) -> "0-0-0"
            | Promotion (move, promotedPieceType) ->
                let timesSignIfTaken =
                    if (Move.getShift move |> fun (struct (x,y)) -> x <> 0) then
                        Coordinates.getFile move.startingCoords
                        + "x"
                    else ""
                timesSignIfTaken +
                $"{Coordinates.getName move.destinationCoords}" +
                "=" +
                $"{(PieceType.getLetter promotedPieceType)}"
            | EnPassant move ->
                Coordinates.getFile move.startingCoords
                + $"x{Coordinates.getName move.destinationCoords}"
            | NormalMove move ->
                let taking = Board.getSquareFromCoordinates board move.destinationCoords |> Square.BitMap.containsPiece
                let piece = Board.getSquareFromCoordinates board move.startingCoords |> Square.Parser.fromBitMaps |> Option.get
                match piece.pieceType with
                | Pawn -> 
                    if taking then
                        $"{Coordinates.getFile move.startingCoords}x"
                    else ""
                    + (Coordinates.getName move.destinationCoords)            
                | pieceType -> 
                    $"{PieceType.getLetter pieceType}" +
                    if taking then
                        "x"
                    else ""
                    +
                    getNewSquareNotationForPiece piece move board

        let private matchReverseEngineerPieceLocation (piece: piece) ((i, j): coordinates) (board: board) : coordinates result =
            match Board.Vision.reverseEngineerPieceLocations piece (i,j) board with
            | oldCoordinates :: [] ->
                Ok oldCoordinates
            | [] ->
                Error $"No {piece.pieceType} avaiable to move to {i}, {j}"
            | squares ->
                squares
                |> List.map Coordinates.getName
                |> List.fold (fun c1 c2 ->
                    $"{c1}\n{c2}"
                ) $"Too many {piece.pieceType}s are able to move to {i}, {j}."
                |> Error 

        let private parsePawnMove colour board pawnFile coords : normalMove result =
            Move.PawnMoves.getPawnOriginPossibilitiesFromDestination coords colour board
            |> Board.getCoordinatesAfterShifts coords board
            |> List.tryFind (fun square ->
                Coordinates.getFile square = pawnFile.ToString()
                && Board.getSquareFromCoordinates board square |> Square.Parser.fromBitMaps |> (=) <| Some {pieceType = Pawn; colour = colour}
            )
            |> Result.fromOption "No possible pawn origin found."
            |> Result.map (fun oldCoords -> {startingCoords = oldCoords; destinationCoords = coords})

        let private parseNonPawnMove (pieceLetter: char) (colour: colour) (board: board) (newCoords: coordinates) : move result =
            PieceType.tryParse pieceLetter
            |> Result.fromOption "Unable to parse piece"
            |> Result.map (fun pieceType -> {pieceType = pieceType; colour = colour})
            |> Result.bind (fun piece ->
                newCoords
                |> fun (newCoords: coordinates) ->
                    let temp = matchReverseEngineerPieceLocation piece newCoords board
                    Result.map (fun (oldCoords: coordinates) ->
                        NormalMove {startingCoords = oldCoords; destinationCoords = newCoords}
                    ) temp
            )

        let private normalMoveParsing (colour: colour) (board: board) (move: string) : move result =
            let (getMoveFunc: coordinates -> move result), file, rank =
                match List.ofSeq move with
                | [fstLetter; 'x'; file; rank; '='; promotionPiece] when System.Char.IsLower(fstLetter) ->
                    parsePawnMove colour board fstLetter >> 
                        Result.map (fun parsedMove -> 
                            Promotion (parsedMove, PieceType.fromLetter promotionPiece)
                        )
                    , file, rank
                | [file; rank; '='; promotionPiece] ->
                    fun newCoords ->
                        parsePawnMove colour board (Coordinates.getFile newCoords) newCoords
                        |> Result.map (fun parsedMove -> 
                            Promotion (parsedMove, PieceType.fromLetter promotionPiece)
                        )
                    , file, rank
                | [fstLetter; 'x'; file; rank] when System.Char.IsUpper(fstLetter) ->
                    parseNonPawnMove fstLetter colour board, file, rank
                | [fstLetter; 'x'; file; rank] when System.Char.IsLower(fstLetter) ->
                    fun coords -> 
                        parsePawnMove colour board fstLetter coords
                        |> Result.map (fun move ->
                            if Board.getSquareFromCoordinates board move.destinationCoords |> Square.BitMap.containsPiece then
                                NormalMove move
                            else
                                EnPassant move
                        )
                    , file, rank
                | [pieceLetter; file; rank] -> 
                    parseNonPawnMove pieceLetter colour board, file, rank
                | [file; rank] -> 
                    fun newCoords ->
                        parsePawnMove colour board (Coordinates.getFile newCoords) newCoords
                    >> Result.map NormalMove
                    , file, rank
                | _ -> failwith "Error parsing normal move"
            $"{file}{rank}"
            |> Coordinates.tryParse
            |> Result.fromOption $"Error parsing coordinates {file}{rank}"
            |> Result.bind getMoveFunc

        let tryParse (colour: colour) (board: board) (move: string) : move result =
            match move with
            | "0-0-0" ->
                Ok <| Castling (Queenside, colour)
            | "0-0" ->
                Ok <| Castling (Kingside, colour)
            | move -> 
                normalMoveParsing colour board move

    let tryParse (playerTurn: colour) (board: board) (move: string) =
        AlgebraicNotation.tryParse playerTurn board move
        |> Result.orElseWith (fun () -> 
            FullNotation.tryParse move
            |> Result.map NormalMove
        )