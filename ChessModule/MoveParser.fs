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
                $"{Square.getDescription move.startingCoords (Board.getSquareFromCoordinates board move.startingCoords)} -> " +
                timesSignIfTaken +
                $"{Coordinates.getName move.destinationCoords}" +
                " = " +
                $"{(PieceType.getLetter promotedPieceType)}"
            | EnPassant move ->
                $"{Square.getDescription move.startingCoords (Board.getSquareFromCoordinates board move.startingCoords)} -> " +
                $"x" +
                match Board.getSquareFromCoordinates board move.startingCoords |> Square.getPieceColour |> Option.get with 
                | White -> "p"
                | Black -> "P"
                + $"{Coordinates.getName move.destinationCoords}"
                + $" e.p."
            | NormalMove move ->
                match Board.getSquareFromCoordinates board move.destinationCoords |> Square.Parser.fromBitMaps with
                | Some takenPiece ->
                    $"{Square.getDescription move.startingCoords (Board.getSquareFromCoordinates board move.startingCoords)} -> " +
                    $"x{(Piece.getLetter takenPiece)}" +
                    $"{Coordinates.getName move.destinationCoords}"
                | None -> 
                    $"{Square.getDescription move.startingCoords (Board.getSquareFromCoordinates board move.startingCoords)} -> " +
                    $"{Coordinates.getName move.destinationCoords}"
        let print (move: move) (board: board) =
            printfn $"{toString board move}"
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
        let tryParse (move: string) : normalMove option =
            match move.Split(' ') with
            | [|fstSquare; _; sndSquare |] ->
                (tryParseSquare fstSquare, tryParseSquare sndSquare)
                ||> Option.map2 (fun fs ss -> {startingCoords = fs; destinationCoords= ss})
            | _ -> None
        let parse (move: string) : normalMove =
            tryParse move
            |> Option.failOnNone $"Failed to parse notation {move}"

    module AlgebraicNotation =
    
        let private matchReverseEngineerPieceLocation (piece: piece) ((i, j): coordinates) (board: board) : coordinates option =
            match Board.GetSquares.reverseEngineerPieceLocations piece (i,j) board with
            | oldCoordinates :: [] ->
                Some oldCoordinates
            | [] ->
                printfn $"No {piece.pieceType} avaiable to move to {i}, {j}"
                None
            | squares ->
                printfn $"Too many {piece.pieceType}s are able to move to {i}, {j}"
                squares
                |> List.iter (fun coords -> printfn $"{Coordinates.getName coords}")
                None

        let private getNewSquareNotationForPiece (piece: piece) (move: normalMove) (board: board) =
            match Board.GetSquares.reverseEngineerPieceLocations piece move.destinationCoords board with
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
        let print (board: board) (move: move) =
            printfn $"{(toString move board)}"

        let private parsePawnMove colour board pawnFile coords : normalMove option =
            Move.PawnMoves.getPawnOriginPossibilitiesFromDestination coords colour board
            |> Board.getCoordinatesAfterShifts coords board
            |> List.tryFind (fun square ->
                Coordinates.getFile square = pawnFile.ToString()
                && Board.getSquareFromCoordinates board square |> Square.Parser.fromBitMaps |> (=) <| Some {pieceType = Pawn; colour = colour}
            )
            |> Option.map (fun oldCoords -> {startingCoords = oldCoords; destinationCoords = coords})

        let private parseNonPawnMove (pieceLetter: char) (colour: colour) (board: board) (newCoords: coordinates) : move option =
            PieceType.tryParse pieceLetter
            |> Option.map (fun pieceType -> {pieceType = pieceType; colour = colour})
            |> Option.bind (fun piece ->
                newCoords
                |> fun (newCoords: coordinates) ->
                    let temp = matchReverseEngineerPieceLocation piece newCoords board
                    Option.map (fun (oldCoords: coordinates) ->
                        NormalMove {startingCoords = oldCoords; destinationCoords = newCoords}
                    ) temp
            )

        let private normalMoveParsing (colour: colour) (board: board) (move: string) : move option =
            let (getMoveFunc: coordinates -> move option), file, rank =
                match List.ofSeq move with
                | [fstLetter; 'x'; file; rank; '='; promotionPiece] when System.Char.IsLower(fstLetter) ->
                    parsePawnMove colour board fstLetter >> 
                        Option.map (fun parsedMove -> 
                            Promotion (parsedMove, PieceType.fromLetter promotionPiece)
                        )
                    , file, rank
                | [file; rank; '='; promotionPiece] ->
                    fun newCoords ->
                        parsePawnMove colour board (Coordinates.getFile newCoords) newCoords
                        |> Option.map (fun parsedMove -> 
                            Promotion (parsedMove, PieceType.fromLetter promotionPiece)
                        )
                    , file, rank
                | [fstLetter; 'x'; file; rank] when System.Char.IsUpper(fstLetter) ->
                    parseNonPawnMove fstLetter colour board, file, rank
                | [fstLetter; 'x'; file; rank] when System.Char.IsLower(fstLetter) ->
                    fun coords -> 
                        parsePawnMove colour board fstLetter coords
                        |> Option.map (fun move ->
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
                    >> Option.map NormalMove
                    , file, rank
                | _ -> failwith "Error parsing normal move"
            $"{file}{rank}"
            |> Coordinates.tryParse
            |> Option.bind getMoveFunc

        let rec tryParse (colour: colour) (board: board) (move: string) : move option =
            match move with
            | "0-0-0" ->
                Some <| Castling (Queenside, colour)
            | "0-0" ->
                Some <| Castling (Kingside, colour)
            | move -> 
                normalMoveParsing colour board move
        let parse (colour: colour) (board: board) (move: string) : move =
            tryParse colour board move
            |> Option.failOnNone $"Failed to parse notation {move}"

    let tryParse (playerTurn: colour) (board: board) (move: string) =
        AlgebraicNotation.tryParse playerTurn board move
        |> Option.orElseWith (fun () -> 
            FullNotation.tryParse move
            |> Option.map NormalMove
        )
    let parse (colour: colour) (board: board) (move: string) : move =
        tryParse colour board move
        |> Option.failOnNone $"Failed to parse notation {move}"
