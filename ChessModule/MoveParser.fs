namespace Chess

open Checkerboard
open FSharp.Extensions

module MoveParser =

    module FullNotation =
        let toString (move: move) : string =
            match move with
            | Castling (Kingside, _) -> "0-0"
            | Castling (Queenside, _) -> "0-0-0"
            | Promotion (move, promotedPieceType) ->
                let timesSignIfTaken =
                    if Move.getShift move |> fst <> 0 then "x"
                    else ""
                $"{move |> fst |> Square.getDescription} -> " +
                timesSignIfTaken +
                $"{(move |> snd |> Square.getCoordinatesName)}" +
                " = " +
                $"{(PieceType.getLetter promotedPieceType)}"
            | EnPassant move ->
                $"{move |> fst |> Square.getDescription} -> " +
                $"x" +
                match (Move.getMovedPiece move).colour with 
                | White -> "p"
                | Black -> "P"
                + $"{(move |> snd |> Square.getCoordinatesName)}"
                + $" e.p."
            | NormalMove move ->
                match Move.getPieceAtDestination move with
                | Some takenPiece ->
                    $"{move |> fst |> Square.getDescription} -> " +
                    $"x{(Piece.getLetter takenPiece)}" +
                    $"{(move |> snd |> Square.getCoordinatesName)}"
                | None -> 
                    $"{move |> fst |> Square.getDescription} -> " +
                    $"{(move |> snd |> Square.getCoordinatesName)}"
        let print (move: move) =
            printfn $"{toString move}"
        let private tryParseSquare (square: string) : square option =
            let square =
                match List.ofSeq square with
                | 'x'::sqr -> sqr
                | sqr -> sqr
            if square.Length > 3 then
                None
            else
                match square with
                | pieceLetter::coordinates when square.Length = 3 ->
                    Some {
                        piece = Piece.getFromLetter pieceLetter |> Some;
                        coordinates = 
                            String.ofSeq coordinates
                            |> Coordinates.tryParse
                            |> Option.get
                    }
                | coordinates ->
                    Some {
                        piece = None
                        coordinates = 
                            String.ofSeq coordinates
                            |> Coordinates.tryParse
                            |> Option.get
                    }
        let tryParse (move: string) : normalMove option =
            match move.Split(' ') with
            | [|fstSquare; _; sndSquare |] ->
                (tryParseSquare fstSquare, tryParseSquare sndSquare)
                ||> Option.map2 (fun fs ss -> (fs, ss))
            | _ -> None
        let parse (move: string) : normalMove =
            tryParse move
            |> Option.failOnNone "Failed to parse notation"

    module AlgebraicNotation =
    
        let private matchReverseEngineerPieceLocation (piece: piece) ((i, j): coordinates) (board: board) : coordinates option =
            match Board.GetSquares.reverseEngineerPieceLocations piece (i,j) board with
            | oldSquare :: [] ->
                Some oldSquare.coordinates
            | [] ->
                printfn $"No {piece.pieceType} avaiable to move to {i}, {j}"
                None
            | squares ->
                printfn $"Too many {piece.pieceType}s are able to move to {i}, {j}"
                squares
                |> List.iter (fun square -> printfn $"{Coordinates.getName square.coordinates}")
                None
        
        let private getNewSquareNotationForPiece (piece: piece) (move: normalMove) (board: board) =
            match Board.GetSquares.reverseEngineerPieceLocations piece (snd move).coordinates board with
            | [] -> ""
            | others ->
                let piecesOnRow = 
                    List.filter (fun square ->
                        Square.getRow square = Square.getRow (fst move)  
                    ) others
                match piecesOnRow with
                    | _ :: [] -> Square.getRow (fst move) 
                    | _ -> Square.getFile (fst move) 
            + (snd move |> Square.getCoordinatesName)

        let toString (move: move) (board: board) : string =
            match move with
            | Castling (Kingside, _) -> "0-0"
            | Castling (Queenside, _) -> "0-0-0"
            | Promotion (move, promotedPieceType) ->
                let timesSignIfTaken =
                    if Move.getShift move |> fst <> 0 then
                        (fst move |> Square.getFile)
                        + "x"
                    else ""
                timesSignIfTaken +
                $"{(move |> snd |> Square.getCoordinatesName)}" +
                "=" +
                $"{(PieceType.getLetter promotedPieceType)}"
            | EnPassant move ->
                (fst move |> Square.getFile)
                + $"x{(move |> snd |> Square.getCoordinatesName)}"
            | NormalMove move ->
                let taking = Move.getPieceAtDestination move |> Option.isSome
                let piece = (Move.getMovedPiece move)
                match piece.pieceType with
                | Pawn -> 
                    if taking then
                        $"{(fst move |> Square.getFile)}x"
                    else ""
                    + (snd move |> Square.getCoordinatesName)            
                | pieceType -> 
                    $"{PieceType.getLetter pieceType}" +
                    if taking then
                        "x"
                    else ""
                    +
                    getNewSquareNotationForPiece piece move board
        let print (move: move) =
            printfn $"{toString move}"
        let private getNewSquareFromMove (board: board) (file: char) (rank: char) : coordinates option =
            $"{file}{rank}"
            |> Coordinates.tryParse

        let private parsePawnMove colour board pawnFile newSquare : normalMove option =
            Move.PawnMoves.getPawnFrom newSquare colour board
            |> List.map (fun sqr -> sqr.coordinates)
            |> List.tryFind (fun coords ->
                Coordinates.getFile coords = pawnFile.ToString()
                && (Board.GetSquare.fromCoordinates board coords).piece |> (=) <| (Some {pieceType = Pawn; colour = colour})
            )
            |> Option.map (fun (oldSquare: coordinates) -> (Board.GetSquare.fromCoordinates board oldSquare, Board.GetSquare.fromCoordinates board newSquare))

        let private parseNonPawnMove (pieceLetter: char) (colour: colour) (board: board) (newSquare: coordinates) : move option =
            PieceType.tryParse pieceLetter
            |> Option.map (fun pieceType -> {pieceType = pieceType; colour = colour})
            |> Option.bind (fun piece ->
                newSquare
                |> fun (newCoords: coordinates) ->
                    let temp = matchReverseEngineerPieceLocation piece newCoords board
                    Option.map (fun (oldSquare: coordinates) ->
                        let oldSqr = Board.GetSquare.fromCoordinates board newCoords
                        let newSqr = Board.GetSquare.fromCoordinates board newCoords
                        NormalMove (oldSqr, newSqr)
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
                    fun newCoordinates ->
                        parsePawnMove colour board (Coordinates.getFile newCoordinates) newCoordinates
                        |> Option.map (fun parsedMove -> 
                            Promotion (parsedMove, PieceType.fromLetter promotionPiece)
                        )
                    , file, rank
                | [fstLetter; 'x'; file; rank] when System.Char.IsUpper(fstLetter) ->
                    parseNonPawnMove fstLetter colour board, file, rank
                | [fstLetter; 'x'; file; rank] when System.Char.IsLower(fstLetter) ->
                    fun coords -> 
                        parsePawnMove colour board fstLetter coords
                        |> Option.map (
                            if Board.containsPiece coords board then
                                NormalMove
                            else
                                EnPassant
                        )
                    , file, rank
                | [pieceLetter; file; rank] -> 
                    parseNonPawnMove pieceLetter colour board, file, rank
                | [file; rank] -> 
                    fun newcoordinates ->
                        parsePawnMove colour board (Coordinates.getFile newcoordinates) newcoordinates
                    >> Option.map NormalMove
                    , file, rank
                | _ -> failwith "Error parsing normal move"
            getNewSquareFromMove board file rank
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
            |> Option.failOnNone "Failed to parse notation"

    let tryParse (playerTurn: colour) (board: board) (move: string) =
        AlgebraicNotation.tryParse playerTurn board move
        |> Option.orElseWith (fun () -> 
            FullNotation.tryParse move
            |> Option.map NormalMove
        )
    let parse (colour: colour) (board: board) (move: string) : move =
        tryParse colour board move
        |> Option.failOnNone "Failed to parse notation"
