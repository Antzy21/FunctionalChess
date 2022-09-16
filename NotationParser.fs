namespace Chess

open Checkerboard
open FSharp.Extensions

module NotationParser =
    
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

    let private normalMoveParsing (colour: colour) (board: board) (move: string) : normalMove option =
        
        let piece = 
            if move.Length = 2 || move.Chars 0 = 'x' then
                Some Pawn
            else 
                PieceType.tryParse (move.Chars 0)
            |> Option.map (fun pieceType ->
                {pieceType = pieceType; colour = colour}
            )

        let newSquare = 
            move.[move.Length-2 ..]
            |> Coordinates.tryParse
            |> Option.map (fun c ->
                Board.GetSquare.fromCoordinates c board
            )

        (piece, newSquare)
        ||> Option.map2 (fun p ns -> (p, ns))
        |> Option.bind (fun (piece, newSquare) ->
            Board.GetSquares.reverseEngineerPieceLocations piece newSquare.coordinates board
            |> function
            | oldSquare :: [] ->
                Some (oldSquare, newSquare)
            | [] ->
                printfn $"No {piece.pieceType} avaiable to do {move}"
                None
            | squares ->
                printfn $"Too many {piece.pieceType}s are able to do {move}"
                squares
                |> List.iter (fun square -> printfn $"{Square.getDescription square}")
                None
        )

    let tryParseFullNotation (move: string) : normalMove option =
        match move.Split(' ') with
        | [|fstSquare; _; sndSquare |] ->
            (tryParseSquare fstSquare, tryParseSquare sndSquare)
            ||> Option.map2 (fun fs ss -> (fs, ss))
        | _ -> None
    let parseFullNotation (move: string) : normalMove =
        tryParseFullNotation move
        |> Option.failOnNone "Failed to parse notation"

    let tryParse (colour: colour) (board: board) (move: string) : move option =
        match move with
        | "0-0-0" ->
            Some <| Castling (Queenside, colour)
        | "0-0" ->
            Some <| Castling (Kingside, colour)
        | move when move.Contains('=') ->
            let moveWithoutPromotion = move.Remove(move.Length-4)
            normalMoveParsing colour board moveWithoutPromotion
            |> Option.orElse (tryParseFullNotation moveWithoutPromotion)
            |> Option.map (fun parsedMove ->
                Promotion (parsedMove, PieceType.fromLetter (move.[move.Length-1]))
            )
        | move when move.Contains("e.p.") ->
            tryParseFullNotation (move.Remove(move.Length-5))
            |> Option.map (fun move ->
                EnPassant (fst move, Square.removePiece (snd move))
            )
        | move -> 
            let parsedMove = 
                try
                    normalMoveParsing colour board move
                with
                _ -> tryParseFullNotation move
            parsedMove
            |> Option.orElse (tryParseFullNotation move)
            |> Option.map (fun parsedMove ->
                NormalMove parsedMove
            )
    let parse (colour: colour) (board: board) (move: string) : move =
        tryParse colour board move
        |> Option.failOnNone "Failed to parse notation"
