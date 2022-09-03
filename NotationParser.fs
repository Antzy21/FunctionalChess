namespace Chess

open Checkerboard
open FSharp.Extensions

module NotationParser =
    
    let private tryParseSquare (board: board) (square: string) : square option =
        square.[square.Length-2 ..]
        |> Coordinates.tryParse
        |> Option.map (fun coordinates ->
            Board.GetSquare.fromCoordinates coordinates board
        )

    let private normalMoveParsing (colour: colour) (board: board) (move: string) : normalMove option =
        
        let piece = 
            if move.Length = 2 || move.Chars 0 = 'x' then
                Some Pawn
            else 
                PieceType.tryParse (move.Chars 0)
            |> Option.map (fun pieceType ->
                {pieceType = pieceType; colour = colour}
            )

        let newSquare = tryParseSquare board move

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

    let tryParseFullNotation (board: board) (move: string) : normalMove option =
        match move.Split(' ') with
        | [|fstSquare; _; sndSquare |] ->
            (tryParseSquare board fstSquare, tryParseSquare board sndSquare)
            ||> Option.map2 (fun fs ss -> (fs, ss))
        | _ -> None

    let tryParse (colour: colour) (board: board) (move: string) : move option =
        match move with
        | "0-0-0" ->
            Some <| Castling (Queenside, colour)
        | "0-0" ->
            Some <| Castling (Kingside, colour)
        | move when move.Contains('=') ->
            normalMoveParsing colour board (move.Split(' ').[0])
            |> Option.map (fun parsedMove ->
                Promotion (parsedMove, PieceType.fromLetter (move.[move.Length-1]))
            )
        | move -> 
            try
                normalMoveParsing colour board move
                |> Option.map Move
            with
            _ -> tryParseFullNotation board move
                |> Option.map Move
    let parse (colour: colour) (board: board) (move: string) : move =
        tryParse colour board move
        |> Option.failOnNone "Failed to parse notation"
