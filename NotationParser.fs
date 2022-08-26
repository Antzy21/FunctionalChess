namespace Chess

open Checkerboard
open FSharp.Extensions

module NotationParser =
    let private normalMoveParsing (colour: colour) (board: board) (move: string) : move option =
        
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
            |> Option.map (fun newCoordinates ->
                Board.GetSquare.fromCoordinates newCoordinates board
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
        
    let tryParseFullNotation (board: board) (move: string) : move option =
        match move.Split(' ') with
        | [|fstSquare; _; sndSquare |] ->
            fstSquare.[fstSquare.Length-2 ..]
            |> Coordinates.tryParse
            |> Option.bind (fun fstCoordinates ->
                let fstSquare = Board.GetSquare.fromCoordinates fstCoordinates board
                sndSquare.[sndSquare.Length-2 ..]
                |> Coordinates.tryParse
                |> Option.bind (fun sndCoordinates ->
                    let sndSquare = Board.GetSquare.fromCoordinates sndCoordinates board
                    Some (fstSquare, sndSquare)
                )
            )
        | _ -> None

    let tryParse (colour: colour) (board: board) (move: string) : move option =
        match move with
        | "0-0-0" ->
            match colour with
            | White ->
                Some (
                    {piece = Some {pieceType = King; colour = colour} ; coordinates = (3,0)},
                    {piece = None; coordinates = (5,0)}
                )
            | Black ->
                Some (
                    {piece = Some {pieceType = King; colour = colour}; coordinates = (3,7)},
                    {piece = None; coordinates = (5,7)}
                )
        | "0-0" ->
            match colour with
            | White ->
                Some (
                    {piece = Some {pieceType = King; colour = colour}; coordinates = (3,0)},
                    {piece = None; coordinates = (1,0)}
                )
            | Black ->
                Some (
                    {piece = Some {pieceType = King; colour = colour}; coordinates = (3,7)},
                    {piece = None; coordinates = (1,7)}
                )
        | move when move.Contains('=') ->
            normalMoveParsing colour board (move.Split(' ').[0])
            |> Option.map (fun parsedMove ->
                let sndSquare = snd parsedMove
                let sndSquare = Square.updateWithPiece (Piece.getFromLetter (move.[move.Length-1])) sndSquare
                fst parsedMove, sndSquare
            )
        | move -> 
            try
                normalMoveParsing colour board move
            with
            _ -> tryParseFullNotation board move
    let parse (colour: colour) (board: board) (move: string) : move =
        tryParse colour board move
        |> Option.failOnNone "Failed to parse notation"
