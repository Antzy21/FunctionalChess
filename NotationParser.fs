namespace Chess

open Checkerboard
open FSharp.Extensions

module NotationParser =
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
        | move ->
            let newCoordinates = 
                move.[move.Length-2 .. move.Length]
                |> Coordinates.fromName
            let pieceType =
                if move.Length = 2 || move.Chars 0 = 'x' then
                    Pawn
                else 
                    PieceType.fromLetter (move.Chars 0)
            let piece = {pieceType = pieceType; colour = colour}

            let newSquare = Board.GetSquare.fromCoordinates newCoordinates board

            Board.GetSquares.reverseEngineerPieceLocations piece newSquare.coordinates board
            |> function
            | oldSquare :: [] ->
                Some (oldSquare, newSquare)
            | [] ->
                printfn $"No {pieceType} avaiable to do {move}"
                None
            | squares ->
                printfn $"Too many {pieceType}s are able to do {move}"
                squares
                |> List.iter (fun square -> printfn $"{square.coordinates}")
                None

    let parse (colour: colour) (board: board) (move: string) : move =
        tryParse colour board move
        |> Option.failOnNone "Failed to parse notation"
