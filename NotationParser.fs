namespace Chess

open Checkerboard

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
                move.[move.Length-1 .. move.Length]
                |> Coordinates.fromName
            let pieceType =
                if move.Length = 2 then
                    Pawn
                else 
                    PieceType.fromLetter (move.Chars 0)

            let newSquare = {piece = Some {pieceType = pieceType; colour = colour}; coordinates = newCoordinates}

            Board.GetSquares.reverseEngineerPieceLocations board newSquare
            |> function
            | oldSquare :: [] ->
                Some (newSquare, oldSquare)
            | [] ->
                printfn $"No {pieceType} avaiable to do {move}"
                None
            | _ ->
                printfn $"Too many {pieceType}s are able to do {move}"
                None
