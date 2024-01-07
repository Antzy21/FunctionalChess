namespace Chess

open System
open Checkerboard
open FSharp.Extensions

module BoardParser =

    let private updateBoardFromFenChar (fenChar: char) (coords: coordinates) (board: board) : board =
        if fenChar <> '1' then
            let piece = Piece.getFromLetter fenChar
            Board.Update.updateSquare piece coords board
        else
            board

    let private replaceNumbersWithReplicatedOnes =
        Seq.fold (fun acc (c: char) ->
            if (System.Char.IsNumber c) then
                acc + (String.replicate (int $"{c}") "1")
            else
                acc + $"{c}"
        ) ""

    /// Converts a FEN notation string into a chess board object
    let fromFen (fen: string) : board =
        let board = Board.construct ()
        fen
        |> replaceNumbersWithReplicatedOnes
        |> fun fen -> fen.Split('/')
        |> Array.rev
        |> Array.fold (fun (j, board) (row: string) ->
            row
            |> Seq.fold (fun (((i,j), b): (int*int) * board) (c: char) ->
                let coords = Coordinates.construct i j |> Result.failOnError
                (i+1,j), (updateBoardFromFenChar c coords b)
            ) ((0,j), board)
            |> fun ((_,j), board) -> (j+1, board)
        ) (0, board)
        |> snd

    let private addOrIncrementIntegerAtEndOfString (str: string) =
        if str = "" then
            "1"
        else if Seq.last str |> Char.IsNumber then
            let addOne =
                Seq.last str
                |> Char.GetNumericValue
                |> (+) 1. |> string
            str[..str.Length-2] + addOne
        else
            str + "1"

    let private addSlashIfEndOfLine (c : coordinates) (fen: string) : string =
        if Coordinates.getFile c = 7 && Coordinates.getRow c <> 0 then
            fen + "/"
        else
            fen

    /// Converts a chess board object into a FEN notation string
    let toFen (board: board) : string =
        board
        |> Board.foldjiback (fun coords fen square ->
            match square with
            | Some piece ->
                fen + (Piece.getLetter piece |> string)
            | None -> 
                addOrIncrementIntegerAtEndOfString fen
            |> addSlashIfEndOfLine coords
        ) ""
