namespace Chess

open System

type square = piece option

module Square =
    let print (square: square) =
        match square with
        | Some piece -> Piece.getLetter piece
        | None -> '.'

type board = square[,]

module Board =
    module Create =
        let empty () : board =
            Array2D.create 8 8 None
        let starting () : board =
            let board = empty ()
            board.[0,1] <- Some {pieceType = Pawn; colour = White}
            board.[1,1] <- Some {pieceType = Pawn; colour = White}
            board.[2,1] <- Some {pieceType = Pawn; colour = White}
            board.[3,1] <- Some {pieceType = Pawn; colour = White}
            board.[4,1] <- Some {pieceType = Pawn; colour = White}
            board.[5,1] <- Some {pieceType = Pawn; colour = White}
            board.[6,1] <- Some {pieceType = Pawn; colour = White}
            board.[7,1] <- Some {pieceType = Pawn; colour = White}
            board.[0,0] <- Some {pieceType = Rook; colour = White}
            board.[1,0] <- Some {pieceType = Knight; colour = White}
            board.[2,0] <- Some {pieceType = Bishop; colour = White}
            board.[3,0] <- Some {pieceType = Queen; colour = White}
            board.[4,0] <- Some {pieceType = King; colour = White}
            board.[5,0] <- Some {pieceType = Bishop; colour = White}
            board.[6,0] <- Some {pieceType = Knight; colour = White}
            board.[7,0] <- Some {pieceType = Rook; colour = White}
            
            board.[0,6] <- Some {pieceType = Pawn; colour = Black}
            board.[1,6] <- Some {pieceType = Pawn; colour = Black}
            board.[2,6] <- Some {pieceType = Pawn; colour = Black}
            board.[3,6] <- Some {pieceType = Pawn; colour = Black}
            board.[4,6] <- Some {pieceType = Pawn; colour = Black}
            board.[5,6] <- Some {pieceType = Pawn; colour = Black}
            board.[6,6] <- Some {pieceType = Pawn; colour = Black}
            board.[7,6] <- Some {pieceType = Pawn; colour = Black}
            board.[0,7] <- Some {pieceType = Rook; colour = Black}
            board.[1,7] <- Some {pieceType = Knight; colour = Black}
            board.[2,7] <- Some {pieceType = Bishop; colour = Black}
            board.[3,7] <- Some {pieceType = Queen; colour = Black}
            board.[4,7] <- Some {pieceType = King; colour = Black}
            board.[5,7] <- Some {pieceType = Bishop; colour = Black}
            board.[6,7] <- Some {pieceType = Knight; colour = Black}
            board.[7,7] <- Some {pieceType = Rook; colour = Black}

            board
    let print (board : board) : unit =
        printfn "   ________________________"
        printfn "  /                        \\"
        [0..7]
        |> List.rev
        |> List.iter (fun j ->
            printf $"{j+1} |"
            [0..7]
            |> List.iter (fun i ->
                printf " %c " <| Square.print board.[i,j]
            )
            printfn "|"
        )
        printfn "  \\________________________/"
        printfn "    a  b  c  d  e  f  g  h"