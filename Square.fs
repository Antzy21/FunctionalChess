namespace Chess

open Checkerboard

type square = square<piece>

module Square =
    let print (square: square) =
        match square.piece with
        | Some piece -> Piece.getLetter piece
        | None -> '.'
    let getDescription (square: square) =
        let pieceTypeLetter = 
            match square.piece with
            | None -> ' '
            | Some piece -> PieceType.getLetter piece.pieceType
        $"{pieceTypeLetter}{(Square.getName square)}"
    let getMoves (board: board<piece>) (square: square) : square list=
        let piece = square |> Square.getPiece
        let stopAt = Some (fun (otherPiece: piece) -> true)
        let blockSelfTaking (newSquare: square) : bool =
            let piece = Square.getPiece square
            let x,y = newSquare.coordinates
            match board.[x,y].piece with
            | Some oldPiece -> oldPiece.colour <> piece.colour
            | None -> true
        match piece.pieceType with
            | Knight -> Board.getSquares.afterAllShiftDirections square 1 2 board
            | Bishop -> Board.getSquares.getDiagonals square stopAt board
            | Rook -> Board.getSquares.getRowAndFile square stopAt board
            | Queen -> List.append (Board.getSquares.getRowAndFile square stopAt board) (Board.getSquares.getDiagonals square stopAt board)
            | King -> Board.getSquares.adjacent square board
            | Pawn -> Piece.getPawnMoveFunction square board piece 
        |> List.filter blockSelfTaking