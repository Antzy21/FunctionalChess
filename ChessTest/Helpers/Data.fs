module internal ChessTest.Helpers.Data

open Chess
open Checkerboard

module Fens =

    module Enpassant =
        let PreWhite : string = "4k3/8/8/pP6/8/8/8/4K3 w - a6 0 0"
        let PostWhite : string = "4k3/8/P7/8/8/8/8/4K3 b - - 1 1"
        let PreBlack : string = "4k3/8/8/8/Pp6/8/8/4K3 b - a3 0 0"
        let PostBlack : string = "4k3/8/8/8/8/p7/8/4K3 w - - 1 0"

    module Promotion =
        let PreWhite1 : string = "8/P7/8/8/8/8/8/4K2k w - - 0 0"
        let PostWhite1 : string = "Q7/8/8/8/8/8/8/4K2k b - - 1 1"
        let PreWhite2 : string = "bn6/P7/8/8/8/8/8/4K2k w - - 0 0"
        let PostWhite2 : string = "bR6/8/8/8/8/8/8/4K2k b - - 1 1"
    
    module Castling =
        let PreWhite : string = "4k3/8/8/8/8/8/8/R3K2R w KQkq - 0 0"
        let PreBlack : string = "r3k2r/8/8/8/8/8/8/4K3 b KQkq - 0 0"
        let PostWhiteKing : string = "4k3/8/8/8/8/8/8/R4RK1 b Qkq - 1 1"
        let PostWhiteQueen : string = "4k3/8/8/8/8/8/8/2KR3R b Kkq - 1 1"
        let PostBlackKing : string = "r4rk1/8/8/8/8/8/8/4K3 w KQq - 1 0"
        let PostBlackQueen : string = "2kr3r/8/8/8/8/8/8/4K3 w KQk - 1 0"

    module ExampleGame =
        let White1 : string = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 1 1"
        let Black1 : string = "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 2 1"
        let White2 : string = "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 3 2"
        let Black2 : string = "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPP1PPP/RNBQKBNR w KQkq - 4 2"
    
module Moves =

    module EnPassant =
        let White : move =
            ({
                coordinates = (1,4);
                piece = Some {pieceType = Pawn; colour = White}
            },
            {
                coordinates = (0,5);
                piece = None
            })
            |> EnPassant
        let Black : move =
            ({
                coordinates = (1,3);
                piece = Some {pieceType = Pawn; colour = Black}
            },
            {
                coordinates = (0,2);
                piece = None
            })
            |> EnPassant
    
    module Promotion =
        let White1 : move =
            ({
                coordinates = (0,6);
                piece = Some {pieceType = Pawn; colour = White}
            },
            {
                coordinates = (0,7);
                piece = None
            })
            |> fun move -> Promotion (move, Queen)
        let White2 : move =
            ({
                coordinates = (0,6);
                piece = Some {pieceType = Pawn; colour = White}
            },
            {
                coordinates = (1,7);
                piece = Some {pieceType = Knight; colour = Black}
            })
            |> fun move -> Promotion (move, Rook)

    let WhiteMove1 : move = 
        ({
            coordinates = (4,1);
            piece = Some {pieceType = Pawn; colour = White}
        },
        {
            coordinates = (4,3);
            piece = None
        })
        |> NormalMove
    let BlackMove1 : move = 
        ({
            coordinates = (3,6);
            piece = Some {pieceType = Pawn; colour = Black}
        },
        {
            coordinates = (3,4);
            piece = None
        })
        |> NormalMove
    let WhiteMove2 : move =
        ({
            coordinates = (4,3);
            piece = Some {pieceType = Pawn; colour = White}
        },
        {
            coordinates = (3,4);
            piece = Some {pieceType = Pawn; colour = Black}
        })
        |> NormalMove
    let BlackMove2 : move = 
        ({
            coordinates = (6,7);
            piece = Some {pieceType = Knight; colour = Black}
        },
        {
            coordinates = (5,5);
            piece = None
        })
        |> NormalMove

