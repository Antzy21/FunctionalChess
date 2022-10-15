module internal ChessTest.Helpers.Data

open Chess
open Checkerboard
    
module Moves =

    module EnPassant =
        let PreWhite : move =
            ({
                coordinates = (0,6);
                piece = Some {pieceType = Pawn; colour = Black}
            },
            {
                coordinates = (0,4)
                piece = None
            })
            |> NormalMove
        let PostWhite : move =
            ({
                coordinates = (1,4);
                piece = Some {pieceType = Pawn; colour = White}
            },
            {
                coordinates = (0,5);
                piece = None
            })
            |> EnPassant
        let PreBlack : move =
            ({
                coordinates = (0,1);
                piece = Some {pieceType = Pawn; colour = White}
            },
            {
                coordinates = (0,3)
                piece = None
            })
            |> NormalMove
        let PostBlack : move =
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

module Games =

    module Enpassant =
        let PreWhite : game = 
            {
                gameState = GameState.Create.fromFen "4k3/8/8/pP6/8/8/8/4K3 w - a6 0 0";
                moves = [Moves.EnPassant.PreWhite]
            }
        let PostWhite : game = 
            {
                gameState = GameState.Create.fromFen "4k3/8/P7/8/8/8/8/4K3 b - - 1 1"
                moves = [Moves.EnPassant.PostWhite; Moves.EnPassant.PreWhite]
            }
        let PreBlack : game = 
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/Pp6/8/8/4K3 b - a3 0 0";
                moves = [Moves.EnPassant.PreBlack]
            }
        let PostBlack : game =
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/8/p7/8/4K3 w - - 1 0";
                moves = [Moves.EnPassant.PostBlack; Moves.EnPassant.PreBlack;]
            }

    module Promotion =
        let PreWhite1 : game =
            {
                gameState = GameState.Create.fromFen "8/P7/8/8/8/8/8/4K2k w - - 0 0";
                moves = []
            }
        let PostWhite1 : game =
            {
                gameState = GameState.Create.fromFen "Q7/8/8/8/8/8/8/4K2k b - - 1 1";
                moves = [Moves.Promotion.White1]
            }
        let PreWhite2 : game =
            {
                gameState = GameState.Create.fromFen "bn6/P7/8/8/8/8/8/4K2k w - - 0 0";
                moves = []
            }
        let PostWhite2 : game =
            {
                gameState = GameState.Create.fromFen "bR6/8/8/8/8/8/8/4K2k b - - 1 1";
                moves = [Moves.Promotion.White2]
            }
    
    module Castling =
        let PreWhite : game =
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/8/8/8/R3K2R w KQkq - 0 0";
                moves = []
            }
        let PreBlack : game =
            {
                gameState = GameState.Create.fromFen "r3k2r/8/8/8/8/8/8/4K3 b KQkq - 0 0";
                moves = []
            }
        let PostWhiteKing : game =
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/8/8/8/R4RK1 b kq - 1 1";
                moves = [Castling (Kingside, White)]
            }
        let PostWhiteQueen : game =
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/8/8/8/2KR3R b kq - 1 1";
                moves = [Castling (Queenside, White)]
            }
        let PostBlackKing : game =
            {
                gameState = GameState.Create.fromFen "r4rk1/8/8/8/8/8/8/4K3 w KQ - 1 0";
                moves = [Castling (Kingside, Black)]
            }
        let PostBlackQueen : game =
            {
                gameState = GameState.Create.fromFen "2kr3r/8/8/8/8/8/8/4K3 w KQ - 1 0";
                moves = [Castling (Queenside, Black)]
            }

    module ExampleGame =
        let White1 : game =
            {
                gameState = GameState.Create.fromFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 1 1";
                moves = [
                    Moves.WhiteMove1;
                ]
            }
        let Black1 : game =
            {
                gameState = GameState.Create.fromFen "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 2 1";
                moves = [
                    Moves.BlackMove1;
                    Moves.WhiteMove1;
                ]
            }
        let White2 : game =
            {
                gameState = GameState.Create.fromFen "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 3 2";
                moves = [
                    Moves.WhiteMove2;
                    Moves.BlackMove1;
                    Moves.WhiteMove1;
                ]
            }
        let Black2 : game =
            {
                gameState = GameState.Create.fromFen "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPP1PPP/RNBQKBNR w KQkq - 4 2";
                moves = [
                    Moves.BlackMove2;
                    Moves.WhiteMove2;
                    Moves.BlackMove1;
                    Moves.WhiteMove1;
                ]
            }