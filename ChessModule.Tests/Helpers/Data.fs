module internal ChessTest.Helpers.Data

open Chess
open ChessTest.Helpers.Functions

module Pieces =
    
    let WhiteBishop = {pieceType = Bishop; colour = White}
    
module Moves =

    module EnPassant =
        let PreWhite : move =
            ({
                startingCoords = coordCntr 0 6; destinationCoords = coordCntr 0 4
            })
            |> NormalMove
        let PostWhite : move =
            ({
                startingCoords = coordCntr 1 4; destinationCoords = coordCntr 0 5;
            })
            |> EnPassant
        let PreBlack : move =
            ({
                startingCoords = coordCntr 0 1; destinationCoords = coordCntr 0 3
            })
            |> NormalMove
        let PostBlack : move =
            ({
                startingCoords = coordCntr 1 3; destinationCoords = coordCntr 0 2;
            })
            |> EnPassant
    
    module Promotion =
        let White1 : move =
            ({
                startingCoords = coordCntr 0 6; destinationCoords = coordCntr 0 7;
            })
            |> fun move -> Promotion (move, Queen)
        let White2 : move =
            ({
                startingCoords = coordCntr 0 6; destinationCoords = coordCntr 1 7;
            })
            |> fun move -> Promotion (move, Rook)

    let e4 : move = 
        ({
            startingCoords = coordCntr 4 1; destinationCoords = coordCntr 4 3;
        })
        |> NormalMove
    let d5 : move = 
        ({
            startingCoords = coordCntr 3 6; destinationCoords = coordCntr 3 4;
        })
        |> NormalMove
    let e5 : move = 
        ({
            startingCoords = coordCntr 4 6; destinationCoords = coordCntr 4 4;
        })
        |> NormalMove
    let xd5 : move =
        ({
            startingCoords = coordCntr 4 3; destinationCoords = coordCntr 3 4;
        })
        |> NormalMove
    let Nf6 : move = 
        ({
            startingCoords = coordCntr 6 7; destinationCoords = coordCntr 5 5;
        })
        |> NormalMove
    let Nc3 : move = 
        ({
            startingCoords = coordCntr 1 0; destinationCoords = coordCntr 2 2;
        })
        |> NormalMove
    let Nf3 : move = 
        ({
            startingCoords = coordCntr 6 0; destinationCoords = coordCntr 5 2;
        })
        |> NormalMove
    let Nge2 : move = 
        ({
            startingCoords = coordCntr 6 0; destinationCoords = coordCntr 4 1;
        })
        |> NormalMove

module Games =

    module Enpassant =
        let PreWhite() : game = 
            {
                gameState = GameState.Create.fromFen "4k3/8/8/pP6/8/8/8/4K3 w - a6 0 0";
                previousGameStates = Map[];
                moves = ["a5"]
            }
        let PostWhite() : game = 
            {
                gameState = GameState.Create.fromFen "4k3/8/P7/8/8/8/8/4K3 b - - 1 1"
                previousGameStates = Map[(GameState.Create.fromFen "4k3/8/P7/8/8/8/8/4K3 b - - 1 1", 1)];
                moves = ["bxa6"; "a5"]
            }
        let PreBlack() : game = 
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/Pp6/8/8/4K3 b - a3 0 0";
                previousGameStates = Map[];
                moves = ["a4"]
            }
        let PostBlack() : game =
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/8/p7/8/4K3 w - - 1 0";
                previousGameStates = Map[(GameState.Create.fromFen "4k3/8/8/8/8/p7/8/4K3 w - - 1 0", 1)];
                moves = ["bxa3"; "a4"]
            }

    module Promotion =
        let PreWhite1() : game =
            {
                gameState = GameState.Create.fromFen "8/P7/8/8/8/8/8/4K2k w - - 0 0";
                previousGameStates = Map[];
                moves = []
            }
        let PostWhite1() : game =
            {
                gameState = GameState.Create.fromFen "Q7/8/8/8/8/8/8/4K2k b - - 1 1";
                previousGameStates = Map[(GameState.Create.fromFen "Q7/8/8/8/8/8/8/4K2k b - - 1 1", 1)];
                moves = ["a8=Q"]
            }
        let PreWhite2() : game =
            {
                gameState = GameState.Create.fromFen "bn6/P7/8/8/8/8/8/4K2k w - - 0 0";
                previousGameStates = Map[];
                moves = []
            }
        let PostWhite2() : game =
            {
                gameState = GameState.Create.fromFen "bR6/8/8/8/8/8/8/4K2k b - - 1 1";
                previousGameStates = Map[(GameState.Create.fromFen "bR6/8/8/8/8/8/8/4K2k b - - 1 1", 1)];
                moves = ["axb8=R"]
            }
    
    module Castling =
        let PreWhite() : game =
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/8/8/8/R3K2R w KQkq - 0 0";
                previousGameStates = Map[];
                moves = []
            }
        let PreBlack() : game =
            {
                gameState = GameState.Create.fromFen "r3k2r/8/8/8/8/8/8/4K3 b KQkq - 0 0";
                previousGameStates = Map[];
                moves = []
            }
        let PostWhiteKing() : game =
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/8/8/8/R4RK1 b kq - 1 1";
                previousGameStates = Map[(GameState.Create.fromFen "4k3/8/8/8/8/8/8/R4RK1 b kq - 1 1", 1)];
                moves = ["0-0"]
            }
        let PostWhiteQueen() : game =
            {
                gameState = GameState.Create.fromFen "4k3/8/8/8/8/8/8/2KR3R b kq - 1 1";
                previousGameStates = Map[(GameState.Create.fromFen "4k3/8/8/8/8/8/8/2KR3R b kq - 1 1", 1)];
                moves = ["0-0-0"]
            }
        let PostBlackKing() : game =
            {
                gameState = GameState.Create.fromFen "r4rk1/8/8/8/8/8/8/4K3 w KQ - 1 0";
                previousGameStates = Map[(GameState.Create.fromFen "r4rk1/8/8/8/8/8/8/4K3 w KQ - 1 0", 1)];
                moves = ["0-0"]
            }
        let PostBlackQueen() : game =
            {
                gameState = GameState.Create.fromFen "2kr3r/8/8/8/8/8/8/4K3 w KQ - 1 0";
                previousGameStates = Map[(GameState.Create.fromFen "2kr3r/8/8/8/8/8/8/4K3 w KQ - 1 0", 1)];
                moves = ["0-0-0"]
            }

    module ExampleGame =
        let getWhite1 () : game =
            {
                gameState = GameState.Create.fromFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 1 1";
                previousGameStates = Map[
                    (GameState.Create.fromFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 1 1", 1)
                ];
                moves = [
                    "e4";
                ]
            }
        let getBlack1 () : game =
            {
                gameState = GameState.Create.fromFen "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 2 1";
                previousGameStates = Map[
                    (GameState.Create.fromFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 1 1", 1)
                    (GameState.Create.fromFen "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 2 1", 1)
                ];
                moves = [
                    "d5";
                    "e4";
                ]
            }
        let getWhite2 () : game =
            {
                gameState = GameState.Create.fromFen "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 3 2";
                previousGameStates = Map[
                    (GameState.Create.fromFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 1 1", 1);
                    (GameState.Create.fromFen "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 2 1", 1);
                    (GameState.Create.fromFen "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 3 2", 1)
                ];
                moves = [
                    "exd5";
                    "d5";
                    "e4";
                ]
            }
        let getBlack2 () : game =
            {
                gameState = GameState.Create.fromFen "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPP1PPP/RNBQKBNR w KQkq - 4 2";
                previousGameStates = Map[
                    (GameState.Create.fromFen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 1 1", 1);
                    (GameState.Create.fromFen "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 2 1", 1);
                    (GameState.Create.fromFen "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 3 2", 1);
                    (GameState.Create.fromFen "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPP1PPP/RNBQKBNR w KQkq - 4 2", 1)
                ];
                moves = [
                    "Nf6";
                    "exd5";
                    "d5";
                    "e4";
                ]
            }

    module Special =
        let knightsSharingMoveSquare() : game =
            {
                gameState = GameState.Create.fromFen "rnbqkb1r/ppp1pppp/5n2/4p3/4P3/5N2/PPPPNPPP/R1BQKB1R b KQkq - 5 3";
                previousGameStates = Map[];
                moves = [
                    "Nge2";
                    "Nf6";
                    "Nc3";
                    "e5";
                    "e4";
                ]
            }