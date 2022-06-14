﻿namespace Moves

open Xunit
open Chess
open TestHelpers

module White =

    [<Fact>]
    let ``White's opening moves`` () =
        let moves = 
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
            |> getPossibleMoves
        Assert.StrictEqual<string list>([
            "Pa2 -> a3";
            "Pa2 -> a4";
            "Nb1 -> c3";
            "Nb1 -> a3";
            "Pb2 -> b3";
            "Pb2 -> b4";
            "Pc2 -> c3";
            "Pc2 -> c4";
            "Pd2 -> d3";
            "Pd2 -> d4";
            "Pe2 -> e3";
            "Pe2 -> e4";
            "Pf2 -> f3";
            "Pf2 -> f4";
            "Ng1 -> h3";
            "Ng1 -> f3";
            "Pg2 -> g3";
            "Pg2 -> g4";
            "Ph2 -> h3";
            "Ph2 -> h4";
        ], moves)
    
module Black = 
    
    [<Fact>]
    let ``Black's move options`` () =
        let moves = 
            "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
            |> getPossibleMoves
        Assert.StrictEqual<string list>([
            "Pa7 -> a6";
            "Pa7 -> a5";
            "Pb7 -> b6";
            "Pb7 -> b5";
            "Nb8 -> c6";
            "Nb8 -> a6";
            "Pc5 -> c4";
            "Pd7 -> d6";
            "Pd7 -> d5";
            "Qd8 -> c7";
            "Qd8 -> b6";
            "Qd8 -> a5";
            "Pe7 -> e6";
            "Pe7 -> e5";
            "Pf7 -> f6";
            "Pf7 -> f5";
            "Pg7 -> g6";
            "Pg7 -> g5";
            "Ng8 -> h6";
            "Ng8 -> f6";
            "Ph7 -> h6";
            "Ph7 -> h5";
        ], moves)