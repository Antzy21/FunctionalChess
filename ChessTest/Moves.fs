module Moves

open Xunit
open ChessTest.Helpers.Functions

[<Fact>]
let ``Opening moves for White`` () =
    let moves = 
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
        |> GetPossibleMoves.all
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
    
[<Fact>]
let ``Move options for Black`` () =
    let moves = 
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
        |> GetPossibleMoves.all
    Assert.StrictEqual<string list>([
        "pa7 -> a6";
        "pa7 -> a5";
        "pb7 -> b6";
        "pb7 -> b5";
        "nb8 -> c6";
        "nb8 -> a6";
        "pc5 -> c4";
        "pd7 -> d6";
        "pd7 -> d5";
        "qd8 -> c7";
        "qd8 -> b6";
        "qd8 -> a5";
        "pe7 -> e6";
        "pe7 -> e5";
        "pf7 -> f6";
        "pf7 -> f5";
        "pg7 -> g6";
        "pg7 -> g5";
        "ng8 -> h6";
        "ng8 -> f6";
        "ph7 -> h6";
        "ph7 -> h5";
    ], moves)

[<Fact>]
let ``pd5 -> xe4`` () =
    let moves = 
        "rnbqkbnr/pp1ppppp/8/3p4/3PP3/8/PPP2PPP/RNBQKBNR b KQkq - 1 2"
        |> GetPossibleMoves.fromSquare "d5"
    Assert.StrictEqual<string list>([
        "pd5 -> xPe4";
    ], moves)