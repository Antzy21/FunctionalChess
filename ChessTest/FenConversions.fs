module FenConversions

open Xunit
open Chess

[<Fact>]
let ``Starting Fen converts with inverse`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
    fen
    |> GameState.fromFen
    |> GameState.toFen
    |> fun f -> Assert.Equal(f, fen)

[<Fact>]
let ``Empty board`` () =
    let fen = "4k3/8/8/8/8/8/8/4K3 w KQkq - 0 0"
    fen
    |> GameState.fromFen
    |> GameState.toFen
    |> fun f -> Assert.Equal(f, fen)

[<Fact>]
let ``Asymetric board`` () =
    let fen = "4k3/8/8/nNnN4/8/8/PPPP4/4K3 w KQkq - 0 0"
    fen
    |> GameState.fromFen
    |> GameState.toFen
    |> fun f -> Assert.Equal(f, fen)