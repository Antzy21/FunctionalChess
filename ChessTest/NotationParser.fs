namespace NotationParser

open Xunit
open Chess
open Checkerboard
open TestHelpers

module White =

    [<Fact>]
    let ``Parse e4 move`` () =
        let game = GameState.newGame ()
        let fullNotation = moveNotationFromNotationParser game "e4" 
        Assert.Equal("Pe2 -> e4", fullNotation)

    [<Fact>]
    let ``Parse Nc3 move`` () =
        let game = GameState.newGame ()
        let fullNotation = moveNotationFromNotationParser game "Nc3"
        Assert.Equal("Nb1 -> c3", fullNotation)
    
    [<Fact>]
    let ``Parse xd5 move`` () =
        let game = GameState.fromFen "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 2 1"
        let fullNotation = moveNotationFromNotationParser game "xd5"        
        Assert.Equal("Pe4 -> xPd5", fullNotation)

    [<Fact>]
    let ``Parse Enpassant`` () =
        let game = GameState.fromFen "rnbqkbnr/1ppppppp/8/pP6/8/8/P1PPPPPP/RNBQKBNR w KQkq a6 4 2"
        let fullNotation = moveNotationFromNotationParser game "xa6"
        Assert.Equal("Pb5 -> xpa6", fullNotation)

    [<Fact>]
    let ``Parse Full Notation of e4`` () =
        let game = GameState.newGame ()
        let fullNotation = moveNotationFromNotationParser game "Pe2 -> e4" 
        Assert.Equal("Pe2 -> e4", fullNotation)
        
    [<Fact>]
    let ``Parse Full Notation of Nc3`` () =
        let game = GameState.newGame ()
        let fullNotation = moveNotationFromNotationParser game "Nb1 -> c3" 
        Assert.Equal("Nb1 -> c3", fullNotation)

module Black =

    [<Fact>]
    let ``Parse e5 move`` () =
        let game =
            GameState.newGame ()
            |> GameState.makeMoveFromNotation "e4"
        let fullNotation = moveNotationFromNotationParser game "e5" 
        Assert.Equal("pe7 -> e5", fullNotation)

    [<Fact>]
    let ``Setup a6 enpassant`` () =
        let game = 
            "rnbqkbnr/p1pppppp/6p1/1P6/8/8/P1PPPPPP/RNBQKBNR b KQkq - 3 2"
            |> GameState.fromFen
        let fullNotation = moveNotationFromNotationParser game "a5"
        Assert.Equal("pa7 -> a5", fullNotation)