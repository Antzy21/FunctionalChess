namespace Game

open Xunit
open Chess

module ThreeMoveRepetition =
    [<Fact>]
    let ``Trivial case with 3 repeats`` () =
        let gameWithThreeRepeatedFens = 
            let newGameState = GameState.Create.newGame ()
            {
                moves = [];
                fens = fens[("fenString", 3)];
                gameState = newGameState
            }
        Game.isGameOver gameWithThreeRepeatedFens
        |> Assert.True
        
    [<Fact>]
    let ``Trivial case not with 3 repeats`` () =
        let gameWithThreeRepeatedFens = 
            let newGameState = GameState.Create.newGame ()
            {
                moves = [];
                fens = fens[("fenString", 2)];
                gameState = newGameState
            }
        Game.isGameOver gameWithThreeRepeatedFens
        |> Assert.False