namespace Game

open Xunit
open Chess
open FSharp.Extensions

module ThreeMoveRepetition =
    [<Fact>]
    let ``Trivial case with 3 repeats`` () =
        let gameWithThreeRepeatedFens = 
            let newGameState = GameState.Create.newGame ()
            {
                moves = [];
                previousBoardOccurrences = boardOccurenceCounter[(newGameState.board, 3)];
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
                previousBoardOccurrences = boardOccurenceCounter[(newGameState.board, 2)];
                gameState = newGameState
            }
        Game.isGameOver gameWithThreeRepeatedFens
        |> Assert.False
                
    [<Fact>]
    let ``Make 3 repeated moves`` () =
        let game =
            let newGameState = GameState.Create.fromFen "8/8/8/8/8/k7/8/5qQK b - - 0 0"
            {
                moves = [];
                previousBoardOccurrences = boardOccurenceCounter[];
                gameState = newGameState
            }
            
        let notGameOverGame =
            game
            |> Game.Update.makeMoveFromNotation "qh3" |> Result.failOnError
            |> Game.Update.makeMoveFromNotation "Qh2" |> Result.failOnError
            |> Game.Update.makeMoveFromNotation "qf1" |> Result.failOnError
            |> Game.Update.makeMoveFromNotation "Qg1" |> Result.failOnError
            |> Game.Update.makeMoveFromNotation "qh3" |> Result.failOnError
            |> Game.Update.makeMoveFromNotation "Qh2" |> Result.failOnError
            |> Game.Update.makeMoveFromNotation "qf1" |> Result.failOnError
            |> Game.Update.makeMoveFromNotation "Qg1" |> Result.failOnError
        
        notGameOverGame
        |> Game.isGameOver
        |> Assert.False
        
        notGameOverGame
        |> Game.Update.makeMoveFromNotation "qh3" |> Result.failOnError
        |> Game.isGameOver
        |> Assert.True
        