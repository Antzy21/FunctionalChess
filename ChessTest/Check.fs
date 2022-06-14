namespace Check

open Xunit
open TestHelpers

module White =

    [<Fact>]
    let ``White in check from Rook`` () =
        gameStateIsInCheck "3kr3/8/8/8/8/8/8/4K3 w KQkq - 1 2"
        |> Assert.True
    
module Black = 

    [<Fact>]
    let ``Black in check from Rook`` () =
        gameStateIsInCheck "4k3/8/8/8/8/8/8/4RK2 b KQkq - 1 2"
        |> Assert.True
