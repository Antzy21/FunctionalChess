namespace Square

open Xunit
open Chess
open Checkerboard

module BitMap =
    
    [<Fact>]
    let ``Correctly Parses into bitmaps`` () =
        let square = Some ({pieceType = Bishop; colour = White})
        let result = Square.Parser.toBitMaps square
        let expected : squareBitMap = [true; true; false; true; false]
        Assert.Equal<squareBitMap>(expected, result)

    [<Fact>]
    let ``IsColour function works`` () =
        let result =
            Some ({pieceType = Bishop; colour = White})
            |> Square.Parser.toBitMaps
            |> Square.BitMap.isColour White
        Assert.True(result)   
        let result2 =
            Some ({pieceType = Bishop; colour = Black})
            |> Square.Parser.toBitMaps
            |> Square.BitMap.isColour Black
        Assert.True(result2)  
        let result2 =
            None
            |> Square.Parser.toBitMaps
            |> Square.BitMap.isColour Black
        Assert.False(result2)
