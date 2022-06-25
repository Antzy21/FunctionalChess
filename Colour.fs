namespace Chess

type colour = 
    | White
    | Black

module Colour =
    let opposite (colour: colour) : colour =
        match colour with
        | White -> Black
        | Black -> White
    let tryParse (colour: string) : colour option =
        match colour.ToUpper() with
        | "WHITE" | "W" -> Some White
        | "BLACK" | "B" -> Some Black
        | _ -> None
