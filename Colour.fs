namespace Chess

type colour = 
    | White
    | Black

module Colour =
    let opposite (colour: colour) : colour =
        match colour with
        | White -> Black
        | Black -> White
