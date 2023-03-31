﻿namespace Chess

type colour = 
    | White
    | Black

module Colour =
    let fromBool (bool: bool) : colour =
        match bool with
        | true -> White        
        | false -> Black
    let toBool (colour: colour) : bool =
        match colour with
        | White -> true
        | Black -> false
    let opposite (colour: colour) : colour =
        match colour with
        | White -> Black
        | Black -> White
    let tryParse (colour: string) : colour option =
        match colour.ToUpper() with
        | "WHITE" | "W" -> Some White
        | "BLACK" | "B" -> Some Black
        | _ -> None
    let toChar (colour: colour) : char =
        match colour with
        | White -> 'W'
        | Black -> 'B'
