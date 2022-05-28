open Chess

[<EntryPoint>]
let main args =
    let fens = [
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2";
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        "rnbqkbnr/pppp1p1p/8/4pPp1/4P3/8/PPPP2PP/RNBQKBNR w KQkq g6 0 1";
    ]
    fens |> List.iter (fun fen ->
        let gamestate = GameState.fromFen fen
        gamestate.board |> Board.print
    )
    
    //|> Board.getPossibleMoves White
    //|> List.iter (printfn "%s")
    0