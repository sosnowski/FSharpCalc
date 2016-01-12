open Calc

[<EntryPoint>]
let main argv = 
    let input = argv.[0]

    printfn "Input: %s" input
    printfn "Result: %A" (RPN.calculateRPN (Parser.parseInfixNotation input))

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
