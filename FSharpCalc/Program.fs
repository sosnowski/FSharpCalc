open Calc

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let input = "(5 + (1 + 2)) * 4 - 3";

    printfn "Input: %s" input
    printfn "Result: %A" (RPN.calculateRPN (Parser.parseInfixNotation "(5 + (1 + 2)) * 4 - 3"))

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
