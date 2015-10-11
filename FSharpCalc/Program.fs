// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    printfn "D: %A" (RPN.calculateRPN "2 7 + 3 / 14 3 - 4 * + 2 /")

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
