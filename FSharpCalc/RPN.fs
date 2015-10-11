module RPN
    open FParsec

    type OPERATORS =
        | ADD
        | SUBSTRACT
        | MULTIPLY
        | DIVIDE
    type RPNValue =
        | RPNNumber of float
        | RPNOperator of OPERATORS

    let pOperator = 
        choice [
            stringReturn "+" (RPNOperator OPERATORS.ADD);
            stringReturn "-" (RPNOperator OPERATORS.SUBSTRACT);
            stringReturn "*" (RPNOperator OPERATORS.MULTIPLY);
            stringReturn "/" (RPNOperator OPERATORS.DIVIDE)
        ]
    let pNumber = pfloat |>> RPNNumber

    let calculateRPN str = 
        let applyOperator op stack =
            let operator = match op with
                | OPERATORS.ADD -> (+)
                | OPERATORS.SUBSTRACT -> (-)
                | OPERATORS.MULTIPLY -> (*)
                | OPERATORS.DIVIDE -> (/)
            
            match stack with
                | first::second::tail -> (operator second first) :: tail
                | [_] | [] -> failwith "Not enough arguments for operator!"

        let handleNewInput input stack =
            match input with
                | RPNNumber num -> num :: stack
                | RPNOperator op -> stack |> applyOperator op
        
        let rec nextInput str (stack: float list) =
            match String.length str with
                | 0 -> stack
                | _ ->
                    match (run (spaces >>. (pNumber <|> pOperator)) str) with
                        | Failure(str, err, _) -> failwith (sprintf "PARSER ERROR: %s" str)
                        | Success(res, _, position) ->
                            stack |> handleNewInput res |> nextInput (str.[int(position.Index)..])
        
        nextInput str []
        //nextInput str 0 (String.length str) numbersStack