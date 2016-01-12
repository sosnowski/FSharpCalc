namespace Calc

module RPN =
    open Parser

    let OPERATORS = [
        (OPERATOR_TYPES.ADD, (+));
        (OPERATOR_TYPES.DIVIDE, (/));
        (OPERATOR_TYPES.MULTIPLY, (*));
        (OPERATOR_TYPES.SUBSTRACT, (-))] |> Map.ofList


    let calculateRPN (rpnInput) =
        let resStack: float list = []

        let handleOperator stack opType =
            match stack with
                | num1::num2::tail -> (OPERATORS.[opType] num2 num1)::tail
                | [_] | [] -> failwith "Not enought operator arguments!"

        let rec nextToken rpnInput resStack =
            match rpnInput with
                | head::tail -> 
                    match head with
                        | RPNNumber num -> nextToken tail (num::resStack)
                        | RPNOperator (opPriority, opType) -> nextToken tail (handleOperator resStack opType)
                        | _ -> failwith "Invalid input token"
                            
                | [] -> (rpnInput, resStack)
        
        match (nextToken rpnInput resStack) with
            | (input, output) -> output.[0]