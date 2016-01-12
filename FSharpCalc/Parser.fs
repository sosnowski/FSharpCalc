namespace Calc

module Parser =
    open FParsec

    type OPERATOR_TYPES =
        | ADD
        | SUBSTRACT
        | MULTIPLY
        | DIVIDE

    type PARENTHESIS_TYPES =
        | LEFT
        | RIGHT

    type ParserTokens =
        | RPNNumber of float
        | RPNOperator of int * OPERATOR_TYPES
        | RPNParenthesis of PARENTHESIS_TYPES

    let OPERATORS = [
        ("ADD", RPNOperator(1, OPERATOR_TYPES.ADD));
        ("SUBSTRACT", RPNOperator(1, OPERATOR_TYPES.SUBSTRACT));
        ("MULTIPLY", RPNOperator(2, OPERATOR_TYPES.MULTIPLY));
        ("DIVIDE", RPNOperator(2, OPERATOR_TYPES.DIVIDE)) ] |> Map.ofList

    let PARENTHESIS = [
        ("LEFT", RPNParenthesis PARENTHESIS_TYPES.LEFT);
        ("RIGHT", RPNParenthesis PARENTHESIS_TYPES.RIGHT) ] |> Map.ofList

    let parseInfixNotation (input:string) =
        let parseOperator = 
            choice [
                stringReturn "+" (OPERATORS.["ADD"]);
                stringReturn "-" (OPERATORS.["SUBSTRACT"]);
                stringReturn "*" (OPERATORS.["MULTIPLY"]);
                stringReturn "/" (OPERATORS.["DIVIDE"]);
            ]
        let parseNumber = pfloat |>> RPNNumber
        let parseParenthesis = (stringReturn "(" (PARENTHESIS.["LEFT"])) <|> (stringReturn ")" (PARENTHESIS.["RIGHT"]))

        let emptyStack (output, stack) =
            let rec popStack output stack =
                match stack with
                    | head::tail -> popStack (head::output) tail
                    | [] -> (output, stack)
            
            match (popStack output stack) with
                | (output, stack) -> output
        
        let rec handleOperator (operator:ParserTokens) (output:ParserTokens list) (stack:ParserTokens list) =
            match stack with
                | head::tail -> 
                    match head with
                        | RPNOperator (op2Priority, op2Type) ->
                            match operator with
                                | RPNOperator (opPriority, opType) ->
                                    if op2Priority >= opPriority then handleOperator operator (head::output) tail
                                    else (output, operator::stack)
                                | _ -> failwith "Wrong operator type"
                        | _ -> (output, operator::stack)
                | [] -> (output, operator::stack)

        let handleParenthesis (parenthesis:ParserTokens) (output:ParserTokens list) (stack:ParserTokens list) =
            let rec popOperator (output:ParserTokens list) (stack:ParserTokens list) =
                match stack with
                    | head::tail -> 
                        match head with
                            | RPNParenthesis _ -> 
                                if head = PARENTHESIS.["LEFT"] then (output, tail)
                                else failwith "Invalid operator in the queue"
                            | _ -> popOperator (head::output) tail
                    | [] -> (output, stack)

            match parenthesis with
                | RPNParenthesis _ -> 
                    if (parenthesis = PARENTHESIS.["LEFT"]) then (output, parenthesis::stack)
                    else popOperator output stack
                | _ -> failwith "Wrong parenthesis type"

        let handleNumber number output stack =
            ((RPNNumber number)::output, stack)

        let handleNextToken token (output, stack) =
            match token with
                | RPNNumber num -> handleNumber num output stack
                | RPNOperator _ -> handleOperator token output stack
                | RPNParenthesis _ -> handleParenthesis token output stack

        let rec nextToken input (output, stack) =
            match String.length input with
                | 0 -> (output, stack)
                | _ ->
                    match (run (spaces >>. (parseNumber <|> parseOperator <|> parseParenthesis)) input) with
                        | Failure(str, err, _) -> failwith (sprintf "PARSER ERROR: %s" str)
                        | Success(res, _, position) ->
                            (output, stack) |> handleNextToken res |> nextToken (input.[int(position.Index)..])

        let output : ParserTokens list = []
        let stack : ParserTokens list = []
        nextToken input (output, stack) |> emptyStack |> List.rev