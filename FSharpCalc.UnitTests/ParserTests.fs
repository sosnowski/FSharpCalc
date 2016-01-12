namespace UnitTests

module ParserTests =
    open Xunit
    open Calc
    open Parser

    [<Fact>]
    let ``Should convert infix addition to RPN`` () =
        let res = [
            ParserTokens.RPNNumber 6.0;
            ParserTokens.RPNNumber 3.0;
            OPERATORS.["ADD"]
            ]

        Xunit.Assert.Equal<ParserTokens list>(res, parseInfixNotation("6 + 3"))
        
    
    [<Fact>]
    let ``Should convert infix substraction to RPN`` () =
        let res = [
            ParserTokens.RPNNumber 15.0;
            ParserTokens.RPNNumber 11.0;
            OPERATORS.["SUBSTRACT"]
            ]

        Xunit.Assert.Equal<ParserTokens list>(res, parseInfixNotation("15 - 11"))

    [<Fact>]
    let ``Should covert infiv multiplication to RPN`` () =
        let res = [
            ParserTokens.RPNNumber 4.0;
            ParserTokens.RPNNumber 2.0;
            OPERATORS.["MULTIPLY"]
            ]

        Xunit.Assert.Equal<ParserTokens list>(res, parseInfixNotation("4 * 2"))

    [<Fact>]
    let ``Should convert infic division to RPN`` () =
        let res = [
            ParserTokens.RPNNumber 8.0;
            ParserTokens.RPNNumber 2.0;
            OPERATORS.["DIVIDE"]
            ]

        Xunit.Assert.Equal<ParserTokens list>(res, parseInfixNotation("8 / 2"))

    [<Fact>]
    let ``Should deal with multiple operators`` () =
        let res = [
            ParserTokens.RPNNumber 8.0;
            ParserTokens.RPNNumber 2.0;
            OPERATORS.["ADD"];
            ParserTokens.RPNNumber 5.0;
            OPERATORS.["SUBSTRACT"]
            ]

        Xunit.Assert.Equal<ParserTokens list>(res, parseInfixNotation(" 8  + 2 - 5"))

    [<Fact>]
    let ``Should take operators precendense in to consideration`` () =
        let res = [
            ParserTokens.RPNNumber 2.0;
            ParserTokens.RPNNumber 5.0;
            ParserTokens.RPNNumber 3.0;
            OPERATORS.["MULTIPLY"];
            OPERATORS.["ADD"]
            ]

        Xunit.Assert.Equal<ParserTokens list>(res, parseInfixNotation(" 2 + 5 * 3"))

    [<Fact>]
    let ``Should properly deal with parenthesis`` () =
        let res = [
            ParserTokens.RPNNumber 2.0;
            ParserTokens.RPNNumber 4.0;
            ParserTokens.RPNNumber 3.0;
            OPERATORS.["ADD"];
            OPERATORS.["MULTIPLY"]
            ]

        Xunit.Assert.Equal<ParserTokens list>(res, parseInfixNotation(" 2 * (4 + 3)"))

    [<Fact>]
    let ``Should deal with a complex equation`` () =
        let res = [
            ParserTokens.RPNNumber 2.0;
            ParserTokens.RPNNumber 7.0;
            OPERATORS.["ADD"];
            ParserTokens.RPNNumber 3.0;
            OPERATORS.["DIVIDE"];
            ParserTokens.RPNNumber 14.0;
            ParserTokens.RPNNumber 3.0;
            OPERATORS.["SUBSTRACT"];
            ParserTokens.RPNNumber 4.0;
            OPERATORS.["MULTIPLY"];
            OPERATORS.["ADD"];
            ParserTokens.RPNNumber 2.0;
            OPERATORS.["DIVIDE"]
            ]

        Xunit.Assert.Equal<ParserTokens list>(res, parseInfixNotation("((2 + 7) / 3 + (14 - 3) * 4) / 2"))