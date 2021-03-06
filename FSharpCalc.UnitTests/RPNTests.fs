﻿namespace UnitTests

module RPNTests =
    open Xunit
    open Calc
    open Parser

    [<Fact>]
    let ``Should add two values`` () =
        let res = [
            ParserTokens.RPNNumber 6.0;
            ParserTokens.RPNNumber 3.0;
            OPERATORS.["ADD"]
            ]

        Xunit.Assert.Equal(9.0, RPN.calculateRPN(res))
        
    
    [<Fact>]
    let ``Should substract two values`` () =
        let res = [
            ParserTokens.RPNNumber 15.0;
            ParserTokens.RPNNumber 11.0;
            OPERATORS.["SUBSTRACT"]
            ]

        Xunit.Assert.Equal(4.0, RPN.calculateRPN(res))

    [<Fact>]
    let ``Should multiply two values`` () =
        let res = [
            ParserTokens.RPNNumber 4.0;
            ParserTokens.RPNNumber 2.0;
            OPERATORS.["MULTIPLY"]
            ]

        Xunit.Assert.Equal(8.0, RPN.calculateRPN(res))

    [<Fact>]
    let ``Should divide two values`` () =
        let res = [
            ParserTokens.RPNNumber 8.0;
            ParserTokens.RPNNumber 2.0;
            OPERATORS.["DIVIDE"]
            ]

        Xunit.Assert.Equal(4.0, RPN.calculateRPN(res))

    [<Fact>]
    let ``Should properly deal with two operators`` () =
        let res = [
            ParserTokens.RPNNumber 8.0;
            ParserTokens.RPNNumber 2.0;
            OPERATORS.["ADD"];
            ParserTokens.RPNNumber 5.0;
            OPERATORS.["SUBSTRACT"]
            ]

        Xunit.Assert.Equal(5.0, RPN.calculateRPN(res))

    [<Fact>]
    let ``Should properly deal with operators with different precedences`` () =
        let res = [
            ParserTokens.RPNNumber 2.0;
            ParserTokens.RPNNumber 5.0;
            ParserTokens.RPNNumber 3.0;
            OPERATORS.["MULTIPLY"];
            OPERATORS.["ADD"]
            ]

        Xunit.Assert.Equal(17.0, RPN.calculateRPN(res))


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

        Xunit.Assert.Equal(23.5, RPN.calculateRPN(res))