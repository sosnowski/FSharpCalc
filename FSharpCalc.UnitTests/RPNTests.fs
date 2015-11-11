namespace UnitTests

module RPNUnitTests =
    open Xunit
    open Calc

    [<Fact>]
    let ``Should add two values`` () =
        Assert.Equal(9.0, RPN.calculateRPN("3 6 +"))
    
    [<Fact>]
    let ``Should work properly with typical floats`` () =
        Assert.Equal(10.3, RPN.calculateRPN("5.1 5.2 +"))

    [<Fact>]
    let ``Should substract two numbers`` () =
        Assert.Equal(5.0, RPN.calculateRPN("8 3 -"))

    [<Fact>]
    let ``Should divide two numbers `` () =
        Assert.Equal(3.0, RPN.calculateRPN("30 10 /"))

    [<Fact>]
    let ``Should multiply two numbers `` () =
        Assert.Equal(15.0, RPN.calculateRPN("3 5 *"))

    [<Fact>]
    let ``Should solve complex equation with many operations`` () =
        Assert.Equal(2.0, RPN.calculateRPN("3 3 4 + * 7 / 1 -"))