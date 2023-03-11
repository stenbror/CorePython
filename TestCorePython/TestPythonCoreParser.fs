module Tests

open System
open Xunit
open CorePython.PythonCoreParser


// UnitTests for reserved keywords ////////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Reserved keyword 'False'`` () =
    let res = IsReservedKeywordSymbol("False", 11u)
    Assert.True( match res with | Some(Symbol.PyFalse(11u, 16u)) -> true | _ -> false )
    
[<Fact>]
let ``Reserved keyword 'None'`` () =
    let res = IsReservedKeywordSymbol("None", 11u)
    Assert.True( match res with | Some(Symbol.PyNone(11u, 15u)) -> true | _ -> false )
    
[<Fact>]
let ``Reserved keyword 'True'`` () =
    let res = IsReservedKeywordSymbol("True", 11u)
    Assert.True( match res with | Some(Symbol.PyTrue(11u, 15u)) -> true | _ -> false )
    
    
    
// UnitTests for operators and delimiters /////////////////////////////////////////////////////////////////////////////   
    
[<Fact>]
let ``Operator or delimiter '**='`` () =
    let res = IsOperatorOrDelimiterSymbol( ( '*', '*', '=' ), 5u )
    Assert.True( match res with | Some( Symbol.PyPowerAssign(5u, 8u), 3uy ) -> true | _ -> false )
    
[<Fact>]
let ``Operator or delimiter '**'`` () =
    let res = IsOperatorOrDelimiterSymbol( ( '*', '*', ' ' ), 5u )
    Assert.True( match res with | Some( Symbol.PyPower(5u, 7u), 2uy ) -> true | _ -> false )
    
[<Fact>]
let ``Operator or delimiter '*='`` () =
    let res = IsOperatorOrDelimiterSymbol( ( '*', '=', ' ' ), 5u )
    Assert.True( match res with | Some( Symbol.PyMulAssign(5u, 7u), 2uy ) -> true | _ -> false )
    
[<Fact>]
let ``Operator or delimiter '*'`` () =
    let res = IsOperatorOrDelimiterSymbol( ( '*', ' ', ' ' ), 5u )
    Assert.True( match res with | Some( Symbol.PyMul(5u, 6u), 1uy ) -> true | _ -> false )