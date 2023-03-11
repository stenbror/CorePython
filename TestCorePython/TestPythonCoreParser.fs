module Tests

open System
open Xunit
open CorePython.PythonCoreParser

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