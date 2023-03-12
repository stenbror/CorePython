module CorePython.Compiler.Parser.Tests

open Xunit
open CorePython.Compiler.PythonCoreParser


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
    
    
    
// UnitTests for expression rules of parser ///////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Parse Expression rule 'atom' with None literal`` () =
    let stream = [ Symbol.PyNone( 10u, 13u );  ]
    let node, rest  = stream |> ParseAtom
    let expecting = AbstractSyntaxNodes.None( 10u, 13u, Symbol.PyNone( 10u, 13u ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )

[<Fact>]
let ``Parse Expression rule 'atom' with False literal`` () =
    let stream = [ Symbol.PyFalse( 10u, 14u );  ]
    let node, rest  = stream |> ParseAtom
    let expecting = AbstractSyntaxNodes.False( 10u, 14u, Symbol.PyFalse( 10u, 14u ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )

[<Fact>]
let ``Parse Expression rule 'atom' with True literal`` () =
    let stream = [ Symbol.PyTrue( 10u, 13u );  ]
    let node, rest  = stream |> ParseAtom
    let expecting = AbstractSyntaxNodes.True( 10u, 13u, Symbol.PyTrue( 10u, 13u ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )

[<Fact>]
let ``Parse Expression rule 'atom' with '...' literal`` () =
    let stream = [ Symbol.PyEllipsis( 10u, 12u );  ]
    let node, rest  = stream |> ParseAtom
    let expecting = AbstractSyntaxNodes.Ellipsis( 10u, 12u, Symbol.PyEllipsis( 10u, 12u ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )

[<Fact>]
let ``Parse Expression rule 'atom' with Name literal`` () =
    let stream = [ Symbol.PyName( 10u, 17u, "__init__" );  ]
    let node, rest  = stream |> ParseAtom
    let expecting = AbstractSyntaxNodes.Name( 10u, 17u, Symbol.PyName( 10u, 17u, "__init__" ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'atom' with Number literal`` () =
    let stream = [ Symbol.PyNumber( 10u, 13u, "0xff" );  ]
    let node, rest  = stream |> ParseAtom
    let expecting = AbstractSyntaxNodes.Number( 10u, 13u, Symbol.PyNumber( 10u, 13u, "0xff" ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )