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
    
[<Fact>]
let ``Parse Expression rule 'atom' with single String literal`` () =
    let stream = [ Symbol.PyString( 10u, 22u, "Hello, world!" );  ]
    let node, rest  = stream |> ParseAtom
    let expecting = AbstractSyntaxNodes.String( 10u, 22u, [| Symbol.PyString( 10u, 22u, "Hello, world!" ) |] );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'atom' with multiple String literal`` () =
    let stream = [ Symbol.PyString( 10u, 22u, "Hello, world!" )
                   Symbol.PyString( 26u, 30u, "Mimmi" ) ]
    let node, rest  = stream |> ParseAtom
    let expecting = AbstractSyntaxNodes.String( 10u, 30u, [|
        Symbol.PyString( 10u, 22u, "Hello, world!" )
        Symbol.PyString( 26u, 30u, "Mimmi" ) |] );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'atom expr' with await and no trailer`` () =
    let stream = [ Symbol.PyAwait( 0u, 4u ); Symbol.PyName( 5u, 12u, "__lock__" );  ]
    let node, rest  = stream |> ParseAtomExpr
    let expecting = AbstractSyntaxNodes.AtomExpr( 0u, 12u,
                                                  Some( Symbol.PyAwait( 0u, 4u ) ),
                                                  AbstractSyntaxNodes.Name(5u, 12u, Symbol.PyName( 5u, 12u, "__lock__" ) ),
                                                  Option.None )
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'atom expr' with Name literal without await and trailer`` () =
    let stream = [ Symbol.PyName( 10u, 17u, "__init__" );  ]
    let node, rest  = stream |> ParseAtomExpr
    let expecting = AbstractSyntaxNodes.Name( 10u, 17u, Symbol.PyName( 10u, 17u, "__init__" ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'power expr'`` () =
    let stream = [ Symbol.PyName( 0u, 1u, "a" ); Symbol.PyPower( 2u, 3u ); Symbol.PyNumber( 4u, 5u, "8" ); ]
    let node, rest  = stream |> ParsePower
    let expecting = AbstractSyntaxNodes.Power( 0u, 5u, Name( 0u, 1u, PyName( 0u, 1u, "a") ), Symbol.PyPower( 2u, 3u ), Number( 4u, 5u, PyNumber( 4u, 5u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'unary expr' with '+'`` () =
    let stream = [ PyPlus( 2u, 3u ); PyNumber( 4u, 5u, "8" ); ]
    let node, rest  = stream |> ParseFactor
    let expecting = AbstractSyntaxNodes.UnaryPlus( 2u, 5u, PyPlus( 2u, 3u ), Number( 4u, 5u, PyNumber( 4u, 5u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'unary expr' with '-'`` () =
    let stream = [ PyMinus( 2u, 3u ); PyNumber( 4u, 5u, "8" ); ]
    let node, rest  = stream |> ParseFactor
    let expecting = AbstractSyntaxNodes.UnaryMinus( 2u, 5u, PyMinus( 2u, 3u ), Number( 4u, 5u, PyNumber( 4u, 5u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'unary expr' with '~'`` () =
    let stream = [ PyBitwiseInvert( 2u, 3u ); PyNumber( 4u, 5u, "8" ); ]
    let node, rest  = stream |> ParseFactor
    let expecting = AbstractSyntaxNodes.BitwiseInvert( 2u, 5u, PyBitwiseInvert( 2u, 3u ), Number( 4u, 5u, PyNumber( 4u, 5u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'term' with '*'`` () =
    let stream = [ Symbol.PyName( 0u, 0u, "a" ); Symbol.PyMul( 1u, 1u ); Symbol.PyNumber( 2u, 2u, "8" ); ]
    let node, rest  = stream |> ParseTerm
    let expecting = AbstractSyntaxNodes.Mul( 0u, 2u, Name( 0u, 0u, PyName( 0u, 0u, "a") ), Symbol.PyMul( 1u, 1u ), Number( 2u, 2u, PyNumber( 2u, 2u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'term' with '/'`` () =
    let stream = [ Symbol.PyName( 0u, 0u, "a" ); Symbol.PyDiv( 1u, 1u ); Symbol.PyNumber( 2u, 2u, "8" ); ]
    let node, rest  = stream |> ParseTerm
    let expecting = AbstractSyntaxNodes.Div( 0u, 2u, Name( 0u, 0u, PyName( 0u, 0u, "a") ), Symbol.PyDiv( 1u, 1u ), Number( 2u, 2u, PyNumber( 2u, 2u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'term' with '%'`` () =
    let stream = [ Symbol.PyName( 0u, 0u, "a" ); Symbol.PyModulo( 1u, 1u ); Symbol.PyNumber( 2u, 2u, "8" ); ]
    let node, rest  = stream |> ParseTerm
    let expecting = AbstractSyntaxNodes.Modulo( 0u, 2u, Name( 0u, 0u, PyName( 0u, 0u, "a") ), Symbol.PyModulo( 1u, 1u ), Number( 2u, 2u, PyNumber( 2u, 2u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'term' with 'matrices'`` () =
    let stream = [ Symbol.PyName( 0u, 0u, "a" ); Symbol.PyMatrice( 1u, 1u ); Symbol.PyNumber( 2u, 2u, "8" ); ]
    let node, rest  = stream |> ParseTerm
    let expecting = AbstractSyntaxNodes.Matrices( 0u, 2u, Name( 0u, 0u, PyName( 0u, 0u, "a") ), Symbol.PyMatrice( 1u, 1u ), Number( 2u, 2u, PyNumber( 2u, 2u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'term' with '//'`` () =
    let stream = [ Symbol.PyName( 0u, 0u, "a" ); Symbol.PyFloorDiv( 1u, 2u ); Symbol.PyNumber( 3u, 3u, "8" ); ]
    let node, rest  = stream |> ParseTerm
    let expecting = AbstractSyntaxNodes.FloorDiv( 0u, 3u, Name( 0u, 0u, PyName( 0u, 0u, "a") ), Symbol.PyFloorDiv( 1u, 2u ), Number( 3u, 3u, PyNumber( 3u, 3u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'arith expr' with '+'`` () =
    let stream = [ Symbol.PyName( 0u, 0u, "a" ); Symbol.PyPlus( 1u, 1u ); Symbol.PyNumber( 2u, 2u, "8" ); ]
    let node, rest  = stream |> ParseArithExpr
    let expecting = AbstractSyntaxNodes.Plus( 0u, 2u, Name( 0u, 0u, PyName( 0u, 0u, "a") ), Symbol.PyPlus( 1u, 1u ), Number( 2u, 2u, PyNumber( 2u, 2u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )
    
[<Fact>]
let ``Parse Expression rule 'arith expr' with '-'`` () =
    let stream = [ Symbol.PyName( 0u, 0u, "a" ); Symbol.PyMinus( 1u, 1u ); Symbol.PyNumber( 2u, 2u, "8" ); ]
    let node, rest  = stream |> ParseArithExpr
    let expecting = AbstractSyntaxNodes.Minus( 0u, 2u, Name( 0u, 0u, PyName( 0u, 0u, "a") ), Symbol.PyMinus( 1u, 1u ), Number( 2u, 2u, PyNumber( 2u, 2u, "8" ) ) );
    Assert.Equal( expecting, node )
    Assert.True( List.isEmpty rest )