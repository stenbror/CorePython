
// CorePython:  Parser for Python 3.11 language with tokenizer and needed structures
//              Copyright (C) 2023 By Richard Magnor Stenbro   stenbror@hotmail.com

module CorePython.Compiler.PythonCoreParser

// Error handling system in parser ////////////////////////////////////////////////////////////////////////////////////
exception SyntaxError of uint * string


// Lexical symbols used by parser from lexer //////////////////////////////////////////////////////////////////////////
type Symbol =
    |   Empty
    |   PyFalse             of uint32 * uint32
    |   PyNone              of uint32 * uint32
    |   PyTrue              of uint32 * uint32
    |   PyAnd               of uint32 * uint32
    |   PyAs                of uint32 * uint32
    |   PyAssert            of uint32 * uint32
    |   PyAsync             of uint32 * uint32
    |   PyAwait             of uint32 * uint32
    |   PyBreak             of uint32 * uint32
    |   PyClass             of uint32 * uint32
    |   PyContinue          of uint32 * uint32
    |   PyDef               of uint32 * uint32
    |   PyDel               of uint32 * uint32
    |   PyElif              of uint32 * uint32
    |   PyElse              of uint32 * uint32
    |   PyExcept            of uint32 * uint32
    |   PyFinally           of uint32 * uint32
    |   PyFor               of uint32 * uint32
    |   PyFrom              of uint32 * uint32
    |   PyGlobal            of uint32 * uint32
    |   PyIf                of uint32 * uint32
    |   PyImport            of uint32 * uint32
    |   PyIn                of uint32 * uint32
    |   PyIs                of uint32 * uint32
    |   PyLambda            of uint32 * uint32
    |   PyNonlocal          of uint32 * uint32
    |   PyNot               of uint32 * uint32
    |   PyOr                of uint32 * uint32
    |   PyPass              of uint32 * uint32
    |   PyRaise             of uint32 * uint32
    |   PyReturn            of uint32 * uint32
    |   PyTry               of uint32 * uint32
    |   PyWhile             of uint32 * uint32
    |   PyWith              of uint32 * uint32
    |   PyYield             of uint32 * uint32
    |   PyPowerAssign       of uint32 * uint32
    |   PyPower             of uint32 * uint32
    |   PyMulAssign         of uint32 * uint32
    |   PyMul               of uint32 * uint32
    |   PyFloorDivAssign    of uint32 * uint32
    |   PyFloorDiv          of uint32 * uint32
    |   PyDivAssign         of uint32 * uint32
    |   PyDiv               of uint32 * uint32
    |   PyShiftLeftAssign   of uint32 * uint32
    |   PyShiftLeft         of uint32 * uint32
    |   PyLessEqual         of uint32 * uint32
    |   PyLess              of uint32 * uint32
    |   PyShiftRightAssign  of uint32 * uint32
    |   PyShiftRight        of uint32 * uint32
    |   PyGreaterEqual      of uint32 * uint32
    |   PyGreater           of uint32 * uint32
    |   PyEllipsis          of uint32 * uint32
    |   PyNotEqual          of uint32 * uint32
    |   PyDot               of uint32 * uint32
    |   PyPlusAssign        of uint32 * uint32
    |   PyPlus              of uint32 * uint32
    |   PyMinusAssign       of uint32 * uint32
    |   PyArrow             of uint32 * uint32
    |   PyMinus             of uint32 * uint32
    |   PyModuloAssign      of uint32 * uint32
    |   PyModulo            of uint32 * uint32
    |   PyMatriceAssign     of uint32 * uint32
    |   PyMatrice           of uint32 * uint32
    |   PyColonAssign       of uint32 * uint32
    |   PyColon             of uint32 * uint32
    |   PyBitwiseAndAssign  of uint32 * uint32        
    |   PyBitwiseAnd        of uint32 * uint32
    |   PyBitwiseOrAssign   of uint32 * uint32
    |   PyBitwiseOr         of uint32 * uint32
    |   PyBitwiseXorAssign  of uint32 * uint32
    |   PyBitwiseXor        of uint32 * uint32
    |   PyBitwiseInvert     of uint32 * uint32
    |   PySemicolon         of uint32 * uint32
    |   PyComma             of uint32 * uint32
    |   PyEqual             of uint32 * uint32
    |   PyAssign            of uint32 * uint32
    |   PyLeftParen         of uint32 * uint32
    |   PyLeftBracket       of uint32 * uint32
    |   PyLeftCurly         of uint32 * uint32
    |   PyRightParen        of uint32 * uint32
    |   PyRightBracket      of uint32 * uint32
    |   PyRightCurly        of uint32 * uint32
    |   PyName              of uint32 * uint32 * string
    |   PyNumber            of uint32 * uint32 * string
    |   PyString            of uint32 * uint32 * string
    |   TypeComment         of uint32 * uint32 * string
    |   Newline             of uint32 * uint32
    |   Indent
    |   Dedent
    |   EOF                 of uint32
     
type SymbolStream = Symbol list


// Nodes generated during parsing of source code //////////////////////////////////////////////////////////////////////
type AbstractSyntaxNodes =
    |   Empty
    |   False           of uint32 * uint32 * Symbol
    |   None            of uint32 * uint32 * Symbol 
    |   True            of uint32 * uint32 * Symbol
    |   Ellipsis        of uint32 * uint32 * Symbol
    |   Name            of uint32 * uint32 * Symbol
    |   Number          of uint32 * uint32 * Symbol
    |   String          of uint32 * uint32 * Symbol array
    |   AtomExpr        of uint32 * uint32 * Symbol option * AbstractSyntaxNodes * AbstractSyntaxNodes array option
    |   Power           of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   UnaryPlus       of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   UnaryMinus      of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   BitwiseInvert   of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   Mul             of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Div             of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   FloorDiv        of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Modulo          of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Matrices        of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Plus            of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Minus           of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    
// Parser and lexer functions /////////////////////////////////////////////////////////////////////////////////////////

let IsOperatorOrDelimiterSymbol( values : (char * char * char), startPos: uint32 ) : ( Symbol * uint8 ) option =
    match values with
    |   ( '*', '*', '=' )   -> Some( Symbol.PyPowerAssign( startPos, startPos + 3u), 3uy )
    |   ( '*', '*', _ )     -> Some( Symbol.PyPower( startPos, startPos + 2u), 2uy )
    |   ( '*', '=', _ )     -> Some( Symbol.PyMulAssign( startPos, startPos + 2u), 2uy )
    |   ( '*', _ , _ )      -> Some( Symbol.PyMul( startPos, startPos + 1u), 1uy )
    |   ( '/', '/', '=' )   -> Some( Symbol.PyFloorDivAssign( startPos, startPos + 3u), 3uy )
    |   ( '/', '/', _ )     -> Some( Symbol.PyFloorDiv( startPos, startPos + 2u), 2uy )
    |   ( '/', '=', _ )     -> Some( Symbol.PyDivAssign( startPos, startPos + 2u), 2uy )
    |   ( '/', _ , _ )      -> Some( Symbol.PyDiv( startPos, startPos + 1u), 1uy )
    |   ( '<', '<', '=' )   -> Some( Symbol.PyShiftLeftAssign( startPos, startPos + 3u), 3uy )
    |   ( '<', '<', _ )     -> Some( Symbol.PyShiftLeft( startPos, startPos + 2u), 2uy )
    |   ( '<', '=', _ )     -> Some( Symbol.PyLessEqual( startPos, startPos + 2u), 2uy )
    |   ( '<', '>', _ )     -> Some( Symbol.PyNotEqual( startPos, startPos + 2u), 2uy )
    |   ( '<', _ , _ )      -> Some( Symbol.PyLess( startPos, startPos + 1u), 1uy )
    |   ( '>', '>', '=' )   -> Some( Symbol.PyShiftRightAssign( startPos, startPos + 3u), 3uy )
    |   ( '>', '>', _ )     -> Some( Symbol.PyShiftRight( startPos, startPos + 2u), 2uy )
    |   ( '>', '=', _ )     -> Some( Symbol.PyGreaterEqual( startPos, startPos + 2u), 2uy )
    |   ( '>', _ , _ )      -> Some( Symbol.PyGreater( startPos, startPos + 1u), 1uy )
    |   ( '.', '.', '.' )   -> Some( Symbol.PyEllipsis( startPos, startPos + 3u), 3uy )
    |   ( '.', _ , _ )      -> Some( Symbol.PyDot( startPos, startPos + 1u), 1uy )
    |   ( '+', '=', _ )     -> Some( Symbol.PyPlusAssign( startPos, startPos + 2u), 2uy )
    |   ( '+', _ , _ )      -> Some( Symbol.PyPlus( startPos, startPos + 1u), 1uy )
    |   ( '-', '=', _ )     -> Some( Symbol.PyMinusAssign( startPos, startPos + 2u), 2uy )
    |   ( '-', '>', _ )     -> Some( Symbol.PyArrow( startPos, startPos + 2u), 2uy )
    |   ( '-', _ , _ )      -> Some( Symbol.PyMinus( startPos, startPos + 1u), 1uy )
    |   ( '%', '=', _ )     -> Some( Symbol.PyModuloAssign( startPos, startPos + 2u), 2uy )
    |   ( '%', _ , _ )      -> Some( Symbol.PyModulo( startPos, startPos + 1u), 1uy )
    |   ( '@', '=', _ )     -> Some( Symbol.PyMatriceAssign( startPos, startPos + 2u), 2uy )
    |   ( '@', _ , _ )      -> Some( Symbol.PyMatrice( startPos, startPos + 1u), 1uy )
    |   ( ':', '=', _ )     -> Some( Symbol.PyColonAssign( startPos, startPos + 2u), 2uy )
    |   ( ':', _ , _ )      -> Some( Symbol.PyColon( startPos, startPos + 1u), 1uy )
    |   ( '&', '=', _ )     -> Some( Symbol.PyBitwiseAndAssign( startPos, startPos + 2u), 2uy )
    |   ( '&', _ , _ )      -> Some( Symbol.PyBitwiseAnd( startPos, startPos + 1u), 1uy )
    |   ( '|', '=', _ )     -> Some( Symbol.PyBitwiseOrAssign( startPos, startPos + 2u), 2uy )
    |   ( '|', _ , _ )      -> Some( Symbol.PyBitwiseOr( startPos, startPos + 1u), 1uy )
    |   ( '^', '=', _ )     -> Some( Symbol.PyBitwiseXorAssign( startPos, startPos + 2u), 2uy )
    |   ( '^', _ , _ )      -> Some( Symbol.PyBitwiseXor( startPos, startPos + 1u), 1uy )
    |   ( '=', '=', _ )     -> Some( Symbol.PyEqual( startPos, startPos + 2u), 2uy )
    |   ( '=', _ , _ )      -> Some( Symbol.PyAssert( startPos, startPos + 1u), 1uy )
    |   ( '!', '=', _ )     -> Some( Symbol.PyNotEqual( startPos, startPos + 2u), 2uy )
    |   ( '~', _ , _ )      -> Some( Symbol.PyBitwiseInvert( startPos, startPos + 1u), 1uy )
    |   ( ';', _ , _ )      -> Some( Symbol.PySemicolon( startPos, startPos + 1u), 1uy )
    |   ( ',', _ , _ )      -> Some( Symbol.PyComma( startPos, startPos + 1u), 1uy )
    |   ( '(', _ , _ )      -> Some( Symbol.PyLeftParen( startPos, startPos + 1u), 1uy )
    |   ( '[', _ , _ )      -> Some( Symbol.PyLeftBracket( startPos, startPos + 1u), 1uy )
    |   ( '{', _ , _ )      -> Some( Symbol.PyLeftCurly( startPos, startPos + 1u), 1uy )
    |   ( ')', _ , _ )      -> Some( Symbol.PyRightParen( startPos, startPos + 1u), 1uy )
    |   ( ']', _ , _ )      -> Some( Symbol.PyRightBracket( startPos, startPos + 1u), 1uy )
    |   ( '}', _ , _ )      -> Some( Symbol.PyRightCurly( startPos, startPos + 1u), 1uy )
    |   _                   -> Option.None 

let IsReservedKeywordSymbol(text: string, startPos: uint32) : Symbol option =
    match text with
    |   "False"         -> Some( Symbol.PyFalse( startPos, startPos + 5u ) )
    |   "None"          -> Some( Symbol.PyNone( startPos, startPos + 4u ) )
    |   "True"          -> Some( Symbol.PyTrue( startPos, startPos + 4u ) )   
    |   "and"           -> Some( Symbol.PyAnd( startPos, startPos + 3u ) )
    |   "as"            -> Some( Symbol.PyAs( startPos, startPos + 2u ) )
    |   "assert"        -> Some( Symbol.PyAssert( startPos, startPos + 6u ) )
    |   "async"         -> Some( Symbol.PyAsync( startPos, startPos + 5u ) )
    |   "await"         -> Some( Symbol.PyAwait( startPos, startPos + 5u ) )
    |   "break"         -> Some( Symbol.PyBreak( startPos, startPos + 5u ) )
    |   "class"         -> Some( Symbol.PyClass( startPos, startPos + 5u ) )
    |   "continue"      -> Some( Symbol.PyContinue( startPos, startPos + 8u ) )
    |   "def"           -> Some( Symbol.PyDef( startPos, startPos + 3u ) )
    |   "del"           -> Some( Symbol.PyDel( startPos, startPos + 3u ) )
    |   "elif"          -> Some( Symbol.PyElif( startPos, startPos + 4u ) )
    |   "else"          -> Some( Symbol.PyElse( startPos, startPos + 4u ) )
    |   "except"        -> Some( Symbol.PyExcept( startPos, startPos + 6u ) )
    |   "finally"       -> Some( Symbol.PyFinally( startPos, startPos + 7u ) )
    |   "for"           -> Some( Symbol.PyFor( startPos, startPos + 3u ) )
    |   "from"          -> Some( Symbol.PyFrom( startPos, startPos + 4u ) )
    |   "global"        -> Some( Symbol.PyGlobal( startPos, startPos + 6u ) )
    |   "if"            -> Some( Symbol.PyIf( startPos, startPos + 2u ) )
    |   "import"        -> Some( Symbol.PyImport( startPos, startPos + 6u ) )
    |   "in"            -> Some( Symbol.PyIn( startPos, startPos + 2u ) )
    |   "is"            -> Some( Symbol.PyIs( startPos, startPos + 2u ) )
    |   "lambda"        -> Some( Symbol.PyLambda( startPos, startPos + 6u ) )
    |   "nonlocal"      -> Some( Symbol.PyNonlocal( startPos, startPos + 8u ) )
    |   "not"           -> Some( Symbol.PyNot( startPos, startPos + 3u ) )
    |   "or"            -> Some( Symbol.PyOr( startPos, startPos + 2u ) )
    |   "pass"          -> Some( Symbol.PyPass( startPos, startPos + 4u ) )
    |   "raise"         -> Some( Symbol.PyRaise( startPos, startPos + 5u ) )
    |   "return"        -> Some( Symbol.PyReturn( startPos, startPos + 6u ) )
    |   "try"           -> Some( Symbol.PyTry( startPos, startPos + 3u ) )
    |   "while"         -> Some( Symbol.PyWhile( startPos, startPos + 5u ) )
    |   "with"          -> Some( Symbol.PyWith( startPos, startPos + 4u ) )
    |   "yield"         -> Some( Symbol.PyYield( startPos, startPos + 5u ) )
    |   _               -> Option.None
    
// Parser utilities ///////////////////////////////////////////////////////////////////////////////////////////////////

let TryToken ( stream: SymbolStream ) : ( Symbol * SymbolStream ) option =
    match stream with
    |   symbol :: rest ->   Some(symbol, rest)
    |   _ ->    Option.None
    
let GetStartPosition ( stream: SymbolStream ) : uint =
    if stream.Length > 0 then
        match stream.Head with
        |   Indent  |   Dedent  -> 0u
        |   PyFalse( s, _ ) | PyNone( s, _ ) | PyTrue( s, _ ) | PyAnd( s, _ ) | PyAs( s, _ ) | PyAssert( s, _ )
        |   PyAsync( s, _ ) | PyAwait( s, _ ) | PyBreak( s, _ ) | PyClass( s, _ ) | PyContinue( s, _ ) | PyDef( s, _ )
        |   PyDel( s, _ ) | PyElif( s, _ ) | PyElse( s, _ ) | PyExcept( s, _ ) | PyFinally( s, _ ) | PyFor( s, _ )
        |   PyFrom( s, _ ) | PyGlobal( s, _ ) | PyIf( s, _ ) | PyImport( s, _ ) | PyIn( s, _ ) | PyIs( s, _ )
        |   PyLambda( s, _ ) | PyNonlocal( s, _ ) | PyNot( s, _ ) | PyOr( s, _ ) | PyPass( s, _ ) | PyRaise( s, _ )
        |   PyReturn( s, _ ) | PyTry( s, _ ) | PyWhile( s, _ ) | PyWith( s, _ ) | PyYield( s, _ )
                 -> s
        |   PyPowerAssign( s, _ ) | PyPower( s, _ ) | PyMulAssign( s, _ ) | PyMul( s, _ ) | PyFloorDivAssign( s, _ )
        |   PyFloorDiv( s, _ ) | PyDivAssign( s, _ ) | PyDiv( s, _ ) | PyShiftLeftAssign( s, _ ) | PyShiftLeft( s, _ )
        |   PyLessEqual( s, _ ) | PyLess( s, _ ) | PyShiftRightAssign( s, _ ) | PyShiftRight( s, _ ) | PyGreaterEqual( s, _ )
        |   PyGreater( s, _ ) | PyEllipsis( s, _ ) | PyDot( s, _ ) | PyPlusAssign( s, _ ) | PyPlus( s, _ ) | PyMinusAssign( s, _ )
        |   PyMinus( s, _ ) | PyArrow( s, _ ) | PyModuloAssign( s, _ ) | PyModulo( s, _ ) | PyMatriceAssign( s, _ )
        |   PyMatrice( s, _ ) | PyColonAssign( s, _ ) | PyColon( s, _ ) | PyBitwiseAndAssign( s, _ ) | PyBitwiseAnd( s, _ )
        |   PyBitwiseOrAssign( s, _ ) | PyBitwiseOr( s, _ ) | PyBitwiseXorAssign( s, _ ) | PyBitwiseXor( s, _ )
        |   PyBitwiseInvert( s, _ ) | PySemicolon( s, _ ) | PyComma( s, _ ) | PyEqual( s, _ ) | PyAssign( s, _ )
        |   PyNotEqual( s, _ ) | PyLeftParen( s, _ ) | PyLeftBracket( s, _ ) | PyLeftCurly( s, _ ) | PyRightParen( s, _ )
        |   PyRightBracket( s, _ ) | PyRightCurly( s, _ )
                -> s
        |   PyName( s, _ , _ ) | PyNumber( s, _ , _ ) | PyString( s, _ , _ ) -> s
        |   Newline( s, _ ) -> s
        |   EOF( s ) -> s
        |   _  ->  0u
    else 0u
    
let GetNodeEndPosition( node: AbstractSyntaxNodes ) : uint32 =
    match node with
    |   Name( _ , e , _ ) | Number( _ , e , _ ) | String( _ , e , _ )
    |   AtomExpr( _ , e , _ , _ , _  ) ->   e
    |   _   -> 0ul
    
// Parser:  Expression rules //////////////////////////////////////////////////////////////////////////////////////////

let rec ParseAtom( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    match TryToken stream with
    |   Some ( PyNone( s, e ), rest )       ->   None( s, e, List.head stream ) , rest
    |   Some ( PyFalse( s, e ), rest )      ->   False( s, e, List.head stream ) , rest
    |   Some ( PyTrue( s, e ), rest )       ->   True( s, e, List.head stream ) , rest
    |   Some ( PyEllipsis( s, e ), rest )   ->   Ellipsis( s, e, List.head stream ) , rest
    |   Some ( PyName( s, e, _  ), rest )   ->   Name( s, e, List.head stream ) , rest
    |   Some ( PyNumber( s, e, _  ), rest ) ->   Number( s, e, List.head stream ) , rest
    |   Some ( PyString( s, e, _ ), rest )  ->
            let start_pos = s
            let mutable end_pos = e
            let mutable restAgain = rest
            let mutable nodes : SymbolStream = List.Empty
            nodes <- stream.Head :: nodes
            while   restAgain.Length > 0 &&
                    match TryToken restAgain with
                    |  Some( PyString( _ , e, _ ), restNow ) ->
                            nodes <- restAgain.Head :: nodes
                            restAgain <- restNow
                            end_pos <- e
                            true
                    |       Option.None -> false
                    |       _ ->    false
                do ()
            String( start_pos, end_pos, List.toArray( List.rev nodes ) ), restAgain
    | _ ->  raise ( SyntaxError(GetStartPosition(stream), "Expecting a literal!") )

and ParseAtomExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition(stream)
    let mutable restAgain = stream
    let awaitOp = match TryToken stream with
                  |  Some( PyAwait( _ ), rest) ->
                      let op = List.head stream
                      restAgain <- rest
                      Some( op )
                  |  _ -> Option.None
    let right, rest2 = ParseAtom restAgain
    let mutable trailerList : AbstractSyntaxNodes list = []
    
    restAgain <- rest2
    while   match TryToken restAgain with
            |   Some( PyLeftParen( _ ), _ )
            |   Some( PyLeftBracket( _ ), _ )
            |   Some( PyDot( _ ), _ ) ->
                    let node5, rest5 = ParseTrailer restAgain
                    restAgain <- rest5
                    trailerList <- node5 :: trailerList
                    true
            |   _ -> false
        do ()

    match awaitOp, trailerList with
        |   Option.None, [] ->
                right, rest2
        |   _  ->
                let trailer = List.toArray( List.rev trailerList )
                match trailer.Length with
                |   0   ->
                    AtomExpr( start_pos, GetNodeEndPosition( right ), awaitOp, right, Option.None ), restAgain
                |   _ ->
                    AtomExpr( start_pos, GetNodeEndPosition(Array.last trailer), awaitOp, right, Some( trailer ) ), restAgain

and ParsePower( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition(stream)
    let left, rest = ParseAtomExpr stream
    match TryToken rest with
    |  Some( PyPower( _ ), rest2 ) ->
            let op = List.head rest
            let right, rest3 = ParseFactor rest2
            Power(start_pos, GetNodeEndPosition( right ), left, op, right), rest3
    |  _ -> left, rest

and ParseFactor( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition(stream)
    match TryToken stream with
    |  Some( PyPlus( _ ), rest ) ->
            let op = List.head stream
            let right, rest2 = ParseFactor rest
            UnaryPlus( start_pos, GetNodeEndPosition( right ), op, right ), rest2
    |  Some( PyMinus( _ ), rest ) ->
            let op = List.head stream
            let right, rest2 = ParseFactor rest
            UnaryMinus( start_pos, GetNodeEndPosition( right ), op, right  ), rest2
    |  Some( PyBitwiseInvert( _ ), rest ) ->
            let op = List.head stream
            let right, rest2 = ParseFactor rest
            BitwiseInvert( start_pos, GetNodeEndPosition( right ), op, right ), rest2
    |  _ ->
            ParsePower stream

and ParseTerm( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition( stream )
    let mutable left, rest = ParseFactor stream
    while   match TryToken rest with
            |  Some( PyMul( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseFactor rest2
                    left <- Mul( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  Some( PyDiv( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseFactor rest2
                    left <- Div( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  Some( PyFloorDiv( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseFactor rest2
                    left <- FloorDiv( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  Some( PyMatrice( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseFactor rest2
                    left <- Matrices( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  Some( PyModulo( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseFactor rest2
                    left <- Modulo( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  _ -> false
            do ()
    left, rest

and ParseArithExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition( stream )
    let mutable left, rest = ParseTerm stream
    while   match TryToken rest with
            |  Some( PyPlus( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseTerm rest2
                    left <- Plus( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  Some( PyMinus( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseTerm rest2
                    left <- Minus( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  _ -> false
            do ()
    left, rest

and ParseShiftExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseAndExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseXorExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseOrExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseStarExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseComparison( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseNotTest( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseAndTest( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseOrTest( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseLambda( stream: SymbolStream, isCond: bool ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseTestNoCond( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseTest( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseNamedExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseTestListComp( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseTrailer( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseSubscriptList( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseSubscript( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseExprList( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseTestList( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseDictionaryOrSetMaker( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseCompIter( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseSyncCompFor( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseCompFor( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseCompIf( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseVarArgsList( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )

and ParseyieldExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) = ( Empty, [] )


// Parser: Statement rules ////////////////////////////////////////////////////////////////////////////////////////////
