
// CorePython:  Parser for Python 3.11 language with tokenizer and needed structures
//              Copyright (C) 2023 By Richard Magnor Stenbro   stenbror@hotmail.com

module CorePython.Compiler.PythonCoreParser

open Microsoft.FSharp.Core

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
    |   False               of uint32 * uint32 * Symbol
    |   None                of uint32 * uint32 * Symbol 
    |   True                of uint32 * uint32 * Symbol
    |   Ellipsis            of uint32 * uint32 * Symbol
    |   Name                of uint32 * uint32 * Symbol
    |   Number              of uint32 * uint32 * Symbol
    |   String              of uint32 * uint32 * Symbol array
    |   AtomExpr            of uint32 * uint32 * Symbol option * AbstractSyntaxNodes * AbstractSyntaxNodes array option
    |   Power               of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   UnaryPlus           of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   UnaryMinus          of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   BitwiseInvert       of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   Mul                 of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Div                 of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   FloorDiv            of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Modulo              of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Matrices            of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Plus                of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Minus               of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   ShiftLeft           of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   ShiftRight          of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   BitwiseAnd          of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   BitwiseXor          of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   BitwiseOr           of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   StarExpr            of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   Less                of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   LessEqual           of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Equal               of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   GreaterEqual        of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Greater             of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   NotEqual            of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   In                  of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Is                  of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   NotIn               of uint32 * uint32 * AbstractSyntaxNodes * Symbol * Symbol * AbstractSyntaxNodes
    |   IsNot               of uint32 * uint32 * AbstractSyntaxNodes * Symbol * Symbol * AbstractSyntaxNodes
    |   NotTest             of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   AndTest             of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   OrTest              of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   Lambda              of uint32 * uint32 * Symbol * AbstractSyntaxNodes option * Symbol * AbstractSyntaxNodes
    |   Test                of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   NamedExpr           of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   YieldExpr           of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   YieldFrom           of uint32 * uint32 * Symbol * Symbol * AbstractSyntaxNodes
    |   TestListStarExpr    of uint32 * uint32 * AbstractSyntaxNodes array * Symbol array
    |   VarArgsList         of uint32 * uint32 * Symbol option * AbstractSyntaxNodes option * Symbol option * AbstractSyntaxNodes option * AbstractSyntaxNodes array * Symbol array
    |   VFPDefAssign        of uint32 * uint32 * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes
    |   TestList            of uint32 * uint32 * AbstractSyntaxNodes array * Symbol array
    |   ExprList            of uint32 * uint32 * AbstractSyntaxNodes array * Symbol array
    |   SubscriptList       of uint32 * uint32 * AbstractSyntaxNodes array * Symbol array
    |   Subscript           of uint32 * uint32 * AbstractSyntaxNodes option * Symbol option * AbstractSyntaxNodes option * Symbol option * AbstractSyntaxNodes option
    |   CompSyncFor         of uint32 * uint32 * Symbol * AbstractSyntaxNodes * Symbol * AbstractSyntaxNodes * AbstractSyntaxNodes
    |   CompFor             of uint32 * uint32 * Symbol * AbstractSyntaxNodes
    |   CompIf              of uint32 * uint32 * Symbol * AbstractSyntaxNodes * AbstractSyntaxNodes
    |   DictionaryContainer of uint32 * uint32 * AbstractSyntaxNodes array * Symbol array
    |   SetContainer        of uint32 * uint32 * AbstractSyntaxNodes array * Symbol array
    |   DotName             of uint32 * uint32 * Symbol * Symbol
    |   CallExpression      of uint32 * uint32 * Symbol * AbstractSyntaxNodes option * Symbol
    |   IndexExpression     of uint32 * uint32 * Symbol * AbstractSyntaxNodes * Symbol
    |   Tuple               of uint32 * uint32 * Symbol * AbstractSyntaxNodes option * Symbol
    |   List                of uint32 * uint32 * Symbol * AbstractSyntaxNodes option * Symbol
    |   Dictionary          of uint32 * uint32 * Symbol * AbstractSyntaxNodes option * Symbol
    |   Set                 of uint32 * uint32 * Symbol * AbstractSyntaxNodes option * Symbol
    |   ArgumentList        of uint32 * uint32 * AbstractSyntaxNodes array * Symbol array
    |   Argument            of uint32 * uint32 * AbstractSyntaxNodes option * Symbol option * AbstractSyntaxNodes
    
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
    |   Name( _ , e , _ ) | Number( _ , e , _ ) | String( _ , e , _ ) | NotTest( _ , e , _ , _  )
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
    |   Some( PyLeftParen( _ ), rest) ->
            let start_pos = GetStartPosition(stream)
            let op1 = List.head stream
            match TryToken rest with
            |   Some( PyRightParen( _, e ), rest2) ->
                    let op2 = List.head rest
                    Tuple( start_pos, e, op1, Option.None, op2), rest2
            |   _ ->
                    let node10, rest10 =    match TryToken rest with
                                            |   Some( PyYield( _ ), _ ) -> ParseYieldExpr rest
                                            |   _ ->  ParseTestListComp rest
                    match TryToken rest10 with
                    |   Some( PyRightParen( _ , e ), rest11) ->
                            let op2 = List.head rest10
                            Tuple( start_pos, e, op1, Some( node10 ), op2 ), rest11
                    |   _ ->   raise (SyntaxError(GetStartPosition rest10, "Expecting ')' in tuple!"))
    |   Some( PyLeftBracket( _ ), rest) ->
            let start_pos = GetStartPosition(stream)
            let op1 = List.head stream
            match TryToken rest with
            |   Some( PyRightBracket( _ , e ), rest2) ->
                    let op2 = List.head rest
                    List( start_pos, e, op1, Option.None, op2), rest2
            |   _ ->
                    let node20, rest12 = ParseTestListComp rest
                    match TryToken rest12 with
                    |   Some( PyRightBracket( _ , e ), rest13) ->
                            let op2 = List.head rest12
                            List( start_pos, e, op1, Some( node20 ), op2), rest13
                    |   _ ->  raise (SyntaxError(GetStartPosition rest12, "Expecting ']' in list!"))
    |   Some( PyLeftCurly( _ ), rest) ->
            let start_pos = GetStartPosition(stream)
            let op1 = List.head stream
            match TryToken rest with
            |   Some( PyRightCurly( _ , e ), rest2) ->
                    let op2 = List.head rest
                    Dictionary( start_pos, e, op1, Option.None, op2), rest2
            |   _ ->
                    let node30, rest15 = ParseDictionaryOrSetMaker rest
                    match node30 with
                    |   DictionaryContainer( _ ) ->
                            match TryToken rest15 with
                            |   Some( PyRightCurly( _ , e ), rest16) ->
                                    let op2 = List.head rest15
                                    Dictionary( start_pos, e, op1, Some( node30 ), op2), rest16
                            |   _ ->  raise (SyntaxError(GetStartPosition rest15, "Expecting '}' in dictionary!"))
                    |   SetContainer( _ ) ->
                            match TryToken rest15 with
                            |   Some( PyRightCurly( _ , e ), rest17) ->
                                    let op2 = List.head rest15
                                    Set( start_pos, e, op1, Some( node30 ), op2), rest17
                            |   _ ->  raise (SyntaxError(GetStartPosition rest15, "Expecting '}' in set!"))
                    |   _ ->
                            raise (SyntaxError(GetStartPosition rest, "Expecting dictionary or set!")) 
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

and ParseShiftExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos  = GetStartPosition( stream )
    let mutable left, rest = ParseArithExpr stream
    while   match TryToken rest with
            |  Some( PyShiftLeft( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseArithExpr rest2
                    left <- ShiftLeft( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  Some( PyShiftRight( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseArithExpr rest2
                    left <- ShiftRight( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  _ -> false
            do ()
    left, rest

and ParseAndExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition( stream )
    let mutable left, rest = ParseShiftExpr stream
    while   match TryToken rest with
            |  Some( PyBitwiseAnd( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseShiftExpr rest2
                    left <- BitwiseAnd( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  _ -> false
            do ()
    left, rest

and ParseXorExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition( stream )
    let mutable left, rest = ParseAndExpr stream
    while   match TryToken rest with
            |  Some( PyBitwiseXor( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseAndExpr rest2
                    left <- BitwiseXor( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  _ -> false
            do ()
    left, rest


and ParseOrExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition( stream )
    let mutable left, rest = ParseXorExpr stream
    while   match TryToken rest with
            |  Some( PyBitwiseOr( _ ), rest2 ) ->
                    let op = List.head rest
                    let right, rest3 = ParseXorExpr rest2
                    left <- BitwiseOr( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            |  _ -> false
            do ()
    left, rest

and ParseStarExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    match TryToken stream with
    |  Some( PyMul( _ ), rest ) ->
            let op = List.head stream
            let right, rest2 = ParseOrExpr rest
            StarExpr( GetStartPosition(stream), GetNodeEndPosition( right ), op, right ), rest2
    |  _ -> raise (SyntaxError( GetStartPosition( stream ), "Expecting '*' in star expression!" ))

and ParseComparison( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition(stream)
    let mutable left, rest = ParseOrExpr stream
    while   match TryToken rest with
            |  Some( PyLess( _ ), rest2 ) ->
                        let op = List.head rest
                        let right, rest3 = ParseOrExpr rest2
                        left <- Less( start_pos, GetNodeEndPosition( right ), left, op, right )
                        rest <- rest3
                        true
            |  Some( PyLessEqual( _ ), rest2 ) ->
                        let op = List.head rest
                        let right, rest3 = ParseOrExpr rest2
                        left <- LessEqual( start_pos, GetNodeEndPosition( right ), left, op, right )
                        rest <- rest3
                        true
            |  Some( PyEqual( _ ), rest2 ) ->
                        let op = List.head rest
                        let right, rest3 = ParseOrExpr rest2
                        left <- Equal( start_pos, GetNodeEndPosition( right ), left, op, right )
                        rest <- rest3
                        true
            |  Some( PyGreaterEqual( _ ), rest2 ) ->
                        let op = List.head rest
                        let right, rest3 = ParseOrExpr rest2
                        left <- GreaterEqual( start_pos, GetNodeEndPosition( right ), left, op, right )
                        rest <- rest3
                        true
            |  Some( PyGreater( _ ), rest2 ) ->
                        let op = List.head rest
                        let right, rest3 = ParseOrExpr rest2
                        left <- Greater( start_pos, GetNodeEndPosition( right ), left, op, right )
                        rest <- rest3
                        true
            |  Some( PyNotEqual( _ ), rest2 ) ->
                        let op = List.head rest
                        let right, rest3 = ParseOrExpr rest2
                        left <- NotEqual( start_pos, GetNodeEndPosition( right ), left, op, right )
                        rest <- rest3
                        true
            |  Some( PyIn( _ ), rest2 ) ->
                        let op = List.head rest
                        let right, rest3 = ParseOrExpr rest2
                        left <- In( start_pos, GetNodeEndPosition( right ), left, op, right )
                        rest <- rest3
                        true
            |  Some( PyNot( _ ), rest2 ) ->
                        let op = List.head rest
                        match TryToken rest2 with
                        |  Some( PyIn( _ ), rest3) ->
                                let op2 = List.head rest2
                                let right, rest4 = ParseOrExpr rest3
                                left <- NotIn( start_pos, GetNodeEndPosition( right ), left, op, op2, right )
                                rest <- rest4
                        |  _ ->
                                raise ( SyntaxError(GetStartPosition(rest2), "Missing 'in' in 'not in' expression!" ))
                        true
            |  Some( PyIs( _ ), rest2 ) ->
                        let op = List.head rest
                        match TryToken rest2 with 
                        |  Some( PyNot( _ ), rest3) ->
                                let op2 = List.head rest2
                                let right, rest4 = ParseOrExpr rest3
                                left <- IsNot( start_pos, GetNodeEndPosition( right ), left, op, op2, right  )
                                rest <- rest4
                        |  _ ->
                               let right, rest3 = ParseOrExpr rest2
                               left <- Is( start_pos, GetNodeEndPosition( right ), left, op, right )
                               rest <- rest3 
                        true
            |  _ -> false
        do ()
    left, rest

and ParseNotTest( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    match TryToken stream with
    |  Some( PyNot( _ ), rest) ->
            let op = List.head stream
            let right, rest2 = ParseNotTest rest
            NotTest( start_pos, GetNodeEndPosition( right ), op, right ), rest2
    |  _ -> ParseComparison stream

and ParseAndTest( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable left, rest = ParseNotTest stream
    while   match TryToken rest with
            |  Some( PyAnd( _ ), rest2) ->
                    let op = List.head rest
                    let right, rest3 = ParseNotTest rest2
                    left <- AndTest( start_pos, GetNodeEndPosition( right ), left, op, right )
                    rest <- rest3
                    true
            | _ -> false
            do ()
    left, rest

and ParseOrTest( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable left, rest = ParseAndTest stream
    while   match TryToken rest with
            |  Some( PyOr( _ ), rest2) ->
                    let op = List.head rest
                    let right, rest3 = ParseAndTest rest2
                    left <- OrTest( start_pos, GetNodeEndPosition right, left, op, right )
                    rest <- rest3
                    true
            | _ -> false
            do ()
    left, rest

and ParseLambda( stream: SymbolStream, isCond: bool ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    match TryToken stream with
    |  Some( PyLambda( _ ), rest) ->
         let op = List.head stream
         let left, rest2 =  match TryToken rest with
                            |  Some( PyColon( _ ), _ ) -> Option.None, rest
                            |  _ ->
                                let a, b = ParseVarArgsList rest
                                Some( a ), b
         match TryToken rest2 with
         |  Some( PyColon( _ ), rest4) ->
              let op2 = List.head rest2
              let right, rest3 =  match isCond with | true -> ParseTest rest4 | false -> ParseTestNoCond rest4
              Lambda( start_pos, GetNodeEndPosition right, op, left, op2, right), rest3
         |  _ ->    raise(SyntaxError(GetStartPosition rest2, "Expecting ':' in 'lambda' expression!"))
    |  _ ->  raise (SyntaxError(GetStartPosition stream, "Expecting 'lambda' expression!"))

and ParseTestNoCond( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    match TryToken stream with | Some( PyLambda( _ ), _ ) -> ParseLambda( stream, false ) |  _ -> ParseOrTest stream

and ParseTest( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    match TryToken stream with
    |  Some( PyLambda( _ ), _ ) -> ParseLambda( stream, true )
    | _ ->
       let left, rest = ParseOrTest stream
       match TryToken rest with
       |  Some( PyIf( _ ), rest2) ->
               let op1 = List.head rest
               let right, rest3 = ParseOrTest rest2
               match TryToken rest3 with
               |  Some( PyElse( _ ), rest4 ) ->
                    let op2 = List.head rest3
                    let next, rest5 = ParseTest rest4
                    Test( start_pos, GetNodeEndPosition next, left, op1, right, op2, next ), rest5
               | _ ->  raise(SyntaxError(GetStartPosition rest3, "Expecting 'else' in test expression!"))
       |  _ ->
            left, rest

and ParseNamedExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let left, rest = ParseTest stream
    match TryToken rest with
    |  Some( PyColonAssign( _ ), rest2 ) ->
         let op = List.head rest
         let right, rest3 = ParseTest rest2
         NamedExpr( start_pos, GetNodeEndPosition right, left, op, right) , rest3
    | _ ->
         left, rest

and ParseTestListComp( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable end_pos = start_pos
    let mutable nodes : AbstractSyntaxNodes List = List.Empty
    let mutable separators : Symbol List = List.Empty
    let node, rest = match TryToken stream with
                     |   Some( PyMul( _ ), _ ) -> ParseStarExpr stream
                     |   _ ->   ParseNamedExpr stream
    nodes <- node :: nodes
    end_pos <- GetNodeEndPosition node
    let mutable rest2 = rest
    match TryToken rest2 with
    |   Some( PyFor( _ ), _ )
    |   Some( PyAsync( _ ), _ ) ->
            let node2, rest3 = ParseCompFor rest2
            nodes <- node2 :: nodes
            rest2 <- rest3
            end_pos <- GetNodeEndPosition node2
    |   _ ->
            while   match TryToken rest2 with
                    |  Some( PyComma( _ , e ), rest4 ) ->
                          separators <- List.head rest :: separators
                          end_pos <- e
                          match TryToken rest4 with
                          |  Some( PyRightParen( _ ), _ )
                          |  Some( PyRightBracket( _ ), _ ) ->
                                rest2 <- rest4
                                false
                          |  Some( PyComma( _ , e ), _ ) ->
                                raise ( SyntaxError( e, "Unexpected ',' in list!") )
                          |  _ ->
                                let node2, rest5 =
                                       match TryToken rest4 with
                                       |   Some( PyMul( _ ), _ ) -> ParseStarExpr stream
                                       |   _ ->   ParseNamedExpr stream
                                rest2 <- rest5
                                nodes <- node2 :: nodes
                                end_pos <- GetNodeEndPosition node2
                                true
                    |   _ -> false
               do ()
               
    TestList( start_pos, end_pos, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest2

and ParseTrailer( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let symbol1 = List.head stream
    match TryToken stream with
    |   Some( PyDot( _ ), rest ) ->
            match TryToken rest with
            |   Some( PyName( _ , e , _ ), rest2 ) ->
                    let name = List.head rest
                    DotName( start_pos, e, symbol1, name), rest2
            |   _ ->   raise ( SyntaxError(GetStartPosition rest, "Expecting Name literal after '.'!") )
    |   Some( PyLeftParen( _ ), rest ) ->
            let node, rest2 =
                    match TryToken rest with
                    |   Some( PyRightParen( _ ), _ ) -> Option.None, rest
                    |   _ ->
                            let a, b = ParseArgList rest
                            Some( a ), b
            match TryToken rest2 with
            |    Some( PyRightParen( _ , e ), rest3) ->
                    let symbol2 = List.head rest2
                    CallExpression( start_pos, e, symbol1, node, symbol2), rest3
            |    _ ->   raise( SyntaxError(GetStartPosition rest2, "Expecting ')' in call!") )
    |   Some( PyLeftBracket( _ ), rest ) ->
            let node, rest2 = ParseSubscriptList rest
            match TryToken rest2 with
            |    Some( PyRightBracket( _, e ), rest3) ->
                    let symbol2 = List.head rest2
                    IndexExpression( start_pos, e, symbol1, node, symbol2), rest3
            |    _ ->   raise( SyntaxError(GetStartPosition rest2, "Expecting ']' in indexer!") )
    |   _ ->   Empty, stream

and ParseSubscriptList( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable end_pos = start_pos
    let mutable nodes : AbstractSyntaxNodes List = List.Empty
    let mutable separators : Symbol List = List.Empty
    let mutable node, rest = ParseSubscript stream
    nodes <- node :: nodes
    end_pos <- GetNodeEndPosition node
    while   match TryToken rest with
            |  Some( PyComma( _ , e ), rest2 ) ->
                 separators <- List.head rest :: separators
                 end_pos <- e
                 match TryToken rest2 with
                 |  Some( PyRightBracket( _ ), _ ) ->
                       rest <- rest2
                       false
                 |  _ ->
                       let node2, rest3 = ParseSubscript rest2
                       nodes <- node2 :: nodes
                       rest <- rest3
                       end_pos <- GetNodeEndPosition node2
                       true
            |  _ -> false
       do ()
    
    SubscriptList( start_pos, end_pos, List.toArray(List.rev nodes), List.toArray(List.rev separators) ), rest

and ParseSubscript( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable end_pos = start_pos
    let mutable first : AbstractSyntaxNodes option = Option.None
    let mutable op1 : Symbol option = Option.None
    let mutable second : AbstractSyntaxNodes option = Option.None
    let mutable op2 : Symbol option = option.None
    let mutable third : AbstractSyntaxNodes option = option.None
    let mutable rest = stream
    match TryToken rest with
    |   Some( PyColon( _ , e ), _ ) ->  ()
    |   _ ->
            let node1, rest1 = ParseTest rest
            first <- Some( node1 )
            rest <- rest1
            end_pos <- GetNodeEndPosition node1
    match TryToken rest with
    |  Some( PyColon( _ , e ), rest2 ) ->
            op1 <- Some( List.head rest )
            rest <- rest2
            end_pos <- e
            match TryToken rest with 
            |   Some( PyColon( _ ), _ )
            |   Some( PyComma( _ ), _ )
            |   Some( PyRightBracket( _ ), _ ) -> ()
            |  _ ->
                let node2, rest3 = ParseTest rest
                second <- Some( node2 )
                rest <- rest3
                end_pos <- GetNodeEndPosition node2
            match TryToken rest with
            |   Some( PyColon( _ , e ), rest4 ) ->
                    op2 <- Some( List.head rest )
                    rest <- rest4
                    end_pos <- e
                    match TryToken rest with
                    |   Some( PyComma( _ ), _ )
                    |   Some( PyRightBracket( _ ), _ ) ->
                            ()
                    |   _ ->
                            let node3, rest5 = ParseTest rest
                            third <- Some( node3 )
                            rest <- rest5
                            end_pos <- GetNodeEndPosition node3
            |   _ ->  ()
    |  _ -> ()
    
    Subscript( start_pos, end_pos, first, op1, second, op2, third ), rest

and ParseTestList( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable end_pos = start_pos
    let mutable nodes : AbstractSyntaxNodes List = List.Empty
    let mutable separators: Symbol List = List.Empty
    let mutable node, rest = ParseTest stream
    nodes <- node :: nodes
    end_pos <- GetNodeEndPosition node
    while match TryToken rest with
          |  Some( PyComma( _ , e ), rest2 ) ->
               separators <- List.head rest :: separators
               end_pos <- e
               match TryToken rest2 with
               |  Some( PySemicolon( _ ), _ )
               |  Some( Newline( _ ), _ )
               |  Some( EOF( _ ), _ ) ->
                    rest <- rest2
                    false
               |  _ ->
                    let node2, rest3 = ParseTest rest2
                    rest <- rest3
                    nodes <- node2 :: nodes
                    end_pos <- GetNodeEndPosition node2
                    true
          |  _ -> false
       do ()
    
    TestList( start_pos, end_pos, List.toArray(List.rev nodes), List.toArray(List.rev separators) ), rest

and ParseExprList( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable end_pos = start_pos
    let mutable nodes : AbstractSyntaxNodes List = List.Empty
    let mutable separarors: Symbol List = List.Empty
    let mutable node, rest = match TryToken stream with
                             | Some( PyMul( _ ), _ ) -> ParseStarExpr stream
                             | _ -> ParseOrTest stream
    nodes <- node :: nodes
    end_pos <- GetNodeEndPosition node
    while match TryToken rest with
          |  Some( PyComma( _ , e ), rest2 ) ->
               separarors <- List.head rest :: separarors
               end_pos <- e
               match TryToken rest2 with
               |  Some( PyIn( _ ), _ ) ->
                    rest <- rest2
                    false
               |  _ ->
                    let node2, rest3 = match TryToken rest2 with
                                       | Some( PyMul( _ ), _ ) -> ParseStarExpr rest2
                                       | _ -> ParseOrTest rest2
                    nodes <- node2 :: nodes
                    rest <- rest3
                    end_pos <- GetNodeEndPosition node2
                    true
          |  _ -> false
       do ()
    
    ExprList( start_pos, end_pos, List.toArray(List.rev nodes), List.toArray(List.rev separarors) ), rest

and ParseDictionaryOrSetMaker( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable end_pos = start_pos
    let mutable key : AbstractSyntaxNodes = Empty
    let mutable symbol : Symbol = Symbol.Empty
    let mutable value : AbstractSyntaxNodes = Empty
    let mutable isDictionary = true
    let mutable rest = stream
    let mutable nodes : AbstractSyntaxNodes List = List.Empty
    let mutable separators : Symbol List = List.Empty
    
    // Insert code for grammar handling here!
    
    match isDictionary with
        |    true ->    DictionaryContainer( start_pos, end_pos, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest
        |    _ ->       SetContainer( start_pos, end_pos, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest

and ParseCompIter( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    match TryToken stream with
        |  Some( PyFor( _ ), _ )
        |  Some( PyAsync( _ ), _ )  ->  ParseCompFor stream
        |  Some( PyIf( _ ), _ )     ->  ParseCompIf stream
        |  _                        ->  Empty, stream

and ParseSyncCompFor( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    match TryToken stream with
    |  Some( PyFor( _ ), rest ) ->
          let op1 = List.head stream
          let left, rest2 = ParseExprList rest
          match TryToken rest2 with
          |  Some( PyIn( _ ), rest3 ) ->
                let op2 = List.head rest2
                let right, rest4 = ParseOrTest rest3
                let next, rest5 = ParseCompIter rest4
                match next with
                |   Empty   ->  CompSyncFor( start_pos, GetNodeEndPosition right, op1, left, op2, right, next), rest5
                |   _       ->  CompSyncFor( start_pos, GetNodeEndPosition next, op1, left, op2, right, next), rest5
                
          |  _ ->  raise (SyntaxError(GetStartPosition rest2, "Expecting 'in' in 'for' comprehension expression!"))
    |  _ -> raise (SyntaxError(GetStartPosition stream, "Expecting 'for' in 'for' comprehension expression!"))

and ParseCompFor( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    match TryToken stream with
    |  Some( PyAsync( _ ), rest ) ->
         let op = List.head stream
         let right, rest2 = ParseSyncCompFor rest
         CompFor( start_pos, GetNodeEndPosition right, op, right), rest2
         
    |  _ -> ParseSyncCompFor stream

and ParseCompIf( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    match TryToken stream with
    |   Some( PyIf( _ ), rest ) ->
            let op = List.head stream
            let right, rest2 = ParseTestNoCond rest
            let next, rest3 = ParseCompIter rest2
            match next with
            |   Empty   ->  CompIf( start_pos, GetNodeEndPosition right, op, right, next), rest3
            |   _       ->  CompIf( start_pos, GetNodeEndPosition next, op, right, next), rest3
            
    |   _ ->   raise (SyntaxError(GetStartPosition stream, "Expecting 'if' in comprehension expression!"))

and ParseVarArgsList( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable end_pos = start_pos
    let mutable mulOp : Symbol option = Option.None
    let mutable powerOp : Symbol option = Option.None
    let mutable mulNode : AbstractSyntaxNodes option = Option.None
    let mutable powerNode : AbstractSyntaxNodes option = Option.None
    let mutable nodes : AbstractSyntaxNodes List = List.Empty
    let mutable separators : Symbol List = List.Empty
    let mutable rest = stream
    
    // Insert code for grammar here!
    
    VarArgsList( start_pos, end_pos, mulOp, mulNode, powerOp, powerNode,
                            List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest

and ParseVFPDef( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    match TryToken stream with
    |   Some( PyName( _ ), rest ) ->
            let name = List.head stream
            match name with
            |   PyName( _, e , _ ) -> Name( start_pos, e, name ) , rest
            |   _ ->  Empty, stream // Never happens!
    |   _ ->   raise (SyntaxError(GetStartPosition stream, "Expecting Name literal in list!"))

and ParseYieldExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    match TryToken stream with
    |  Some( PyYield( _ ), rest) ->
          let op1 = List.head stream
          match TryToken rest with
          |  Some( PyFrom( _ ), rest2) ->
                let op2 = List.head rest
                let right, rest3 = ParseTest rest2
                YieldFrom( start_pos, GetNodeEndPosition right, op1, op2, right ), rest3
          |  _ ->
                let right, rest4 = ParseTestListStarExpr rest
                YieldExpr( start_pos, GetNodeEndPosition right, op1, right ), rest4
    |  _ -> raise (SyntaxError(GetStartPosition stream, "Expecting 'yield' expression!"))
    
and ParseTestListStarExpr( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable end_pos = start_pos
    let mutable nodes : AbstractSyntaxNodes list = List.Empty
    let mutable separators : Symbol list = List.Empty
    let mutable node, rest = match TryToken stream with
                             |  Some( PyMul( _ ), _ )  -> ParseStarExpr stream
                             | _ -> ParseTest stream
    nodes <- node :: nodes
    end_pos <- GetNodeEndPosition node
    while   match TryToken rest with
            |  Some( PyComma( _ ), rest2 ) ->
                    separators <- List.head rest :: separators
                    match TryToken rest2 with   
                    |  Some( PyPlusAssign( _ ), _ )
                    |  Some( PyMinusAssign( _ ), _ )
                    |  Some( PyMulAssign( _ ), _ )
                    |  Some( PyPowerAssign( _ ), _ )
                    |  Some( PyMatriceAssign( _ ), _ )
                    |  Some( PyModuloAssign( _ ), _ )
                    |  Some( PyBitwiseAndAssign( _ ), _ )
                    |  Some( PyBitwiseOrAssign( _ ), _ )
                    |  Some( PyBitwiseXorAssign( _ ), _ )
                    |  Some( PyShiftLeftAssign( _ ), _ )
                    |  Some( PyShiftRightAssign( _ ), _ )
                    |  Some( PyFloorDivAssign( _ ), _ )
                    |  Some( PyDivAssign( _ ), _ )
                    |  Some( PyAssign( _ ), _ )
                    |  Some( PyColon( _ ), _ )
                    |  Some( PySemicolon( _ ), _ )
                    |  Some( Newline( _ ), _ )
                    |  Some( EOF( _ ), _ ) ->
                            rest <- rest2
                    |  _  ->
                         let node2, rest3 =  match TryToken rest2 with
                                             |  Some( PyMul( _ ), _ )  -> ParseStarExpr rest2
                                             | _ -> ParseTest rest2
                         rest <- rest3
                         nodes <- node2 :: nodes
                         end_pos <- GetNodeEndPosition node2
                    true
            |  _ ->  false
        do ()
    
    TestListStarExpr( start_pos, end_pos, List.toArray(List.rev nodes), List.toArray(List.rev separators) ), rest
    
and ParseArgList( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    let mutable end_pos = start_pos
    let mutable nodes : AbstractSyntaxNodes List = List.Empty
    let mutable separators : Symbol List = List.Empty
    let mutable node, rest = ParseArgument stream
    nodes <- node :: nodes
    end_pos <- GetNodeEndPosition node
    while   match TryToken rest with
            |  Some( PyComma( _ ), rest2 ) ->
                 separators <- List.head rest :: separators
                 match TryToken rest2 with
                 |  Some( PyRightParen( _, e ), _ ) ->
                       rest <- rest2
                       end_pos <- e
                       false
                 |  _ ->
                       let node2, rest3 = ParseArgument rest2
                       nodes <- node2 :: nodes
                       rest <- rest3
                       end_pos <- GetNodeEndPosition node2
                       true
            |  _ -> false
       do ()
    
    ArgumentList( start_pos, end_pos, List.toArray(List.rev nodes), List.toArray(List.rev separators)), rest
    
and ParseArgument( stream: SymbolStream ) : ( AbstractSyntaxNodes * SymbolStream ) =
    let start_pos = GetStartPosition stream
    match TryToken stream with
    |  Some( PyMul( _ ), rest )
    |  Some( PyPower( _ ), rest ) ->
          let op = List.head stream
          let right, rest2 = ParseTest rest
          Argument( start_pos, GetNodeEndPosition right, Option.None, Some( op ), right), rest2
    |  _ ->
         let left, rest3 = ParseTest stream
         match TryToken rest3 with
         |    Some( PyAsync( _ ), _ )
         |    Some( PyFor( _ ), _ ) ->
                 let right, rest4 = ParseCompFor rest3
                 Argument( start_pos, GetNodeEndPosition right, Some( left ), Option.None, right), rest4
         |    Some( PyColonAssign( _ ), rest5)
         |    Some( PyAssign( _ ), rest5) ->
                 let op = List.head rest3
                 let right, rest6 = ParseTest rest5
                 Argument( start_pos, GetNodeEndPosition right, Some( left ), Some( op ), right), rest6
         |  _ ->
              left, rest3

// Parser: Statement rules ////////////////////////////////////////////////////////////////////////////////////////////
