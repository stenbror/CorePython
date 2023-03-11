module CorePython.PythonCoreParser

// Error handling system in parser ////////////////////////////////////////////////////////////////////////////////////
exception SyntaxError of uint * string


// Nodes generated during parsing of source code //////////////////////////////////////////////////////////////////////
type AbstractSyntaxNodes =
    |   Empty

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

    
    
type TokenStream = Symbol list

    
// Parser and lexer functions /////////////////////////////////////////////////////////////////////////////////////////

let IsLiteralOrDelimiterSymbol( values : (char * char * char), startPos: uint32, endPos: uint32 ) : ( Symbol * uint8 ) option =
    match values with
    |   ( '*', '*', '=' )   -> Some( Symbol.PyPowerAssign( startPos, endPos), 3uy )
    |   ( '*', '*', _ )     -> Some( Symbol.PyPower( startPos, endPos), 2uy )
    |   ( '*', '=', _ )     -> Some( Symbol.PyMulAssign( startPos, endPos), 2uy )
    |   ( '*', _ , _ )      -> Some( Symbol.PyMul( startPos, endPos), 1uy )
    |   ( '/', '/', '=' )   -> Some( Symbol.PyFloorDivAssign( startPos, endPos), 3uy )
    |   ( '/', '/', _ )     -> Some( Symbol.PyFloorDiv( startPos, endPos), 2uy )
    |   ( '/', '=', _ )     -> Some( Symbol.PyDivAssign( startPos, endPos), 2uy )
    |   ( '/', _ , _ )      -> Some( Symbol.PyDiv( startPos, endPos), 1uy )
    |   ( '<', '<', '=' )   -> Some( Symbol.PyShiftLeftAssign( startPos, endPos), 3uy )
    |   ( '<', '<', _ )     -> Some( Symbol.PyShiftLeft( startPos, endPos), 2uy )
    |   ( '<', '=', _ )     -> Some( Symbol.PyLessEqual( startPos, endPos), 2uy )
    |   ( '<', '>', _ )     -> Some( Symbol.PyNotEqual( startPos, endPos), 2uy )
    |   ( '<', _ , _ )      -> Some( Symbol.PyLess( startPos, endPos), 1uy )
    |   ( '>', '>', '=' )   -> Some( Symbol.PyShiftRightAssign( startPos, endPos), 3uy )
    |   ( '>', '>', _ )     -> Some( Symbol.PyShiftRight( startPos, endPos), 2uy )
    |   ( '>', '=', _ )     -> Some( Symbol.PyGreaterEqual( startPos, endPos), 2uy )
    |   ( '>', _ , _ )      -> Some( Symbol.PyGreater( startPos, endPos), 1uy )
    |   ( '.', '.', '.' )   -> Some( Symbol.PyEllipsis( startPos, endPos), 3uy )
    |   _                   -> None 

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