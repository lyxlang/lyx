// SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
//
// SPDX-License-Identifier: GPL-3.0-only

%token KWDEF
%token KWLET
%token KWIN
%token KWIF
%token KWTHEN
%token KWELSE
%token KWINT
%token KWFLOAT
%token KWBOOL
%token KWSTRING
%token KWUNIT
%token KWMATCH
%token KWAS

%token EQUAL
%token COLONEQUAL
%token COLON
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token ARROW
%token SEMICOLON
%token UNIT
%token BACKSLASH
%token ELLIPSIS

%token TRIANGLE
%token BARBAR
%token ANDAND
%token EQUALEQUAL
%token BANGEQUAL
%token GT
%token GEQ
%token LT
%token LEQ
%token PLUSPLUS
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token STARSTAR
%token BANG

%token <string> COMMENT
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> STRING
%token <Ast.uid> UID
%token <Ast.lid> LID

%token EOF

%start <Ast.program> program

%{ open Ast %}

%%

let program := ~ = list(declaration); EOF; < >

let declaration :=
  | c = COMMENT; { DComment ({start= $startofs; fin= $endofs}, c) }
  | KWDEF; b = binding; { DValueBinding ({start= $startofs; fin= $endofs}, b) }
  | KWDEF; id = UID; COLONEQUAL; body = typing; { DTypeDefinition ({start= $startofs; fin= $endofs}, {id; body}) }
  | KWDEF; id = LID; parameters = nonempty_list(parameter); signature = signature; EQUAL; body = expression; { DFunctionDefinition ({start= $startofs; fin= $endofs}, {id; parameters; signature; body}) }
  | KWDEF; id = UID; COLONEQUAL; polymorphics = list(LID); LBRACE; variants = nonempty_list(variant); RBRACE; { DADTDefinition ({start= $startofs; fin= $endofs}, {id; polymorphics; variants}) }

let binding == id = LID; signature = signature; EQUAL; body = expression; { {span= {start= $startofs; fin= $endofs}; id; signature; body} }

let signature == option(preceded(COLON, typing))

let parameter :=
  | id = LID; { ALID ({start= $startofs; fin= $endofs}, id) }
  | LPAREN; p = parameter; COMMA; ps = separated_list(COMMA, parameter); RPAREN; { ATuple ({start= $startofs; fin= $endofs}, p :: ps) }

let typing_atom :=
  | KWINT; { TInt {start= $startofs; fin= $endofs} }
  | KWFLOAT; { TFloat {start= $startofs; fin= $endofs} }
  | KWBOOL; { TBool {start= $startofs; fin= $endofs} }
  | KWSTRING; { TString {start= $startofs; fin= $endofs} }
  | KWUNIT; { TUnit {start= $startofs; fin= $endofs} }
  | id = UID; typing = option(typing_atom); { TConstructor ({start= $startofs; fin= $endofs}, {id; typing}) }
  | id = LID; { TPolymorphic ({start= $startofs; fin= $endofs}, id) }
  | LPAREN; t = typing; COMMA; ts = separated_list(COMMA, typing); RPAREN; { TTuple ({start= $startofs; fin= $endofs}, t :: ts) }
  | LBRACKET; t = typing; RBRACKET; { TList ({start= $startofs; fin= $endofs}, t) }

let typing :=
  | typing_atom
  | l = typing; ARROW; r = typing_atom; { TFunction ({start= $startofs; fin= $endofs}, {l; r}) }

let expression_atom :=
  | i = INT; { EInt ({start= $startofs; fin= $endofs}, i) }
  | f = FLOAT; { EFloat ({start= $startofs; fin= $endofs}, f) }
  | b = BOOL; { EBool ({start= $startofs; fin= $endofs}, b) }
  | s = STRING; { EString ({start= $startofs; fin= $endofs}, s) }
  | UNIT; { EUnit {start= $startofs; fin= $endofs} }
  | id = LID; { ELID ({start= $startofs; fin= $endofs}, id) }
  | LPAREN; e = expression; COMMA; es = separated_list(COMMA, expression); RPAREN; { ETuple ({start= $startofs; fin= $endofs}, e :: es) }
  | LBRACKET; l = separated_list(COMMA, expression); RBRACKET; { EList ({start= $startofs; fin= $endofs}, l) }
  | LPAREN; body = expression; signature = signature; RPAREN; { EExpression ({start= $startofs; fin= $endofs}, {body; signature}) }

let application :=
  | expression_atom
  | body = application; argument = expression_atom; { EApplication ({start= $startofs; fin= $endofs}, {body; argument}) }

let expression :=
  | KWLET; bindings = separated_nonempty_list(SEMICOLON, binding); KWIN; body = expression; { ELet ({start= $startofs; fin= $endofs}, {bindings; body}) }
  | KWIF; predicate = expression; KWTHEN; truthy = expression; KWELSE; falsy = expression; { EIf ({start= $startofs; fin= $endofs}, {predicate; truthy; falsy}) }
  | KWMATCH; body = expression; LBRACE; cases = nonempty_list(case); RBRACE; { EMatch ({start= $startofs; fin= $endofs}, {body; cases}) }
  | BACKSLASH; parameters = nonempty_list(parameter); ARROW; body = expression; { ELambda ({start= $startofs; fin= $endofs}, {parameters; body}) }
  | id = UID; body = option(expression_atom); { EConstructor ({start= $startofs; fin= $endofs}, {id; body}) }
  | expression_pipe

let expression_pipe :=
  | expression_or
  | l = expression_pipe; TRIANGLE; r = expression_or; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BPipe; r}) }

let expression_or :=
  | expression_and
  | l = expression_or; BARBAR; r = expression_and; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BOr; r}) }

let expression_and :=
  | expression_equality
  | l = expression_and; ANDAND; r = expression_equality; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BAnd; r}) }

let expression_equality :=
  | expression_compare
  | l = expression_equality; EQUALEQUAL; r = expression_compare; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BEqual; r}) }
  | l = expression_equality; BANGEQUAL; r = expression_compare; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BNotEqual; r}) }

let expression_compare :=
  | expression_concatenate
  | l = expression_compare; GT; r = expression_concatenate; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BGreaterThan; r}) }
  | l = expression_compare; GEQ; r = expression_concatenate; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BGreaterOrEqual; r}) }
  | l = expression_compare; LT; r = expression_concatenate; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BLessThan; r}) }
  | l = expression_compare; LEQ; r = expression_concatenate; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BLessOrEqual; r}) }

let expression_concatenate :=
  | expression_addition
  | l = expression_concatenate; PLUSPLUS; r = expression_addition; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BConcatenate; r}) }

let expression_addition :=
  | expression_multiplication
  | l = expression_addition; PLUS; r = expression_multiplication; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BAdd; r}) }
  | l = expression_addition; MINUS; r = expression_multiplication; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BSubstract; r}) }

let expression_multiplication :=
  | expression_exponentiation
  | l = expression_multiplication; STAR; r = expression_exponentiation; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BMultiply; r}) }
  | l = expression_multiplication; SLASH; r = expression_exponentiation; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BDivide; r}) }
  | l = expression_multiplication; PERCENT; r = expression_exponentiation; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BModulo; r}) }

let expression_exponentiation :=
  | expression_unary
  | l = expression_unary; STARSTAR; r = expression_exponentiation; { EBinaryOperation ({start= $startofs; fin= $endofs}, {l; operator= BExponentiate; r}) }

let expression_unary :=
  | application
  | PLUS; body = expression_unary; { EUnaryOperation ({start= $startofs; fin= $endofs}, {operator= UPlus; body}) }
  | MINUS; body = expression_unary; { EUnaryOperation ({start= $startofs; fin= $endofs}, {operator= UMinus; body}) }
  | BANG; body = expression_unary; { EUnaryOperation ({start= $startofs; fin= $endofs}, {operator= UNot; body}) }

let variant == id = UID; typing = option(preceded(KWAS, typing)); SEMICOLON; { {span= {start= $startofs; fin= $endofs}; id; typing} } 

let case == pattern = pattern; guard = option(preceded(KWIF, expression)); ARROW; body = expression; SEMICOLON; { {span= {start= $startofs; fin= $endofs}; pattern; guard; body} }

let pattern_atom :=
  | i = INT; { PInt ({start= $startofs; fin= $endofs}, i) }
  | f = FLOAT; { PFloat ({start= $startofs; fin= $endofs}, f) }
  | b = BOOL; { PBool ({start= $startofs; fin= $endofs}, b) }
  | s = STRING; { PString ({start= $startofs; fin= $endofs}, s) }
  | id = LID; { PLID ({start= $startofs; fin= $endofs}, id) }
  | LPAREN; p = pattern_atom; COMMA; ps = separated_list(COMMA, pattern_atom); RPAREN; { PTuple ({start= $startofs; fin= $endofs}, p :: ps) }
  | LBRACKET; l = separated_list(COMMA, pattern_atom); RBRACKET; { PList ({start= $startofs; fin= $endofs}, l) }
  | LBRACKET; ps = separated_nonempty_list(COMMA, pattern_atom); ELLIPSIS; p = LID; RBRACKET; { PListSpread ({start= $startofs; fin= $endofs}, ps @ [PLID ({start= $startofs; fin= $endofs}, p)]) }
  | id = UID; pattern = option(pattern_atom); { PConstructor ({start= $startofs; fin= $endofs}, {id; pattern}) }

let pattern :=
  | pattern_atom
  | l = pattern; SEMICOLON; r = pattern_atom; { POr ({start= $startofs; fin= $endofs}, {l; r}) }
