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
  | c = COMMENT; { DComment ($loc, c) }
  | KWDEF; b = binding; { DValueBinding ($loc, b) }
  | KWDEF; id = UID; COLONEQUAL; body = typing; { DTypeDefinition ($loc, {id; body}) }
  | KWDEF; id = LID; parameters = nonempty_list(parameter); signature = signature; EQUAL; body = expression; { DFunctionDefinition ($loc, {id; parameters; signature; body}) }
  | KWDEF; id = UID; COLONEQUAL; polymorphics = list(LID); LBRACE; variants = nonempty_list(variant); RBRACE; { DADTDefinition ($loc, {id; polymorphics; variants}) }

let binding == id = LID; signature = signature; EQUAL; body = expression; { {span= $loc; id; signature; body} }

let signature == option(preceded(COLON, typing))

let parameter :=
  | id = LID; { ALID ($loc, id) }
  | LPAREN; p = parameter; COMMA; ps = separated_list(COMMA, parameter); RPAREN; { ATuple ($loc, p :: ps) }

let typing_atom :=
  | KWINT; { TInt $loc }
  | KWFLOAT; { TFloat $loc }
  | KWBOOL; { TBool $loc }
  | KWSTRING; { TString $loc }
  | KWUNIT; { TUnit $loc }
  | id = UID; typing = option(typing_atom); { TConstructor ($loc, {id; typing}) }
  | id = LID; { TPolymorphic ($loc, id) }
  | LPAREN; t = typing; COMMA; ts = separated_list(COMMA, typing); RPAREN; { TTuple ($loc, t :: ts) }
  | LBRACKET; t = typing; RBRACKET; { TList ($loc, t) }

let typing :=
  | typing_atom
  | l = typing; ARROW; r = typing_atom; { TFunction ($loc, {l; r}) }

let expression_atom :=
  | i = INT; { EInt ($loc, i) }
  | f = FLOAT; { EFloat ($loc, f) }
  | b = BOOL; { EBool ($loc, b) }
  | s = STRING; { EString ($loc, s) }
  | UNIT; { EUnit $loc }
  | id = LID; { ELID ($loc, id) }
  | LPAREN; e = expression; COMMA; es = separated_list(COMMA, expression); RPAREN; { ETuple ($loc, e :: es) }
  | LBRACKET; l = separated_list(COMMA, expression); RBRACKET; { EList ($loc, l) }
  | LPAREN; body = expression; signature = signature; RPAREN; { EExpression ($loc, {body; signature}) }

let application :=
  | expression_atom
  | body = application; argument = expression_atom; { EApplication ($loc, {body; argument}) }

let expression :=
  | KWLET; bindings = separated_nonempty_list(SEMICOLON, binding); KWIN; body = expression; { ELet ($loc, {bindings; body}) }
  | KWIF; predicate = expression; KWTHEN; truthy = expression; KWELSE; falsy = expression; { EIf ($loc, {predicate; truthy; falsy}) }
  | KWMATCH; body = expression; LBRACE; cases = nonempty_list(case); RBRACE; { EMatch ($loc, {body; cases}) }
  | BACKSLASH; parameters = nonempty_list(parameter); ARROW; body = expression; { ELambda ($loc, {parameters; body}) }
  | id = UID; body = option(expression_atom); { EConstructor ($loc, {id; body}) }
  | expression_pipe

let expression_pipe :=
  | expression_or
  | l = expression_pipe; TRIANGLE; r = expression_or; { EBinaryOperation ($loc, {l; operator= BPipe; r}) }

let expression_or :=
  | expression_and
  | l = expression_or; BARBAR; r = expression_and; { EBinaryOperation ($loc, {l; operator= BOr; r}) }

let expression_and :=
  | expression_equality
  | l = expression_and; ANDAND; r = expression_equality; { EBinaryOperation ($loc, {l; operator= BAnd; r}) }

let expression_equality :=
  | expression_compare
  | l = expression_equality; EQUALEQUAL; r = expression_compare; { EBinaryOperation ($loc, {l; operator= BEqual; r}) }
  | l = expression_equality; BANGEQUAL; r = expression_compare; { EBinaryOperation ($loc, {l; operator= BNotEqual; r}) }

let expression_compare :=
  | expression_concatenate
  | l = expression_compare; GT; r = expression_concatenate; { EBinaryOperation ($loc, {l; operator= BGreaterThan; r}) }
  | l = expression_compare; GEQ; r = expression_concatenate; { EBinaryOperation ($loc, {l; operator= BGreaterOrEqual; r}) }
  | l = expression_compare; LT; r = expression_concatenate; { EBinaryOperation ($loc, {l; operator= BLessThan; r}) }
  | l = expression_compare; LEQ; r = expression_concatenate; { EBinaryOperation ($loc, {l; operator= BLessOrEqual; r}) }

let expression_concatenate :=
  | expression_addition
  | l = expression_concatenate; PLUSPLUS; r = expression_addition; { EBinaryOperation ($loc, {l; operator= BConcatenate; r}) }

let expression_addition :=
  | expression_multiplication
  | l = expression_addition; PLUS; r = expression_multiplication; { EBinaryOperation ($loc, {l; operator= BAdd; r}) }
  | l = expression_addition; MINUS; r = expression_multiplication; { EBinaryOperation ($loc, {l; operator= BSubstract; r}) }

let expression_multiplication :=
  | expression_exponentiation
  | l = expression_multiplication; STAR; r = expression_exponentiation; { EBinaryOperation ($loc, {l; operator= BMultiply; r}) }
  | l = expression_multiplication; SLASH; r = expression_exponentiation; { EBinaryOperation ($loc, {l; operator= BDivide; r}) }
  | l = expression_multiplication; PERCENT; r = expression_exponentiation; { EBinaryOperation ($loc, {l; operator= BModulo; r}) }

let expression_exponentiation :=
  | expression_unary
  | l = expression_unary; STARSTAR; r = expression_exponentiation; { EBinaryOperation ($loc, {l; operator= BExponentiate; r}) }

let expression_unary :=
  | application
  | PLUS; body = expression_unary; { EUnaryOperation ($loc, {operator= UPlus; body}) }
  | MINUS; body = expression_unary; { EUnaryOperation ($loc, {operator= UMinus; body}) }
  | BANG; body = expression_unary; { EUnaryOperation ($loc, {operator= UNot; body}) }

let variant == id = UID; typing = option(preceded(KWAS, typing)); SEMICOLON; { {span= $loc; id; typing} } 

let case == pattern = pattern; guard = option(preceded(KWIF, expression)); ARROW; body = expression; SEMICOLON; { {span= $loc; pattern; guard; body} }

let pattern_atom :=
  | i = INT; { PInt ($loc, i) }
  | f = FLOAT; { PFloat ($loc, f) }
  | b = BOOL; { PBool ($loc, b) }
  | s = STRING; { PString ($loc, s) }
  | id = LID; { PLID ($loc, id) }
  | LPAREN; p = pattern_atom; COMMA; ps = separated_list(COMMA, pattern_atom); RPAREN; { PTuple ($loc, p :: ps) }
  | LBRACKET; l = separated_list(COMMA, pattern_atom); RBRACKET; { PList ($loc, l) }
  | LBRACKET; ps = separated_nonempty_list(COMMA, pattern_atom); ELLIPSIS; p = LID; RBRACKET; { PListSpread ($loc, ps @ [PLID ($loc, p)]) }
  | id = UID; pattern = option(pattern_atom); { PConstructor ($loc, {id; pattern}) }

let pattern :=
  | pattern_atom
  | l = pattern; SEMICOLON; r = pattern_atom; { POr ($loc, {l; r}) }
